;;;; init.cl
(in-package #:pycl)

(define-condition start-python-error (pycl-condition error)
  ((reason :initarg :reason :initform "" :type simple-string)))

(defmethod report-pycl-condition ((err start-python-error) stream)
  (format stream "Python Initialization FAILED!~%~a" (slot-value err 'reason)))

(defconstant +minimum-python-version+ "3.7")

(defvar-nonbindable *python* nil
  "A python interpreter instance.")

(eval-when (:load-toplevel)
  (defparameter *find_libpython.py*
    (let ((path (string+ (directory-namestring *load-pathname*) "find_libpython.py")))
      (when (not (probe-file path))
        (error 'start-python-error :reason "Missing file \"find_libpython.py\""))
      path)))

(defun ensure-libpython-loaded (python-exe)
  "Try to load libpython shared object by executing find_libpython.py.
`python find_libpython.py --list-all` will return a list of candidates among
which we will try to load one by one. If anything is loaded without raising an
error, we will return the loaded libpython's pathname; otherwise, we raise an
error."
  (multiple-value-bind (candidates stderr exit)
      (excl.osi:command-output (string+ python-exe #\Space *find_libpython.py* #\Space "--list-all"))
    (when (not (zerop exit))
      (error 'start-python-error
             :reason (string+ "Cannot find libpython shared object by executing find_libpython.py:"
                              #\Newline
                              (apply 'string+ stderr))))
    (dolist (lib candidates)
      (when (ignore-errors (load lib :foreign t))
        (return-from ensure-libpython-loaded lib)))
    (error 'start-python-error
           :reason (string+ "Cannot load libpython shared object from these: " candidates))))

(defun find-python-program (python-exe)
  (let ((script "import sys; print(sys.executable)"))
    (multiple-value-bind (program stderr exit)
        (excl.osi:command-output (string+ python-exe #\Space "-c" #\Space #\" script #\"))
      (when (not (zerop exit))
        (error 'start-python-error
               :reason (format nil "Cannot get the python program name by running ~s:~%~a" script stderr)))
      (first program))))

(defun find-python-home (python-exe)
  (let ((script #+windows "
import sys
if hasattr(sys, 'base_exec_prefix'):
    sys.stdout.write(sys.base_exec_prefix)
else:
    sys.stdout.write(sys.exec_prefix)
"
                #-windows "
import sys
if hasattr(sys, 'base_exec_prefix'):
    sys.stdout.write(sys.base_prefix)
    sys.stdout.write(':')
    sys.stdout.write(sys.base_exec_prefix)
else:
    sys.stdout.write(sys.prefix)
    sys.stdout.write(':')
    sys.stdout.write(sys.exec_prefix)
"))
    (multiple-value-bind (home stderr exit)
        (excl.osi:command-output (string+ python-exe #\Space "-c" #\Space #\" script #\"))
      (when (not (zerop exit))
        (error 'start-python
               :reason (format nil "Cannot get the python home by running ~s:~%~a" script stderr)))
      (first home))))

(defun get-python-version (python-exe)
  (multiple-value-bind (version stderr exit)
      (excl.osi:command-output (string+ python-exe #\Space "--version"))
    (when (not (zerop exit))
      (error "Cannot get the python version by running \"~a --version\":~%~a" python-exe stderr))
    (let ((version (second (split-re " " (first version)))))
      (destructuring-bind (major minor &rest others)
          (split-re "\\." version)
        (declare (ignore others))
        (setq major (parse-integer major)
              minor (parse-integer minor))
        (cond ((= major 2) (error 'start-python-error :reason "Python 2 is not supported"))
              ((<= minor 6) (error 'start-python-error
                                   :reason (format nil "Minimum python version: ~s, but got ~s from ~s"
                                                   +minimum-python-version+ version python-exe)))
              (t version))))))

(defstruct python
  (libpython     "" :type simple-string)
  (exe           "" :type simple-string)
  (program       "" :type simple-string)
  (home          "" :type simple-string)
  (version       "" :type simple-string))

(defmethod print-object ((py python) stream)
  (with-slots (libpython exe program home version) py
    (print-unreadable-object (py stream :type t :identity t)
      (with-stack-list (lines (string+ "LIBPYTHON: "   libpython)
                              (string+ "  EXE:       " exe)
                              (string+ "  PROGRAM:   " program)
                              (string+ "  HOME:      " home)
                              (string+ "  VERSION:   " version))
        (format stream "~{~a~^~%~}~%" lines)))))

(defun init-python-program-name (program)
  (with-native-string (program* program)
    (let (w)
      (with-static-fobjects ((size* (* :unsigned-nat) :allocation :c))
        (setq w (Py_DecodeLocale program* size*))
        (when (zerop w)
          (error 'start-python-error :reason "In init-python-program-name, error raise from calling Py_DecodeLocale"))
        (Py_SetProgramName w)))))

(defvar-nonbindable *python-global-raw-pointers* nil
  "A list of symbols that can be used to reference valid (non-null) global
pointers processed from +libpython-extern-variables+. It is probably only useful
for diagnostic purposes.")

(defvar-nonbindable *global-raw-pointers-table* (make-hash-table :test 'eq)
  "Mapping between a symbol and the foreign address, e.g. 'PyExc_IOError -> #x0000ffff")

(defvar-nonbindable *global-raw-pointers-reverse-table* (make-hash-table :test '=)
  "Mapping between a foreign address and the corresponding name as a symbol, e.g. #x0000ffff -> 'PyExc_IOError")

(defun pyglobalptr (symbol-or-address)
  (let ((k symbol-or-address))
    (etypecase symbol-or-address
      (symbol (gethash k *global-raw-pointers-table*))
      ((unsigned-byte #+32bit 32 #+64bit 64) (gethash k *global-raw-pointers-reverse-table*)))))

(defun (setf pyglobalptr) (new-addr k)
  (check-type k symbol)
  (check-type new-addr (unsigned-byte #+32bit 32 #+64bit 64))
  (multiple-value-bind (old-addr exists-p)
      (gethash k *global-raw-pointers-table*)
    (when exists-p
      (warn "Redefining python global pointer '~a from ~s to ~s" k old-addr new-addr)
      (remhash old-addr *global-raw-pointers-reverse-table*)))
  (pushnew k *python-global-raw-pointers* :test 'eq)
  (setf (gethash k *global-raw-pointers-table*) new-addr
        (gethash new-addr *global-raw-pointers-reverse-table*) k))

(defun defglobalptr-helper (name &key pointer-p)
  (let ((address (get-entry-point name))) ; entry address
    (when pointer-p     ; deref if tagged with :pointer e.g. (:pointer PyObject)
      (setq address (fslot-value-typed :unsigned-nat :c address)))
    (check-type address (unsigned-byte #+32bit 32 #+64bit 64))
    (assert (/= 0 address))
    (funcall
     `(lambda ()
        (setf (pyglobalptr ',(intern name)) ,address)))))

(defun startup ()
  "This function runs after a python instance has been successfully initialized."
  ;; step1: initialize global pointers
  (dolist (spec +libpython-extern-variables+)
    (when (get-entry-point (first spec)) ; validate extern variable
      (apply 'defglobalptr-helper spec)))
  ;; step 2: initialize Py_None, Py_False, and Py_True
  ;; they need to be defined separately because they are aliased
  (setf (pyglobalptr 'Py_None)  (get-entry-point "_Py_NoneStruct"))
  (setf (pyglobalptr 'Py_False) (get-entry-point "_Py_FalseStruct"))
  (setf (pyglobalptr 'Py_True)  (get-entry-point "_Py_TrueStruct"))
  ;; end of startup
  t)

(defun %start-python (python-exe)
  (when *python*                        ; already initialized
    (error 'start-python-error :reason "*python* is non-nil. Is Python already started?"))
  (let ((version (get-python-version python-exe))
        (libpython (ensure-libpython-loaded python-exe))
        (program (find-python-program python-exe))
        (home (find-python-home python-exe)))
    (setf *python*                      ; Initialize *python*
          (make-python :libpython libpython
                       :exe python-exe
                       :program program
                       :home home
                       :version version))
    (handler-case
        (with-slots (home program) *python*
          (setf (sys:getenv "PYTHONIOENCODING") "utf-8"
                (sys:getenv "PYTHONHOME") home)
          (init-python-program-name program) ; Py_SetProgramName
          (Py_InitializeEx 0)                ; Call 'Py_InitializeEx
          (startup))
      (error (err)
        (write-line "Error occured during calling '%start-python. Clean up and aboring ..." *debug-io*)
        (pystop :unload-libpython t)
        (error 'start-python-error :reason (string+ err)))))
  *python*)

(defun pystart (&optional (python-exe (or (sys:getenv "PYCL_PYTHON_EXE")
                                               #+windows "python.exe"
                                               #-windows "python")))
  (check-type python-exe simple-string)
  (when (not (probe-file python-exe))
    (error 'start-python-error :reason (format nil "python-exe is not valid: ~s" python-exe)))
  (%start-python python-exe))

(defun pystop (&key (unload-libpython t))
  (when *python*
    (ignore-errors (Py_FinalizeEx))     ; TODO: handle error value -1?
    (when (and unload-libpython (python-libpython *python*))
      (unload-foreign-library (python-libpython *python*)))
    (setf *python-global-raw-pointers* nil)
    (clrhash *global-raw-pointers-table*)
    (clrhash *global-raw-pointers-reverse-table*)
    (setf *python* nil)
    (setf (sys:getenv "PYTHONIOENCODING") nil
          (sys:getenv "PYTHONHOME") nil)))
