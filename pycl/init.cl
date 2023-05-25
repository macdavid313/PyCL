;;;; init.cl
(in-package #:pycl)

(defvar-nonbindable *python* nil
  "A python interpreter instance.")

(defvar-nonbindable *pyglobalptr*
    (make-hash-table :test 'eq :size (length pycl.sys:+libpython-foreign-pointers+))
  "A hash table where key is the global pointer's name and the value is its foreign address")

(defun pyglobalptr (k)
  (multiple-value-bind (v exists-p)
      (gethash k *pyglobalptr*)
    (if* exists-p
       then v
       else (error "Global pointer ~s has not been initialized!"))))

(defconstant +minimum-python-version+ "3.7")

(eval-when (:load-toplevel)
  (defparameter *find_libpython.py*
    (let ((path (string+ (directory-namestring *load-pathname*) "find_libpython.py")))
      (when (not (probe-file path))
        (error "Missing file \"find_libpython.py\""))
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
      (error "Cannot find libpython shared object by executing find_libpython.py:~%~a" (apply 'string+ stderr)))
    (dolist (lib candidates)
      (when (ignore-errors (load lib :foreign t))
        (return-from ensure-libpython-loaded lib)))
    (error "Cannot load libpython shared object from these: ~a" candidates)))

(defun find-python-program (python-exe)
  (let ((script "import sys; print(sys.executable)"))
    (multiple-value-bind (program stderr exit)
        (excl.osi:command-output (string+ python-exe #\Space "-c" #\Space #\" script #\"))
      (when (not (zerop exit))
        (error "Cannot get the python program name by running ~s:~%~a" script stderr))
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
        (error "Cannot get the python home by running ~s:~%~a" script stderr))
      (first home))))

(defun get-python-version (python-exe)
  (multiple-value-bind (version stderr exit)
      (excl.osi:command-output (string+ python-exe #\Space "--version"))
    (when (not (zerop exit))
      (error "Cannot get the python version by running \"~a --version\":~%~a" python-exe stderr))
    (second (split-re " " (first version)))))

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
          (error "Error from calling Py_DecodeLocale on arg: ~a" program*))
        (Py_SetProgramName w)))))

(defun init-python-home (home)
  (with-native-string (home* home)
    (let (w)
      (with-static-fobjects ((size* (* :unsigned-nat) :allocation :c))
        (setq w (Py_DecodeLocale home* size*))
        (when (zerop w)
          (error "Error from calling Py_DecodeLocale on arg: ~a" home*))
        (Py_SetPythonHome w)))))

(defun startup ()
  "This function runs after a python instance has been successfully initialized.
It does:
  1. initialize basic types
  2. initialize exception and warning pointers"
  (setf (gethash 'Py_None  *pyglobalptr*) (get-entry-point "_Py_NoneStruct")
        (gethash 'Py_False *pyglobalptr*) (get-entry-point "_Py_FalseStruct")
        (gethash 'Py_True  *pyglobalptr*) (get-entry-point "_Py_TrueStruct"))
  (dolist (name '("Type" "Long" "Float" "Complex" "Bytes" "ByteArray" "Unicode"
                  "Tuple" "List" "Dict" "Set" "Function" "InstanceMethod" "Cell"
                  "Code" "Module" "SeqIter" "Property" "Slice" "Gen" "Coro"
                  "Context" "Context_Var" "ContextToken"))
    (setf (gethash (intern #1=(string+ "Py" name "_Type")) *pyglobalptr*)
          (get-entry-point #1#)))
  (dolist (name pycl.sys:+libpython-foreign-pointers+)
    (setf (gethash (intern name) *pyglobalptr*)
          (fslot-value-typed :unsigned-nat              ; type
                             :c                         ; allocation
                             (get-entry-point name))))) ; address

(defun start-python (&optional (python-exe #+windows "python.exe"
                                           #-windows "python"))
  (if* *python*
     then (error "*python* is non-nil: ~a" *python*)
     else (let ((version (get-python-version python-exe))
                libpython
                program
                home)
            (when (string< version +minimum-python-version+)
              (error "Minimum python version: ~s, but got ~s from ~s"
                     +minimum-python-version+ version python-exe))
            (setq libpython (ensure-libpython-loaded python-exe))
            (handler-case
                (progn (setq program (find-python-program python-exe)
                             home (find-python-home python-exe))
                       (setf (sys:getenv "PYTHONIOENCODING") "utf-8"
                             (sys:getenv "PYTHONHOME") nil)
                       (init-python-program-name program) ; Py_SetProgramName
                       (init-python-home home)            ; Py_SetPythonHome
                       (Py_InitializeEx 0) ; Call 'Py_InitializeEx
                       (when (not (Py_IsInitialized))
                         (error "Python initialization failed after calling 'Py_InitializeEx"))
                       (setf *python*   ; Initialize *python*
                             (make-python :libpython libpython
                                          :exe python-exe
                                          :program program
                                          :home home
                                          :version version))
                       *python*)
              (error () (unload-foreign-library libpython)))
            ;; run startup operations
            (startup)
            *python*)))

(defun shutdown-python ()
  (Py_FinalizeEx)
  (unload-foreign-library (python-libpython *python*))
  (setf *python* nil))
