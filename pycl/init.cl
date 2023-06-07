;;;; init.cl
(in-package #:pycl)

(defconstant +minimum-python-version+ "3.7")

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
        (cond ((= major 2) (error "Python 2 is not supported"))
              ((<= minor 6) (error "Minimum python version: ~s, but got ~s from ~s"
                                   +minimum-python-version+ version python-exe))
              (t version))))))

(eval-when (:load-toplevel :execute)
  (defconstant +find_libpython.py+
    (let ((path (string+ (directory-namestring (or *compile-file-pathname* *load-pathname*)) "find_libpython.py")))
      (when (not (probe-file path))
        (error "Missing file \"find_libpython.py\""))
      path))

  (defun ensure-libpython-loaded (python-exe)
    "Try to load libpython shared object by executing find_libpython.py.
`python find_libpython.py --list-all` will return a list of candidates among
which we will try to load one by one. If anything is loaded without raising an
error, we will return the loaded libpython's pathname; otherwise, we raise an
error."
    (multiple-value-bind (candidates stderr exit)
        (excl.osi:command-output (string+ python-exe #\Space +find_libpython.py+ #\Space "--list-all"))
      (when (not (zerop exit))
        (error "Cannot find libpython shared object by executing find_libpython.py:~%~a"
               (apply 'string+ stderr)))
      (dolist (lib candidates)
        (when (ignore-errors (load lib :foreign t))
          (return-from ensure-libpython-loaded lib)))
      (error "Cannot load libpython shared object from these: ~a" candidates))))

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
        (error "Cannot get the python home by running ~s:~%~a" script (apply 'string+ stderr)))
      (first home))))

(defun defglobalptr-helper (name &key pointer-p)
  (let ((address (get-entry-point name))) ; entry address
    (when pointer-p     ; deref if tagged with :pointer e.g. (:pointer PyObject)
      (setq address (fslot-value-typed :unsigned-nat :c address)))
    (check-type address (unsigned-byte #+32bit 32 #+64bit 64))
    (assert (/= 0 address))
    (funcall
     `(lambda ()
        (setf (pyglobalptr ',(intern name :pycl)) ,address)))))

(defun startup ()
  "This function runs after a python instance has been successfully initialized."
  ;; step1: initialize global pointers
  (dolist (spec +libpython-extern-variables+)
    (when (get-entry-point (first spec)) ; validate extern variable
      (apply 'defglobalptr-helper spec)))
  ;; step 2: initialize Py_None, Py_False, and Py_True
  ;; they need to be defined separately because they are aliased
  (setf (pyglobalptr 'Py_None)           (get-entry-point "_Py_NoneStruct"))
  (setf (pyglobalptr 'Py_False)          (get-entry-point "_Py_FalseStruct"))
  (setf (pyglobalptr 'Py_True)           (get-entry-point "_Py_TrueStruct"))
  (setf (pyglobalptr 'Py_NotImplemented) (get-entry-point "_Py_NotImplementedStruct"))
  ;; end of startup
  t)

(defun %start-python (python-exe)
  (when (python-p *python*)             ; already initialized
    (error "*python* is non-nil. Is Python already started?"))
  (let ((version (get-python-version python-exe))
        (libpython (ensure-libpython-loaded python-exe))
        (home (find-python-home python-exe)))
    (setf (sys:getenv "PYTHONIOENCODING") "utf-8"
          (sys:getenv "PYTHONHOME") home)
    (Py_InitializeEx 0)                 ; Call 'Py_InitializeEx
    (setf *python*                      ; Initialize *python*
          (make-python :exe       python-exe
                       :version   version
                       :libpython libpython
                       :home      home))
    (handler-case (startup)
      (error (err)
        (write-line "Error occured during calling '%start-python. Clean up and aboring ..." *debug-io*)
        (pystop :unload-libpython t)
        (error err))))
  *python*)

(defun pystart (&optional (python-exe (or (sys:getenv "PYCL_PYTHON_EXE")
                                          #+windows "python.exe"
                                          #-windows "python")))
  (check-type python-exe simple-string)
  (when (not (probe-file python-exe))
    (error "python executable path is not valid: ~s" python-exe))
  (%start-python python-exe))

(defun pystop (&key (unload-libpython t))
  (when (python-p *python*)
    (when (= -1 (Py_FinalizeEx))
      (warn "(Py_FInalizedEx) returned -1"))
    (setf (sys:getenv "PYTHONIOENCODING") "utf-8"
          (sys:getenv "PYTHONHOME") nil)
    (when (and unload-libpython (python-libpython *python*))
      (format *debug-io* "Unloading libpython: ~s" (python-libpython *python*))
      (unload-foreign-library (python-libpython *python*)))
    (setf *python* nil)))
