;;;; init.cl
(in-package #:pycl)

(defvar-nonbindable *python* nil
  "A python interpreter instance.")

(defconstant +minimum-python-version+ "3.6")

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
  (lib     "" :type simple-string)
  (exe     "" :type simple-string)
  (program "" :type simple-string)
  (home    "" :type simple-string)
  (version "" :type simple-string))

(defmethod print-object ((py python) stream)
  (with-slots (lib exe program home version) py
    (print-unreadable-object (py stream :type t :identity nil)
      (with-stack-list (metadata (cons 'lib lib)
                                 (cons 'exe exe)
                                 (cons 'program program)
                                 (cons 'home home)
                                 (cons 'version version))
        (pprint metadata stream)))))

(defmethod initialize-instance :after ((py python) &rest args)
  (declare (ignore args))
  (flet ((init-global-pointers ()
           "Generate all the top-level bindings."
           `(lambda ()
              "A hacky way to initialize libpython foreign pointers by a list of their names."
              (progn ,@(loop for var in pycl.sys:+libpython-foreign-pointers+
                             collect
                             #+(version>= 11 0)
                             `(def-foreign-constant ,(intern (string+ var "Ptr")) :type :unsigned-nat)
                             #-(version>= 11 0)
                             `(def-foreign-variable ,(intern (string+ var "Ptr")) :type :unsigned-natural))))))
    (funcall (init-global-pointers))))

(defun init-py-program-name (program)
  (with-native-string (program* program)
    (let (w)
      (with-static-fobjects ((size* (* :unsigned-nat) :allocation :c))
        (setq w (Py_DecodeLocale program* size*))
        (when (zerop w)
          (error "Error from calling Py_DecodeLocale on arg: ~a" program*))
        (Py_SetProgramName w)))))

(defun init-py-home (home)
  (with-native-string (home* home)
    (let (w)
      (with-static-fobjects ((size* (* :unsigned-nat) :allocation :c))
        (setq w (Py_DecodeLocale home* size*))
        (when (zerop w)
          (error "Error from calling Py_DecodeLocale on arg: ~a" home*))
        (Py_SetPythonHome w)))))

(defun start-python (&optional (python-exe #+windows "python.exe"
                                           #-windows "python"))
  (if* *python*
     then (error "*python* is non-nil: ~a" *python*)
     else (let ((version (get-python-version python-exe))
                lib
                program
                home)
            (when (string< version +minimum-python-version+)
              (error "Minimum python version: ~s, but got ~s from ~s"
                     +minimum-python-version+ version python-exe))
            (setq lib (ensure-libpython-loaded python-exe))
            (handler-case
                (progn (setq program (find-python-program python-exe)
                             home (find-python-home python-exe))
                       (setf (sys:getenv "PYTHONIOENCODING") "UTF-8"
                             (sys:getenv "PYTHONHOME") nil)
                       (init-py-program-name program) ; Py_SetProgramName
                       (init-py-home home)            ; Py_SetPythonHome
                       (Py_InitializeEx 0)            ; Call 'Py_InitializeEx
                       (when (not (Py_IsInitialized))
                         (error "Python initialization failed after calling 'Py_InitializeEx"))
                       (setf *python*   ; Initialize *python*
                             (make-python :lib lib
                                          :exe python-exe
                                          :program program
                                          :home home
                                          :version version))
                       *python*)
              (error () (unload-foreign-library lib))))))

(defun shutdown-python ()
  (Py_FinalizeEx)
  (unload-foreign-library (python-lib *python*))
  (setf *python* nil))
