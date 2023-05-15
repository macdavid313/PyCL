;;;; configuration.cl
(in-package #:pycl)

;; (def-foreign-call Py_SetProgramName ((name :foreign-address))
;;   :returning :void
;;   :arg-checking nil
;;   :call-direct t
;;   :strings-convert nil)

;; (def-foreign-call Py_SetPythonHome ((home :foreign-address))
;;   :returning :void
;;   :arg-checking nil
;;   :call-direct t
;;   :strings-convert nil)

;; (def-foreign-call Py_GetVersion ()
;;   :returning ((* :char) simple-string)
;;   :strings-convert t)

;; (def-foreign-call Py_InitializeEx ((initsigs :int))
;;   :returning :void
;;   :call-direct t
;;   :arg-checking nil
;;   :strings-convert nil)

;; (def-foreign-call Py_IsInitialized ()
;;   :returning :boolean
;;   :arg-checking nil
;;   :strings-convert nil)

;; (def-foreign-call Py_FinalizeEx ()
;;   :returning :int
;;   :strings-convert nil
;;   :arg-checking nil)

(eval-when (:load-toplevel)
  (defparameter *find_libpython.py*
    (string+ (directory-namestring *load-pathname*) "find_libpython.py")))

(defun try-load-libpython (python-exe)
  (multiple-value-bind (candidates stderr exit)
      (excl.osi:command-output (string+ python-exe #\Space *find_libpython.py* #\Space "--list-all"))
    (when (not (zerop exit))
      (error "Cannot find libpython shared object by executing find_libpython.py:~%~a" (apply 'string+ stderr)))
    (dolist (lib candidates)
      (when (ignore-errors (load lib :foreign t))
        (return-from try-load-libpython lib)))
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

(defstruct pycontext
  lib
  exe
  program
  home
  version
  initialized-p
  finalized-p)

(defmethod print-object ((ctx pycontext) stream)
  (with-slots (initialized-p finalized-p) ctx
    (print-unreadable-object (ctx stream :type t :identity t)
      (format stream
              (if* initialized-p
                 then "INITIALIZED"
               elseif finalized-p
                 then "FINALIZED"
                 else "ERROR")))))

(defun pycontext-to-alist (ctx)
  (declare (type pycontext ctx))
  (with-slots (lib exe program home version initialized-p finalized-p) ctx
    (list (cons 'lib lib)
          (cons 'exe exe)
          (cons 'program program)
          (cons 'home home)
          (cons 'version version)
          (cons 'initialized-p initialized-p)
          (cons 'finalized-p finalized-p))))

(defun startup ()
  (labels ((gen ()
             `(lambda ()
                (progn ,@(loop for var in +libpython-foreign-pointers+
                               collect
                               #+(version>= 11 0)
                               `(def-foreign-constant ,(intern var) :type :unsigned-nat)
                               #-(version>= 11 0)
                               `(def-foreign-variable ,(intern var) :type :unsigned-natural))))))
    (funcall (gen))
    t))

(defun init-pycontext (&optional (python-exe (or (sys:getenv "PYCL_PYTHON_EXE") #+windows "python.exe"
                                                                                #-windows "python")))
  (let ((lib (try-load-libpython python-exe))
        (program (find-python-program python-exe))
        (home (find-python-home python-exe))
        (version (get-python-version python-exe)))
    (setf (sys:getenv "PYTHONIOENCODING") "UTF-8"
          (sys:getenv "PYTHONHOME") nil)
    ;; FIXME: need to find a way to conver to wchar_t* properly
    ;; (with-native-strings* ((program* program :external-format :16-bit)
    ;;                        (home* home :external-format :16-bit))
    ;;   (Py_SetProgramName program*)
    ;;   (Py_SetPythonHome home*))
    (Py_InitializeEx 0)
    (when (not (Py_IsInitialized))
      (error "pycontext initialization failed after calling 'Py_InitializeEx"))
    (startup)
    (make-pycontext :lib lib
                    :exe python-exe
                    :program program
                    :home home
                    :version version
                    :initialized-p t
                    :finalized-p nil)))

(defun finalize-pycontext (ctx)
  (declare (type pycontext ctx))
  (Py_FinalizeEx)
  (setf (pycontext-initialized-p ctx) nil
        (pycontext-finalized-p ctx) t)
  ctx)
