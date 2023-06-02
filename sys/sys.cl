;;;; sys.cl
(in-package :pycl.sys)

;;; "lost and found"
;;; these Python C APIS are not generated because they are not
;;; considered to be "stable"
(def-foreign-call PyObject_DelAttrString ((o (* PyObject)) (attr :foreign-address))
  :returning :int
  :arg-checking nil
  :call-direct t
  :allow-gc :always)

(def-foreign-call (check-python-gil "PyGILState_Check") (:void)
  :returning :boolean
  :arg-checking nil
  :call-direct t
  :allow-gc :always)

(def-foreign-call PyRun_SimpleString ((command :foreign-address))
  :returning :int
  :arg-checking nil
  :call-direct t
  :allow-gc :always)

(def-foreign-call (from-pyunicode! "PyUnicode_AsUTF8") ((o (* PyObject)))
  :returning ((* :char) simple-string)
  :arg-checking t
  :strings-convert t
  :allow-gc :always)

;;; Conditions
(define-condition pycl-condition (condition)
  ()
  (:report report-pycl-condition))

(defgeneric report-pycl-condition (condition stream)
  (:documentation "Report pycl and python related conditions to stream."))

(define-condition simple-pycl-error (pycl-condition simple-error)
  ((msg :initarg :msg :initform "" :type simple-string)))

(defmethod report-pycl-condition ((err simple-pycl-error) stream)
  (with-slots (msg) err
    (write-line msg stream)))

;;; Utilities
(defun pyimport! (module)
  (declare (type simple-string module))
  (with-native-string (str module :external-format :utf-8)
    (the pyobject (PyImport_ImportModule str))))

(defun pyhasattr! (ob attr)
  (declare (type pyobject ob)
           (type simple-string attr))
  (with-native-string (str attr :external-format :utf-8)
    (= 1 (PyObject_HasAttrString ob str))))

(defun pyattr! (ob attr)
  (declare (type pyobject ob)
           (type simple-string attr))
  (with-native-string (str attr :external-format :utf-8)
    (values (PyObject_GetAttrString ob str)
            (= 1 (PyObject_HasAttrString ob str)))))

(defun (setf pyattr!) (new ob attr)
  (declare (type pyobject ob new)
           (type simple-string attr))
  (with-native-string (str attr :external-format :utf-8)
    (if* (and (pynull new) (pyhasattr! ob attr))
       then (PyObject_DelAttrString ob str)
       else (PyObject_SetAttrString ob str new))))

;;; Python and its GIL
(defvar-nonbindable *python* nil
  "The default python interpreter instance.")

(defstruct (python (:print-function print-python-struct))
  (exe            "" :type simple-string :read-only t)
  (version        "" :type simple-string :read-only t)
  (libpython      "" :type simple-string :read-only t)
  (home           "" :type simple-string :read-only t)
  ;; private slots start here ------------------------
  ;; process lock, only available in SMP
  #+smp (lock (mp:make-process-lock :name "python-process-lock")
         :type mp:process-lock :read-only t)
  ;; Mapping between a symbol and the foreign address, e.g. 'PyExc_IOError ->
  ;; #x0000ffff
  (globalptr (make-hash-table :test 'eq :size #.(length +libpython-extern-variables+))
   :type hash-table :read-only t)
  ;; Mapping between a foreign address and the corresponding name as a symbol,
  ;; e.g. #x0000ffff -> 'PyExc_IOError
  (inv-globalptr (make-hash-table :test '= :size #.(length +libpython-extern-variables+))
   :type hash-table :read-only t))

(defun print-python-struct (py stream depth)
  (declare (ignore depth))
  (with-slots (exe version libpython home) py
    (print-unreadable-object (py stream :type t :identity t)
      (terpri stream)
      (with-stack-list (lines (string+ "  exe: "       #\" exe #\" )
                              (string+ "  version: "   #\"  version #\")
                              (string+ "  libpython: " #\" libpython #\")
                              (string+ "  home: "      #\" home #\"))
        (format stream "~{~a~^~%~}~%" lines)))))

(defun pyglobalptr (symbol-or-address)
  (let ((k symbol-or-address))
    (multiple-value-bind (val exists-p)
        (etypecase symbol-or-address
          (symbol (gethash k (python-globalptr *python*)))
          (foreign-pointer (gethash (foreign-pointer-address k) (python-inv-globalptr *python*)))
          ((unsigned-byte #+32bit 32 #+64bit 64) (gethash k (python-inv-globalptr *python*))))
      (when exists-p
        (if (symbolp val) val (make-pyobject val))))))

(defun (setf pyglobalptr) (new-addr sym)
  (check-type sym symbol)
  (check-type new-addr foreign-address)
  (when (foreign-pointer-p new-addr) (setq new-addr (foreign-pointer-address new-addr)))
  (multiple-value-bind (old-addr exists-p)
      (gethash sym (python-globalptr *python*))
    (when exists-p
      (warn "Redefining python global pointer '~a from ~s to ~s" sym old-addr new-addr)
      (remhash old-addr (python-inv-globalptr *python*))))
  (setf (gethash sym (python-globalptr *python*)) new-addr
        (gethash new-addr (python-inv-globalptr *python*)) sym))

#-smp
(defmacro with-python-gil ((&key safe) &body body)
  (declare (ignore safe))
  `(progn ,@body))

#+smp
(defmacro with-python-gil ((&key (unwind-protect t)) &body body)
  (let ((g (gensym "g"))
        (res (gensym "res")))
    (if* unwind-protect
       then `(mp:with-process-lock ((python-lock *python*))
               (let ((,g (PyGILState_Ensure))
                     ,res)
                 (declare (type (mod 1) ,g))
                 (setq ,res (progn ,@body))
                 (PyGILState_Release ,g)
                 ,res))
       else `(mp:with-process-lock ((python-lock *python*))
               (let ((,g (PyGILState_Ensure)))
                 (unwind-protect (progn ,@body)
                   (PyGILState_Release ,g)))))))

;;; pyobject
;;; APIs and Utilities
(defun pyptr-eq (x y)
  (and (typep x 'foreign-python-pointer)
       (typep y 'foreign-python-pointer)
       (eq (foreign-pointer-type x) (foreign-pointer-type y))
       (= (foreign-pointer-address x)
          (foreign-pointer-address y))))

(defun pynull (thing)
  (or (eq thing *pynull*)
      (and (typep thing 'pyptr)
           (= 0 (foreign-pointer-address thing)))))

(defun pyincref (ob)
  (declare (type pyobject ob))
  (Py_IncRef ob)
  ob)

(defun pydecref (ob)
  (declare (type pyobject ob))
  (Py_DecRef ob)
  (setf (foreign-pointer-address ob) 0)
  ob)

(defun pydecref* (&rest obs)
  (dolist (ob obs nil)
    (declare (type pyobject ob))
    (Py_DecRef ob)
    (setf (foreign-pointer-address ob) 0)))

(defun pystealref (ob)
  "The caller (thief) will take the ownership so you are NOT responsible anymore.
This macro should always be used \"in place\" e.g. (PyList_SetItem ob_list idx (pystealref ob_item))"
  (declare (type pyobject ob))
  (if* (pynull ob)
     then *pynull*
     else (prog1 (foreign-pointer-address ob)
            (setf (foreign-pointer-address ob) 0))))

;;; python exception
(define-condition python-exception (simple-pycl-error)
  ((type :initarg :type :initform nil :accessor python-exception-type :type (or null symbol))
   (msg :initarg :msg :initform "" :accessor python-exception-msg :type simple-string))
  (:documentation "A simple condition that represents a python exception. User is responsible to
construct the \"msg\"."))

(defmethod report-pycl-condition ((exc python-exception) stream)
  (print-unreadable-object (exc stream :type t :identity t)
    (with-slots (msg) exc
      (format stream "- python exception caught: ~%~a" msg))))

(define-symbol-macro python-exception-occurred
    (not (pynull (PyErr_Occurred))))

(defun make-python-exception ()
  (if* python-exception-occurred
     then (with-static-fobjects ((ob_type* #1='(* PyObject) :allocation :c)
                                 (ob_value* #1# :allocation :c)
                                 (ob_traceback* #1# :allocation :c))
            (PyErr_Fetch ob_type* ob_value* ob_traceback*)
            (PyErr_NormalizeException ob_type* ob_value* ob_traceback*)
            (let* ((ob_type (make-pyobject (fslot-value-typed #1# :c ob_type*)))
                   (ob_value (make-pyobject (fslot-value-typed #1# :c ob_value*)))
                   (ob_traceback (make-pyobject (fslot-value-typed #1# :c ob_traceback*)))
                   (exc (make-instance 'python-exception
                                       :type nil ; (pyglobalptr ob_type)
                                       :msg (format-python-exception ob_type ob_value ob_traceback))))
              (PyErr_Clear)
              exc))
          else (make-instance 'python-exception :msg "unknwon python exception")))

(defun format-python-exception (ob_type       ; stolen
                                ob_value      ; sotlen
                                ob_traceback) ; stolen
  (flet ((format-exception (ob)
           (with-output-to-string (out)
             (dotimes (idx (PyList_Size ob))
               (let (ob_unicode         ; borrowed
                     line)
                 (setq ob_unicode (PyList_GetItem ob idx))
                 (setq line (from-pyunicode! ob_unicode))
                 (write-string (string+ #\Space #\Space line)
                               out))))))
    (let* ((ob_module (pyimport! "traceback")) ; new
           (ob_formatter                       ; new
             (if* (pynull ob_traceback)
                then (pyattr! ob_module "format_exception_only")
                else (pyattr! ob_module "format_exception")))
           (ob_tuple (PyTuple_New (if (pynull ob_traceback) 2 3))) ; new
           ob_list)                                                ; new
      (if* (pynull ob_tuple)
         then (prog1 "None"
                (pydecref* ob_module ob_formatter ob_tuple))
         else (PyTuple_SetItem ob_tuple 0 (pystealref ob_type))
              (PyTuple_SetItem ob_tuple 1 (pystealref ob_value))
              (when (not (pynull ob_traceback))
                (PyTuple_SetItem ob_tuple 2 (pystealref ob_traceback)))
              (setq ob_list (PyObject_CallObject ob_formatter ob_tuple))
              (prog1 (format-exception ob_list)
                (pydecref* ob_module ob_formatter ob_tuple ob_list))))))

(defun pyexcept ()
  (make-python-exception))

(defun pyerror ()
  (error (pyexcept)))

(defun pycheckn (val)
  (when (pynull val)
    (pyerror))
  val)

(defun pycheckz (val)
  (when (minusp val)
    (pyerror))
  val)
