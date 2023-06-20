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

(def-foreign-call PyGILState_Check (:void)
  :returning :int
  :arg-checking nil
  :call-direct t
  :allow-gc :always)

(def-foreign-call PyRun_SimpleString ((command :foreign-address))
  :returning :int
  :arg-checking nil
  :call-direct t
  :allow-gc :always)

(def-foreign-call PyUnicode_AsUTF8 ((o (* PyObject)))
  :returning :foreign-address
  :arg-checking nil
  :call-direct t
  :allow-gc :always)

;;; Conditions
(define-condition pycl-condition (condition)
  ())

(define-condition simple-pycl-error (pycl-condition simple-error)
  ((msg :initarg :msg :initform "" :type simple-string)))

(defmethod print-object ((err simple-pycl-error) stream)
  (print-unreadable-object (err stream :type t :identity t)
    (format stream "~%  ")
    (with-slots (msg) err
      (write-line msg stream))))

;;; Python and its GIL
(defvar-nonbindable *python* nil
  "The global python interpreter instance.")

(defstruct (python (:print-function print-python-struct))
  (exe            "" :type simple-string :read-only t)
  (version        "" :type simple-string :read-only t)
  (libpython      "" :type simple-string :read-only t)
  (home           "" :type simple-string :read-only t)
  ;; private slots start here ------------------------
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
    (etypecase symbol-or-address
      (symbol (gethash k (python-globalptr *python*)))
      ((unsigned-byte #+32bit 32 #+64bit 64) (gethash k (python-inv-globalptr *python*)))
      (foreign-pointer (gethash (foreign-pointer-address k) (python-inv-globalptr *python*))))))

(defun (setf pyglobalptr) (new-addr sym)
  (when (and (symbolp sym)
             (typep new-addr '(unsigned-byte #+32bit 32 #+64bit 64)))
    (multiple-value-bind (old-addr exists-p)
        (gethash sym (python-globalptr *python*))
      (when exists-p
        (warn "Redefining python global pointer '~a from ~s to ~s" sym old-addr new-addr)
        (remhash old-addr (python-inv-globalptr *python*))))
    (setf (gethash sym (python-globalptr *python*)) new-addr
          (gethash new-addr (python-inv-globalptr *python*)) sym)))

#-smp
(defmacro with-python-gil ((&key (unwind-protect t)) &body body)
  (declare (ignore unwind-protect))
  `(progn ,@body))

#+smp
(defmacro with-python-gil ((&key (unwind-protect t)) &body body)
  (let ((g (gensym "g"))
        (res (gensym "res")))
    (if* unwind-protect
       then `(let ((,g (PyGILState_Ensure))
                   ,res)
               (declare (type (mod 1) ,g))
               (setq ,res (progn ,@body))
               (PyGILState_Release ,g)
               ,res)
       else `(let ((,g (PyGILState_Ensure)))
               (unwind-protect (progn ,@body)
                 (PyGILState_Release ,g))))))

;;; pyobject
;;; APIs and Utilities
;;; -----------------------------------------------------------------------------
;;; Almost all Python objects live on the heap: you never declare an automatic
;;; or static variable of type PyObject, only pointer variables of type
;;; PyObject* can be declared. The sole exception are the type objects; since
;;; these must never be deallocated, they are typically static PyTypeObject
;;; objects. see also
;;; https://docs.python.org/3/c-api/intro.html#objects-types-and-reference-counts
;;; -----------------------------------------------------------------------------
(defclass pyobject (foreign-pointer)
  ((ptype :initform nil :accessor pyobject-type :type (or symbol (unsigned-byte #+32bit 32 #+64bit 64)))
   (finalization :accessor pyobject-finalization :type (or null excl::finalization)))
  (:documentation "Foreign pointer type for PyObject."))

(defvar-nonbindable *pynull*
    (make-instance 'pyobject :foreign-type 'PyObject
                             :foreign-address 0)
  "A singleton that represents a NULL foreign pointer of PyObject")

(defun make-pyobject (addr &optional (lifetime :new))
  (declare (type (unsigned-byte #+32bit 32 #+64bit 64) addr)
           (type (member :default :new :borrowed) lifetime)
           (optimize (speed 3) (safety 0) (space 0)))
  (if* (= 0 addr)
     then *pynull*
     else (let ((ob (make-instance 'pyobject :foreign-type 'PyObject
                                             :foreign-address addr)))
            (when (member lifetime '(:default :new) :test 'eq)
              (schedule-pyobject-finalization ob))
            (setf (pyobject-type ob)
                  (pyglobalptr (PyObject_Type addr)))
            ob)))

(defmethod print-object ((fp pyobject) stream)
  (if* (= 0 (foreign-pointer-address fp))
     then (write-sequence "#<PyObject NULL>" stream)
     else (let ((*print-base* 16))
            (if* (pyobject-type fp)
               then (format stream "#<PyObject (~a) @ #x~a>"
                                   (pyobject-type fp)
                                   (foreign-pointer-address fp))
               else (format stream "#<PyObject @ #x~a>"
                                   (foreign-pointer-address fp))))))

(defun pyobject-p (thing)
  (typep thing 'pyobject))

(defmethod pyobject-eq ((x pyobject) (y pyobject))
  (= (foreign-pointer-address x)
     (foreign-pointer-address y)))

(defmethod pynull ((ob pyobject))
  (or (eq ob *pynull*)
      (= 0 (foreign-pointer-address ob))))

(defmethod pyincref ((ob pyobject))
  (Py_IncRef ob)
  ob)

(defmethod pydecref ((ob pyobject))
  (when (not (pynull ob))
    (unschedule-pyobject-finalization ob)
    (Py_DecRef ob)
    (setf (foreign-pointer-address ob) 0)))

(defun pydecref* (&rest obs)
  (dolist (ob obs)
    (pydecref ob)))

(defmethod pystealref ((ob pyobject))
  "The caller (thief) will take the ownership so you are NOT responsible anymore.
This macro should always be used \"in place\" e.g. (PyList_SetItem ob_list idx (pystealref ob_item))"
  (if* (pynull ob)
     then *pynull*
     else (let ((address (foreign-pointer-address ob)))
            (unschedule-pyobject-finalization ob)
            (setf (foreign-pointer-address ob) 0)
            address)))

(defmethod schedule-pyobject-finalization ((ob pyobject))
  (setf (pyobject-finalization ob)
        (schedule-finalization ob 'pydecref))
  ob)

(defmethod unschedule-pyobject-finalization ((ob pyobject))
  (when (pyobject-finalization ob)
    (unschedule-finalization (pyobject-finalization ob))
    (setf (pyobject-finalization ob) nil))
  ob)

;;; python exception
(define-condition python-exception (simple-pycl-error)
  ((place :initarg :place :initform nil :accessor python-exception-place :type (or null symbol))
   (type :initarg :type :initform nil :accessor python-exception-type :type (or null symbol))
   (msg :initarg :msg :initform "" :accessor python-exception-msg :type simple-string)
   (context :initarg :context :initform nil :accessor python-exception-context :type list))
  (:documentation "A simple condition that represents a python exception. User is responsible to
construct the \"msg\"."))

(defmethod print-object ((exc python-exception) stream)
  (print-unreadable-object (exc stream :type t :identity t)
    (with-slots (place msg) exc
      (if* place
         then (format stream "caught python exception during calling ~a: ~%~a" place msg)
         else (format stream "- python exception caught: ~%~a" msg)))))

(define-symbol-macro python-exception-occurred
    (/= 0 (PyErr_Occurred)))

(defun format-python-exception (ob_type       ; stolen
                                ob_value      ; sotlen
                                ob_traceback) ; stolen
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (let* ((ob_module                     ; new
           (with-native-string (str "traceback" :external-format :utf-8)
             (PyImport_ImportModule str)))
         (ob_formatter                  ; new
           (if* (= 0 ob_traceback)
              then (with-native-string (str "format_exception_only" :external-format :utf-8)
                     (PyObject_GetAttrString ob_module str))
              else (with-native-string (str "format_exception" :external-format :utf-8)
                     (PyObject_GetAttrString ob_module str))))
         (ob_tuple (PyTuple_New (if (= 0 ob_traceback) 2 3)))) ; new
    (declare (type #1=(unsigned-byte #+32bit 32 #+64bit 64) ob_module ob_formatter ob_tuple))
    (PyTuple_SetItem ob_tuple 0 ob_type)
    (PyTuple_SetItem ob_tuple 1 ob_value)
    (when (/= 0 ob_traceback)
      (PyTuple_SetItem ob_tuple 2 ob_traceback))
    (with-output-to-string (out)
      (loop with ob_list of-type #1# = (PyObject_CallObject ob_formatter ob_tuple) ; new
            for idx of-type fixnum from 0 below (PyList_Size ob_list)
            for ob_unicode of-type #1# = (PyList_GetItem ob_list idx) ; borrowed
            for line = (native-to-string (PyUnicode_AsUTF8 ob_unicode) :external-format :utf-8)
            do (write-string (string+ #\Space #\Space line) out)
            finally (with-stack-list (obs ob_module ob_formatter ob_tuple ob_list)
                      (dolist (ob obs)
                        (Py_DecRef ob)))))))

(defun pyerror (&optional place)
  (if* python-exception-occurred
     then (with-static-fobjects ((ob_type* #1='(* PyObject) :allocation :c)
                                 (ob_value* #1# :allocation :c)
                                 (ob_traceback* #1# :allocation :c))
            (PyErr_Fetch ob_type* ob_value* ob_traceback*)
            (PyErr_NormalizeException ob_type* ob_value* ob_traceback*)
            (let ((ob_type (fslot-value-typed #1# :c ob_type*))
                  (ob_value (fslot-value-typed #1# :c ob_value*))
                  (ob_traceback (fslot-value-typed #1# :c ob_traceback*))
                  exception)
              (Py_IncRef ob_type)
              (Py_IncRef ob_value)
              (Py_IncRef ob_traceback)
              (setq exception (make-instance 'python-exception
                                             :place place
                                             :type (pyglobalptr ob_type)
                                             :msg (format-python-exception ob_type ob_value ob_traceback)
                                             :context (list (make-pyobject ob_type)
                                                            (make-pyobject ob_value)
                                                            (make-pyobject ob_traceback))))
              (PyErr_Clear)
              (error exception)))
     else (error 'simple-pycl-error
                 :msg "trying to catch a python exception but none has occurred")))

(defmacro pycheckn (form &optional (place nil place-p))
  (when (and (not place-p)
             (symbolp (car form)))
    (setq place (car form)))
  (check-type place symbol)
  (let ((val (gensym "val")))
    `(let ((,val ,form))
       (etypecase ,val
         (pyobject
          (if (pynull ,val) (pyerror ',place) ,val))
         ((unsigned-byte #+32bit 32 #+64bit 64)
          (if (= 0 ,val) (pyerror ',place) ,val))))))

(defmacro pycheckz (form &optional (place nil place-p))
  (when (and (not place-p)
             (symbolp (car form)))
    (setq place (car form)))
  (check-type place symbol)
  (let ((val (gensym "val")))
    `(let ((,val ,form))
       (declare (type integer ,val))
       (if* (= ,val -1)
          then (pyerror ',place)
          else ,val))))
