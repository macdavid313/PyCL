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
    (typecase symbol-or-address
      (symbol (gethash k (python-globalptr *python*)))
      ((unsigned-byte #+32bit 32 #+64bit 64) (gethash k (python-inv-globalptr *python*)))
      (foreign-pointer (gethash (foreign-pointer-address k) (python-inv-globalptr *python*)))
      (t nil))))

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
(defun pyobject-eq (x y)
  (and (typep x 'pyobject)
       (typep y 'pyobject)
       (= (foreign-pointer-address x)
          (foreign-pointer-address y))))

(defun pynull (ob)
  (or (eq ob *pynull*)
      (and (typep ob 'pyobject)
           (= 0 (foreign-pointer-address ob)))))

(defun pyincref (ob)
  (when (typep ob 'pyobject)
    (Py_IncRef ob))
  ob)

(defun %pydecref (ob)
  (declare (type pyobject ob)
           (optimize (speed 3) (safety 0) (space 0)))
  (when (typep ob 'pyobject)
    (unschedule-finalization (pyobject-finalization ob))
    (Py_DecRef ob)
    (setf (foreign-pointer-address ob) 0
          (pyobject-finalization ob) nil))
  nil)

(defun pydecref (ob)
  (if* (pynull ob)
     then nil
     else (%pydecref ob)))

(defun pydecref* (&rest obs)
  (dolist (ob obs)
    (pydecref ob)))

(defun pystealref (ob)
  "The caller (thief) will take the ownership so you are NOT responsible anymore.
This macro should always be used \"in place\" e.g. (PyList_SetItem ob_list idx (pystealref ob_item))"
  (if* (pynull ob)
     then *pynull*
     else (when (typep ob 'pyobject)
            (let ((address (foreign-pointer-address ob)))
              (unschedule-finalization (pyobject-finalization ob))
              (setf (foreign-pointer-address ob) 0
                    (pyobject-finalization ob) nil)
              address))))

;;; python exception
(define-condition python-exception (simple-pycl-error)
  ((type :initarg :type :initform nil :accessor python-exception-type :type (or null symbol))
   (msg :initarg :msg :initform "" :accessor python-exception-msg :type simple-string))
  (:documentation "A simple condition that represents a python exception. User is responsible to
construct the \"msg\"."))

(defmethod print-object ((exc python-exception) stream)
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
            (let ((ob_type (fslot-value-typed #1# :c ob_type*))
                  (ob_value (fslot-value-typed #1# :c ob_value*))
                  (ob_traceback (fslot-value-typed #1# :c ob_traceback*)))
              (prog1 (make-instance 'python-exception
                                    :type (pyglobalptr ob_type)
                                    :msg (format-python-exception ob_type ob_value ob_traceback))
                (PyErr_Clear))))
     else (make-instance 'simple-pycl-error
                          :msg "trying to catch a python exception but none has occurred")))

(defun format-python-exception (ob_type       ; stolen
                                ob_value      ; sotlen
                                ob_traceback) ; stolen
  (let* ((ob_module                           ; new
           (with-native-string (str "traceback" :external-format :utf-8)
             (PyImport_ImportModule str)))
         (ob_formatter                  ; new
           (if* (= 0 ob_traceback)
              then (with-native-string (str "format_exception_only" :external-format :utf-8)
                     (PyObject_GetAttrString ob_module str))
              else (with-native-string (str "format_exception" :external-format :utf-8)
                     (PyObject_GetAttrString ob_module str))))
         (ob_tuple (PyTuple_New (if (= 0 ob_traceback) 2 3)))) ; new
    (PyTuple_SetItem ob_tuple 0 ob_type)
    (PyTuple_SetItem ob_tuple 1 ob_value)
    (when (/= 0 ob_traceback)
      (PyTuple_SetItem ob_tuple 2 ob_traceback))
    (with-output-to-string (out)
      (loop with ob_list = (PyObject_CallObject ob_formatter ob_tuple) ; new
            for idx from 0 below (PyList_Size ob_list)
            for ob_unicode = (PySequence_GetItem ob_list idx) ; new
            for line = (native-to-string (PyUnicode_AsUTF8 ob_unicode) :external-format :utf-8)
            do (progn (write-string (string+ #\Space #\Space line)
                                    out)
                      (pydecref ob_unicode))
            finally (pydecref* ob_module ob_formatter ob_tuple ob_list)))))

(defun pyerror ()
  (error (make-python-exception)))

(defun pycheckn (val)
  (if* (pynull val)
     then (pyerror)
     else val))

(defun pycheckz (val)
  (if* (= -1 val)
     then (pyerror)
     else val))
