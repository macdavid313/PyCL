;;;; pycl.cl
(in-package #:pycl)

;;; lifetime management --------
;;; GIL, GC, and weak references
;;;
#-os-threads
(defmacro with-python-gil ((&key safe) &body body)
  (declare (ignore safe))
  `(progn ,@body))

#+os-threads
(defmacro with-python-gil ((&key (safe t)) &body body)
  (let ((g (gensym "g"))
        (res (gensym "res")))
    (if* safe
       then `(let ((,g (PyGILState_Ensure))
                   ,res)
               (declare (type (mod 1) ,g)
                        (dynamic-extent ,g))
               (setq ,res (progn ,@body))
               (PyGILState_Release ,g)
               ,res)
       else `(let ((,g (PyGILState_Ensure)))
               (unwind-protect (progn ,@body)
                 (PyGILState_Release))))))

(defun pycheckv (res checker place)
  (cond ((funcall checker res)
         res)
        ((not (pynull (PyErr_Occurred)))
         (with-stack-fobjects ((type* #1='(* PyObject))
                               (value* #1#)
                               (traceback* #1#))
           (PyErr_Fetch type* value* traceback*)
           (PyErr_NormalizeException type* value* traceback*)
           (let ((type (make-foreign-pointer :foreign-address (fslot-value-typed #1# :foreign type*)
                                             :foreign-type 'PyObject))
                 (value (make-foreign-pointer :foreign-address (fslot-value-typed #1# :foreign value*)
                                              :foreign-type 'PyObject))
                 (traceback (make-foreign-pointer :foreign-address (fslot-value-typed #1# :foreign traceback*)
                                                  :foreign-type 'PyObject)))
             (prog1 (make-instance 'python-error :msg (string+ place)
                                                 :type type
                                                 :value value
                                                 :traceback traceback)
               (PyErr_Clear)))))
        (t (make-foreign-pointer :foreign-address 0 :foreign-type 'PyObject))))

(defmacro pycheckn (form &optional place)
  (let ((res (gensym "res"))
        (place (if place place (car form))))
    `(let ((,res ,form))
       (pycheckv ,res #.(complement 'pynull) ',place))))

(defmacro pycheckz (form &optional place)
  (let ((res (gensym "res"))
        (place (if place place (car form))))
    `(let ((,res ,form))
       (pycheckv ,res #.(complement 'minusp) ',place))))

(defgeneric to-pyobject (thing)
  (:documentation "Convert a lisp value to a PyObject foreign pointer."))

(defmethod to-pyobject ((x number))
  (etypecase x
    (unsigned-byte
     (PyLong_FromUnsignedLongLong x))
    (integer
     (PyLong_FromLongLong x))
    ((or float ratio)
     (PyFloat_FromDouble (float x 0d0)))
    (complex
     (PyComplex_FromDoubles (float (realpart x) 0d0)
                            (float (imagpart x) 0d0)))))

(defgeneric from-pyobject (pyobj output-type-spec)
  (:documentation "Convert a PyObject foreign pointer to a value by the given lisp type specifier."))

(defun pynull (ob)
  (etypecase ob
    (foreign-pointer (= 0 (foreign-pointer-address ob)))
    (unsigned-byte (= 0 ob))))

(defun pyincref (o)
  (with-python-gil ()
    (Py_IncRef o)
    o))

(defun pydecref (o)
  (when (not (pynull o))
    (with-python-gil ()
      (Py_DecRef o))
    (setf (foreign-pointer-address o) 0))
  o)

(defun pystr (ob)
  (let (ob_unicode ob_bytes bytes)
    (setq ob_unicode (PyObject_Str ob))
    (assert (not (pynull ob_unicode)))
    (setq ob_bytes (PyUnicode_AsUTF8String ob_unicode))
    (pydecref ob_unicode)
    (assert (not (pynull ob_bytes)))
    (setq bytes (PyBytes_AsString ob_bytes))
    (pydecref ob_bytes)
    (values (native-to-string bytes))))

(defun pyrepr (ob)
  (let (ob_unicode ob_bytes bytes)
    (setq ob_unicode (PyObject_Repr ob))
    (assert (not (pynull ob_unicode)))
    (setq ob_bytes (PyUnicode_AsUTF8String ob_unicode))
    (pydecref ob_unicode)
    (assert (not (pynull ob_bytes)))
    (setq bytes (PyBytes_AsString ob_bytes))
    (pydecref ob_bytes)
    (values (native-to-string bytes))))

;; (defun pyattr (ob attr)
;;   (let (val error)
;;     (with-native-string (attr attr :external-format :utf-8)
;;       (setq val (pycheckn (PyObject_GetAttrString ob attr))))
;;     (typecase val
;;       (foreign-pointer ()))
;;     (when (and (pynull val) (not (zerop (PyErr_Occurred))))
;;       (setq err )))
;;   )

;;; Exceptions
;; (defun make-python-error (msg)
;;   (declare (type (or nil python-error) e)
;;            (ignorable e))
;;   (make-condition 'python-error :msg (string+ msg)
;;                                 :type (python-error-type e)
;;                                 :val (python-error-val e)
;;                                 :traceback (python-error-traceback e)))


;; (defun pyobject-p (thing)
;;   (and (foreign-pointer-p thing)
;;        (eq 'PyObject (foreign-pointer-type thing))))

;; (deftype pyobject ()
;;   '(satisfies pyobject-p))

;; (defun pyobject-eq (x y)
;;   (check-type x pyobject)
;;   (check-type y pyobject)
;;   (= (foreign-pointer-address x)
;;      (foreign-pointer-address y)))

;; (defun pystealref (ob)
;;   (declare (type pyobject ob))
;;   (let ((original-fp (foreign-pointer-address ob)))
;;     (setf (foreign-pointer-address ob) 0)
;;     original-fp))
;;
;;; pyobject APIs
;; (defun pyreturn (o)
;;   (declare (type pyobject o))
;;   (foreign-pointer-address (pyincref (to-pyobject o))))

;; (defun pytypep (o type)
;;   (check-type o pyobject)
;;   (check-type type pyobject)
;;   (and (not (pynull o))
;;        (= 1 (with-python-gil ()
;;               (PyObject_IsInstance o type)))))

;; (defun pytype-of (o)
;;   (assert (not (pynull o)))
;;   ;; (with-python-gil ()
;;   ;;   (PyObject_Type o))
;;   (fslot-value-typed 'PyObject                   ; type
;;                      :c                          ; allocation
;;                      (foreign-pointer-address o) ; fp
;;                      'pycl.sys::ob_type)) ; fslot

;;; Exceptions
;;; see also conditions.cl
