;;;; pycl.cl
(in-package #:pycl)

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

(defun pystr (ob)
  (let (ob_unicode ob_bytes bytes)
    (setq ob_unicode (pycheckn (PyObject_Str ob)))
    (assert (not (null ob_unicode)))
    (setq ob_bytes (PyUnicode_AsUTF8String ob_unicode))
    (pydecref ob_unicode)
    (assert (not (null ob_bytes)))
    (setq bytes (PyBytes_AsString ob_bytes))
    (pydecref ob_bytes)
    (values (native-to-string bytes))))

(defun pyrepr (ob)
  (let (ob_unicode ob_bytes bytes)
    (setq ob_unicode (PyObject_Repr ob))
    (assert (not (null ob_unicode)))
    (setq ob_bytes (PyUnicode_AsUTF8String ob_unicode))
    (pydecref ob_unicode)
    (assert (not (null ob_bytes)))
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
