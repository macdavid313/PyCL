;;;; object.cl
(in-package #:pycl)

(defun pyobject-p (thing)
  (and (foreign-pointer-p thing)
       (eq 'PyObject (foreign-pointer-type thing))))

(deftype pyobject ()
  '(satisfies pyobject-p))

(declaim (ftype (function (t) pyobject) to-pyobject))
(defgeneric to-pyobject (thing)
  (:documentation "Convert a lisp value to a PyObject foreign pointer."))

;; (defgeneric from-pyobject (pyobj output-type-spec)
;;   (:documentation "Convert a PyObject foreign pointer to a value by the given lisp type specifier."))

(defun pyobject-eq (x y)
  (declare (type pyobject x y))
  (= (foreign-pointer-address x)
     (foreign-pointer-address y)))

(defun pynull (o)
  (declare (type pyobject o))
  (zerop (foreign-pointer-address o)))

(defun pyincref (o)
  (declare (type pyobject o))
  (Py_IncRef o)
  o)

(defun pydecref (o)
  (declare (type pyobject o))
  (Py_DecRef o)
  (setf (foreign-pointer-address o) 0)
  o)

(defun pystealref (o)
  (declare (type pyobject o))
  (let ((original-fp (foreign-pointer-address o)))
    (setf (foreign-pointer-address o) 0)
    original-fp))

(defun pyreturn (o)
  (declare (type pyobject o))
  (foreign-pointer-address (pyincref (to-pyobject o))))

(defun pytypep (o type)
  (declare (type pyobject o type))
  (and (not (pynull o))
       (= 1 (PyObject_IsInstance o type))))

(defun pytype-of (o)
  (declare (type pyobject o))
  (assert (not (pynull o)))
  (PyObject_Type o))
