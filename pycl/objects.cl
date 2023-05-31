;;;; objects.cl
(in-package #:pycl)

(defun pystr (ob)
  (let ((ob_unicode (PyObject_Str ob)))
    (unwind-protect (native-to-string (pyunicode-to-native ob_unicode)
                                      :external-format :utf-8)
      (pydecref ob_unicode))))

(defun pyrepr (ob)
  (let ((ob_unicode (PyObject_Repr ob)))
    (if* (pynull ob_unicode)
       then (make-python-exception 'pyrepr)
       else (unwind-protect (native-to-string (pyunicode-to-native ob_unicode)
                                              :external-format :utf-8)
              (pydecref ob_unicode)))))

(defun pytypep (ob type)
  ;; (check-type ob pyobject)
  ;; (check-type type symbol)
  (let ((res (PyObject_IsInstance ob (pyglobalptr type))))
    (declare (type (integer -1 1) res))
    (if* (= res -1)
       then (make-python-exception 'pytypep)
       else (= res 1))))

(defun pytype-of (ob)
  (when (pyobject-p ob)
    (let ((ob_type (PyObject_Type ob)))
      (unwind-protect (pyglobalptr ob_type)
        (pydecref ob_type)))))

(defun pyattr (ob attr)
  (when (and (pyobject-p ob)
             (or (stringp attr) (symbolp attr)))
    (setq attr (if (symbolp attr) (symbol-name attr) attr))
    (with-native-string (str attr :external-format :utf-8)
      (values (PyObject_GetAttrString ob str)
              (= 1 (PyObject_HasAttrString ob str))))))

(defun (setf pyattr) (new-val ob attr)
  (when (and (pyobject-p ob)
             (or (stringp attr) (symbolp attr))
             (pyobject-pointer-p new-val))
    (setq attr (if (symbolp attr) (symbol-name attr) attr))
    (with-native-string (attr attr :external-format :utf-8)
      (if* (and (pynull new-val) (PyObject_HasAttrString ob attr))
         then (PyObject_DelAttrString ob attr)
         else (PyObject_SetAttrString ob attr new-val)))))

(defun check-args/pyetl (ob_seq idx)
  (check-type ob_seq pyobject)
  (assert (PySequence_Check ob_seq))
  (check-type idx (unsigned-byte #+32bit 32 #+64bit 64))
  (assert (< idx (PyObject_Size ob_seq))))

(defun pylength (ob)
  (check-type ob pyobject)
  (PyObject_Length ob))

(defun pyelt (ob_seq idx)
  (check-args/pyetl ob_seq idx)
  (cond ((pytypep ob_seq 'PyTuple_Type) (PyTuple_GetItem ob_seq idx))
        ((pytypep ob_seq 'PyList_Type) (PyList_GetItem ob_seq idx))
        (t (PySequence_GetItem ob_seq idx))))

(defun (setf pyelt) (new_ob ob_seq idx)
  (check-args/pyetl ob_seq idx)
  (cond ((pytypep ob_seq 'PyTuple_Type) (PyTuple_SetItem ob_seq idx (pystealref new_ob)))
        ((pytypep ob_seq 'PyList_Type) (PyList_SetItem ob_seq idx (pystealref new_ob)))
        (t (if* (pynull new_ob)
              then (PySequence_DelItem ob_seq idx)
              else (PySequence_SetItem ob_seq idx (pystealref new_ob))))))

(defgeneric to-pyobject (thing)
  (:documentation "Default method for converting a lisp value to a PyObject pointer"))

(defgeneric from-pyobject (ob)
  (:documentation "Default method for converting a PyObject pointer to a lisp value"))

(defmethod to-pyobject ((n number))
  (typecase n
    (unsigned-byte (PyLong_FromUnsignedLongLong n))
    (integer (PyLong_FromLongLong n))
    ((or float ratio) (PyFloat_FromDouble (float n 0d0)))
    (t (PyComplex_FromDoubles (float (realpart n) 0d0)
                              (float (imagpart n) 0d0)))))

(defun @pynone ()
  (make-pyobject (pyglobalptr 'Py_None)))

(defun @pybool (x)
  (make-pyobject (pyglobalptr (if x 'Py_True 'Py_False))))

(defun @pylong (x)
  (etypecase x
    (unsigned-byte (PyLong_FromUnsignedLongLong x))
    (integer (PyLong_FromLongLong x))
    (ratio (PyLong_FromDouble (excl::ratio-to-double-float x)))
    (single-float (PyLong_FromDouble (float x 0d0)))
    (double-float (PyLong_FromDouble x))))

(defun @pyfloat (x)
  (check-type x real)
  (PyFloat_FromDouble (float (the real x) 0d0)))

(defun @pycomplex (x)
  (check-type x complex)
  (PyComplex_FromDoubles (float (realpart x) 0d0)
                         (float (imagpart x) 0d0)))

(defun @pybytes (x)
  (let ((data (etypecase x
                (string x)
                ((array (unsigned-byte 8) (*)) x)
                (list (coerce x '(simple-array (unsigned-byte 8) (*)))))))
    (declare (dynamic-extent data))
    (with-native-string (cstr data :native-length-var len :external-format :utf-8)
      (PyBytes_FromStringAndSize cstr len))))

(defun @pyunicode (x)
  (check-type x string)
  (with-native-string (cstr x :native-length-var len
                              :external-format :utf-8)
    (PyUnicode_FromStringAndSize cstr len)))

(defun @pytuple (len)
  (check-type len (unsigned-byte #+32bit 32 #+64bit 64))
  (PyTuple_New len))

