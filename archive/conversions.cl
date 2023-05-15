;;;; conversions.cl
(in-package #:pycl)

;;; TODO: The None Object

;;; Numeric Objects
(def-foreign-pycall PyLong_FromUnsignedLongLong ((v :unsigned-long-long))
  :returning ((* PyObject))
  :strings-convert nil
  :call-direct t)

(def-foreign-pycall PyLong_FromLongLong ((v :long-long))
  :returning ((* PyObject))
  :strings-convert nil
  :call-direct t)

(def-foreign-pycall PyFloat_FromDouble ((v :double))
  :returning ((* PyObject))
  :strings-convert nil
  :call-direct t)

(def-foreign-pycall PyComplex_FromDoubles ((real :double) (imag :double))
  :returning ((* PyObject))
  :strings-convert nil
  :call-direct t)

(def-foreign-pycall PyBool_FromLong ((v :long))
  :returning ((* PyObject))
  :call-direct t
  :strings-convert nil)

(def-foreign-pycall PyLong_AsUnsignedLongLong ((obj (* PyObject)))
  :returning :unsigned-long-long
  :strings-convert nil
  :call-direct t)

(def-foreign-pycall PyFloat_AsDouble ((obj (* PyObject)))
  :returning :unsigned-long-long
  :strings-convert nil
  :call-direct t)

(def-foreign-pycall PyComplex_RealAsDouble ((obj (* PyObject)))
  :returning :double
  :strings-convert nil
  :call-direct t)

(def-foreign-pycall PyComplex_ImagAsDouble ((obj (* PyObject)))
  :returning :double
  :strings-convert nil
  :call-direct t)

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

(def-foreign-pycall PyObject_Repr ((obj (* PyObject)))
  :returning ((* PyObject))
  :strings-convert nil
  :call-direct t)

(def-foreign-pycall PyObject_Str ((obj (* PyObject)))
  :returning ((* PyObject))
  :strings-convert nil
  :call-direct t)

(def-foreign-pycall PyUnicode_AsUTF8 ((o (* PyObject)))
  :returning ((* :char) simple-string)
  :strings-convert t)

(defun pystring (o)
  (declare (type pyobject o))
  (if* (pynull-p o)
     then "NULL"
     else (let ((s (PyObject_Repr o)))
            (if* (pynull-p s)
               then (pyerr-clear)
                    (write-to-string (pyobject-ref o))
               else (PyUnicode_AsUTF8 s)))))

(defmethod print-object ((o pyobject) stream)
  (print-unreadable-object (o stream :type t)
    (write-string (pystring o) stream)))
