;;;; abstract.cl
(in-package #:pycl)

(define-condition pyobject-conversion-error (simple-pycl-error)
  ())

(defun pyobject-conversion-error (&key from to)
  (error 'pyobject-conversion-error
         :msg (cond ((and from (null to))
                     (format nil "don't know how to convert ~a to a pyobject" from))
                    ((and (null from) to)
                     (format nil "don't know how to convert a pyobject to ~a" to))
                    (t (format nil "don't know to convert ~a to ~a" from to)))))

(defgeneric to-pyobject (thing)
  (:documentation "Default method for converting a lisp value to a PyObject pointer")
  (:method (thing) (pyobject-conversion-error :from thing))
  (:method ((fp pyobject)) fp))

(defgeneric from-pyobject (ob &rest args &key output-type-spec &allow-other-keys)
  (:documentation "Default method for converting a PyObject pointer to a lisp value"))

(defmethod pyhasattr ((ob pyobject) attr)
  (and (not (pynull ob))
       (with-native-string (str attr :external-format :utf-8)
         (= 1 (PyObject_HasAttrString ob str)))))

(defmethod pyattr ((ob pyobject) attr)
  (assert (not (pynull ob)))
  (with-native-string (str attr :external-format :utf-8)
    (values (PyObject_GetAttrString ob str)
            (= 1 (PyObject_HasAttrString ob str)))))

(defmethod (setf pyattr) (new (ob pyobject) attr)
  (assert (not (pynull ob)))
  (let ((ob-new (to-pyobject new)))
    (with-native-string (str attr :external-format :utf-8)
      (if* (and (pynull ob-new) (pyhasattr ob attr))
         then (PyObject_DelAttrString ob str)
         else (PyObject_SetAttrString ob str ob-new)))))

(defmethod pyrepr ((ob pyobject))
  (let ((ob_unicode (pycheckn (PyObject_Repr ob))))
    (unwind-protect (values (native-to-string (PyUnicode_AsUTF8 ob_unicode)
                                              :external-format :utf-8))
      (pydecref ob_unicode))))

(defmethod pystr ((ob pyobject))
  (let ((ob_unicode (pycheckn (PyObject_Str ob))))
    (unwind-protect (values (native-to-string (PyUnicode_AsUTF8 ob_unicode)
                                              :external-format :utf-8))
      (pydecref ob_unicode))))

(defmethod pybytes ((ob pyobject))
  (let ((ob_bytes (pycheckn (PyObject_Bytes ob))))
    (with-static-fobjects ((buffer '(* :char) :allocation :c)
                           (size 'Py_ssize_t :allocation :c))
      (pycheckz (PyBytes_AsStringAndSize ob_bytes buffer size))
      (pydecref ob_bytes)
      (native-to-octets (fslot-value-typed '(* :char) :c buffer)
                        :length (fslot-value-typed 'Py_ssize_t :c size)
                        :null-terminate nil))))

(defmethod pytypep ((ob pyobject) (type symbol))
  (= (pycheckz (let ((ptr (pyglobalptr type)))
                 (if* ptr
                    then (PyObject_IsInstance ob ptr)
                    else (error "Don't know how to find python type's address: ~a" type))))
     1))

(defmethod pytypep ((ob pyobject) (type integer))
  (check-type type (unsigned-byte #+32bit 32 #+64bit 64))
  (= (pycheckz (PyObject_IsInstance ob type)) 1))

(defmethod pytypep ((ob pyobject) (type pyobject))
  (= (pycheckz (PyObject_IsInstance ob type)) 1))

(defmethod pyhash ((ob pyobject))
  (pycheckz (PyObject_Hash ob)))

(defmethod pytrue-p ((ob pyobject))
  (= (pycheckz (PyObject_IsTrue ob))
     1))

(defmethod pynot ((ob pyobject))
  (= (pycheckz (PyObject_Not ob))
     1))

(defmethod pytype-of ((ob pyobject))
  (assert (not (pynull ob)))
  (let ((ob_type (pycheckn (PyObject_Type ob))))
    (unwind-protect (values (pyglobalptr ob_type) ob_type)
      (pydecref ob_type))))

(defmethod pygethash ((ob pyobject) key)
  (pycheckn (PyObject_GetItem ob (to-pyobject key))))

(defmethod (setf pygethash) (v (ob pyobject) key)
  (setq v (to-pyobject v)
        key (to-pyobject key))
  (= (pycheckz (PyObject_SetItem ob key v))
     0))

(defmethod pylen ((ob pyobject))
  (pycheckz (PyObject_Length ob)))


(defmethod pyiter ((ob pyobject))
  (pycheckn (PyObject_GetIter ob)))
