;;;; pycl.cl
(in-package #:pycl)

(defun pystr (ob)
  (check-type ob pyobject)
  (let ((ob_unicode (PyObject_Str ob)))
    (unwind-protect (native-to-string (pyunicode-to-native ob_unicode)
                                      :external-format :utf-8)
      (pydecref ob_unicode))))

(defun pyrepr (ob)
  (check-type ob pyobject)
  (let ((ob_unicode (PyObject_Repr ob)))
    (unwind-protect (native-to-string (pyunicode-to-native ob_unicode)
                                      :external-format :utf-8)
      (pydecref ob_unicode))))

(defun pytypep (ob type)
  (check-type ob pyobject)
  (= 1 (pycheckz (PyObject_IsInstance ob (pyglobalptr type)))))

(defun pytype-of (ob)
  (check-type ob pyobject)
  (let ((ob_type (pycheckn (PyObject_Type ob))))
    (prog1 (pyglobalptr ob_type)
      (pydecref ob_type))))

(defun pyattr (ob attr)
  (check-type ob pyobject)
  (check-type attr string)
  (with-native-string (str attr :external-format :utf-8)
    (pycheckn (PyObject_GetAttrString ob str))))
