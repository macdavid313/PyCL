;;;; pycl.cl
(in-package #:pycl)

(defun pystr (ob)
  (assert (not (pynull ob)))
  (let ((ob_unicode (PyObject_Str ob)))
    (unwind-protect (native-to-string (pyunicode-to-native ob_unicode)
                                      :external-format :utf-8)
      (pydecref ob_unicode))))

(defun pyrepr (ob)
  (assert (not (pynull ob)))
  (let ((ob_unicode (PyObject_Repr ob)))
    (unwind-protect (native-to-string (pyunicode-to-native ob_unicode)
                                      :external-format :utf-8)
      (pydecref ob_unicode))))

(defun pytypep (ob type)
  (assert (not (pynull ob)))
  (= 1 (pycheckz (PyObject_IsInstance ob (pyglobalptr type)))))

(defun pytype-of (ob)
  (assert (not (pynull ob)))
  (let ((ob_type (pycheckn (PyObject_Type ob))))
    (prog1 (pyglobalptr ob_type)
      (pydecref ob_type))))

(defun pyattr (ob attr)
  (assert (not (pynull ob)))
  (check-type attr string)
  (with-native-string (str attr :external-format :utf-8)
    (pycheckn (PyObject_GetAttrString ob str))))
