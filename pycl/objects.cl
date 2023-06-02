;;;; objects.cl
(in-package #:pycl)

(defgeneric to-pyobject (thing)
  (:documentation "Default method for converting a lisp value to a PyObject pointer")
  (:method ((fp pyobject)) fp))

(defgeneric from-pyobject (ob)
  (:documentation "Default method for converting a PyObject pointer to a lisp value"))

(defclass pynone (pyobject) ())
(defclass pybool (pyobject) ())
(defclass pylong (pyobject) ())
(defclass pyfloat (pyobject) ())
(defclass pycomplex (pyobject) ())
(defclass pybytes (pyobject) ())
(defclass pybytearray (pyobject) ())
(defclass pyunicode (pyobject) ())
(defclass pylist (pyobject) ())
(defclass pytuple (pyobject) ())
(defclass pydict (pyobject) ())

(defun make-pynone ()
  (make-pyobject (pyglobalptr 'Py_None) 'pynone))

(defun make-pybool (x)
  (make-pyobject (pyglobalptr (if x 'Py_True 'Py_False)) 'pybool))

(defun make-pylong (x)
  (make-pyobject (etypecase x
                   (unsigned-byte (PyLong_FromUnsignedLongLong x))
                   (integer (PyLong_FromLongLong x))
                   (real (PyLong_FromDouble (float x 0d0))))
                 'pylong))

(defun make-pyfloat (x)
  (make-pyobject (PyFloat_FromDouble (float (the real x) 0d0))
                 'pyfloat))

(defun make-pycomplex (x)
  (make-pyobject (PyComplex_FromDoubles (float (realpart x) 0d0)
                                        (float (imagpart x) 0d0))
                 'pycomplex))

(defun make-pybytes! (x)
  (with-native-string (str x :native-length-var len :external-format :utf-8)
    (make-pyobject (PyBytes_FromStringAndSize str len) 'pybytes)))

(defun make-pybytes (x)
  (etypecase x
    (array (make-pybytes! x))
    (list (make-pybytes! (coerce x '(simple-array (unsigned-byte 8) *))))))

(defun make-pybytearray (x)
  (etypecase x
    (pyobject (pycheckn (PyByteArray_FromObject x)))
    ((or string (array (unsigned-byte 8) (*)))
     (with-native-string (str x :native-length-var len :external-format :utf-8)
       (PyByteArray_FromStringAndSize str len)))))

(defun make-pyunicode (x)
  (with-native-string (cstr x :native-length-var len
                              :external-format :utf-8)
    (make-pyobject (PyUnicode_FromStringAndSize cstr len)
                   'pyunicode)))

;; (defun make-pylist! (lst)
;;   (let ((ob (PyList_New (length lst))))
;;     (check-type ob pyobject)
;;     (loop for item in lst
;;           for idx from 0
;;           do (PyList_SetItem ob idx (pystealref item)))
;;     ob))

;; (defmethod @pylist ((x list))
;;   (with-stack-list (ob-stack)
;;     (handler-case (dolist (elm x)
;;                     (push (to-pyobject elm) ob-stack))
;;       (error (e)
;;         (dolist (ob ob-stack)
;;           (pydecref ob))
;;         (error e)))
;;     (make-pylist-unsafe! (nreverse ob-stack))))

;; (defmethod @pylist ((x array))
;;   (let (ob-stack)
;;     (handler-case (loop for idx from 0 below (length x)
;;                         do (push (to-pyobject (aref x idx)) ob-stack))
;;       (error (e)
;;         (dolist (ob ob-stack)
;;           (pydecref ob))
;;         (error e)))
;;     (make-pylist-unsafe! (nreverse ob-stack))))

;; (defmethod @pytuple (x)
;;   (etypecase x
;;     ((or list array)
;;      (let ((ob_list (@pylist x))
;;            ob_tuple)
;;        (setq ob_tuple (PySequence_Tuple ob_list))
;;        (prog1 ob_tuple
;;          (pydecref ob_list))))))

;; (defun make-pydict-unsafe! (ht)
;;   (let ((ob (PyDict_New)))
;;     (check-type ob pyobject)
;;     (flet ((iter (k v)
;;              (with-native-string (str k :external-format :utf-8)
;;                (PyDict_SetItemString ob str v))))
;;       (maphash #'iter ht))
;;     ob))

;; (defmethod @pydict ((x hash-table))
;;   (with-stack-list (ob-stack)
;;     (let ((ht (make-hash-table :test 'string= :size (hash-table-size x))))
;;       (flet ((iter (k v)
;;                (setq v (to-pyobject v))
;;                (push v ob-stack)
;;                (setf (gethash (string+ k) ht) v)))
;;         (handler-case (maphash #'iter x)
;;           (error (e)
;;             (dolist (ob ob-stack)
;;               (pydecref ob))
;;             (clrhash ht)
;;             (error e))))
;;       (make-pydict-unsafe! ht))))

;; (defmethod @pydict ((x list))
;;   (with-stack-list (ob-stack)
;;     (let ((ht (make-hash-table :test 'string= :size (length x))))
;;       (handler-case (dolist (pair x)
;;                       (destructuring-bind (k . v) pair
;;                         (setf (gethash (string+ k) ht) (to-pyobject v))))
;;         (error (e)
;;           (dolist (ob ob-stack)
;;             (pydecref ob))
;;           (clrhash ht)
;;           (error e)))
;;       (make-pydict-unsafe! ht))))

(defmethod to-pyobject ((x (eql nil)))  (make-pynone))
(defmethod to-pyobject ((x symbol))     (make-pyunicode (symbol-name x)))
(defmethod to-pyobject ((x integer))    (make-pylong x))
(defmethod to-pyobject ((x ratio))      (make-pyfloat x))
(defmethod to-pyobject ((x float))      (make-pyfloat x))
(defmethod to-pyobject ((x complex))    (make-pycomplex x))
;; (defmethod to-pyobject ((x list))       (make-pylist x))
;; (defmethod to-pyobject ((x array))      (make-pylist x))
(defmethod to-pyobject ((x string))     (make-pyunicode x))
;; (defmethod to-pyobject ((x hash-table)) (make-pydict x))

;;; protocols
(defun pytype-of (ob)
  (let ((ob_type (pycheckn (PyObject_Type ob))))
    (unwind-protect (pyglobalptr ob_type)
      (pydecref ob_type))))

(defun pytypep (ob type)
  (let ((res (etypecase type
               (symbol (let ((ptr (pyglobalptr type)))
                         (when ptr (PyObject_IsInstance ob (pyglobalptr type)))))
               ((or pyptr (unsigned-byte #+32bit 32 #+64bit 64)) (PyObject_IsInstance ob type)))))
    (declare (type (integer -1 1) res))
    (= (pycheckz res) 1)))

(defun pylen (ob)
  (check-type ob pyobject)
  (pycheckz (PyObject_Length ob)))

(defun pystr (ob)
  (let ((ob_unicode (pycheckn (PyObject_Str ob))))
    (unwind-protect (from-pyunicode! ob_unicode)
      (pydecref ob_unicode))))

(defun pyrepr (ob)
  (let ((ob_unicode (pycheckn (PyObject_Repr ob))))
    (unwind-protect (from-pyunicode! ob_unicode)
      (pydecref ob_unicode))))

(defun check-args/pysequence-getter-setter (ob-seq idx)
  (check-type ob-seq pyobject)
  (assert (= 1 (PySequence_Check ob-seq)))
  (check-type idx (unsigned-byte #+32bit 32 #+64bit 64))
  (assert (< idx (PyObject_Size ob-seq))))

(defun pysequence-get (ob-seq idx)
  (check-args/pysequence-getter-setter ob-seq idx)
  (pycheckn
   (cond ((pytypep ob-seq 'PyTuple_Type) (PyTuple_GetItem ob-seq idx))
         ((pytypep ob-seq 'PyList_Type) (PyList_GetItem ob-seq idx))
         (t (PySequence_GetItem ob-seq idx)))))

(defun pysequence-set (ob-seq idx new-val)
  (check-args/pysequence-getter-setter ob-seq idx)
  (setq new-val (to-pyobject new-val))
  (pycheckz
   (cond ((pytypep ob-seq 'PyTuple_Type) (PyTuple_SetItem ob-seq idx (pystealref new-val)))
         ((pytypep ob-seq 'PyList_Type) (PyList_SetItem ob-seq idx (pystealref new-val)))
         (t (if* (pynull new-val)
               then (PySequence_DelItem ob-seq idx)
               else (PySequence_SetItem ob-seq idx new-val))))))

(defun pyhaskey (ob key)
  (check-type ob pyobject)
  (check-type key string)
  (assert (= 1 (PyMapping_Check ob)))
  (with-native-string (str key :external-format :utf-8)
    (= 1 (PyMapping_HasKeyString ob str))))

(defun pymapping-get (ob key)
  (check-type ob pyobject)
  (check-type key string)
  (assert (= 1 (PyMapping_Check ob)))
  (with-native-string (str key :external-format :utf-8)
    (values (pycheckn (PyMapping_GetItemString ob str))
            (= 1 (PyMapping_HasKeyString ob str)))))

(defun pymapping-set (ob key new-val)
  (check-type ob pyobject)
  (check-type key string)
  (assert (= 1 (PyMapping_Check ob)))
  (setq new-val (to-pyobject new-val))
  (with-native-string (str key :external-format :utf-8)
    (= 0 (pycheckz (PyMapping_SetItemString ob str new-val)))))
