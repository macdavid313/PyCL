;;;; objects.cl
(in-package #:pycl)

(define-condition pyobject-conversion-error (simple-pycl-error)
  ())

(defun make-pyobject-conversion-error (&key from to)
  (make-instance 'pyobject-conversion-error
                 :msg (cond ((and from (null to))
                             (format nil "don't know how to convert ~a to pyobject" from))
                            ((and (null from) to)
                             (format nil "don't know how to convert a pyobject to ~a" to))
                            (t (format nil "don't know to convert ~a to ~a" from to)))))

(defun make-pyobject (x &optional subclass)
  (declare (type (or pyobject (unsigned-byte #+32bit 32 #+64bit 64)) x))
  (etypecase x
    ((unsigned-byte #+32bit 32 #+64bit 64)
     (if* (= 0 x)
        then *pynull*
        else (make-instance (if subclass subclass 'pyobject)
                            :foreign-type 'PyObject
                            :foreign-address x)))
    (pyobject (if* (pynull x)
                 then *pynull*
                 else (make-instance (if subclass subclass 'pyobject)
                                     :foreign-type 'PyObject
                                     :foreign-address (foreign-pointer-address x))))))

(defgeneric to-pyobject (thing)
  (:documentation "Default method for converting a lisp value to a PyObject pointer")
  (:method (thing) (values *pynull* (make-pyobject-conversion-error :from thing)))
  (:method ((fp pyobject)) fp))

(defgeneric from-pyobject (ob output-type-spec)
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
  (make-pyobject (pyglobalptr (if x 'Py_True 'Py_False))
                 'pybool))

(defun make-pylong (x)
  (flet ((convert! (x)
           (declare (type real x))
           (typecase x
             (unsigned-byte (PyLong_FromUnsignedLongLong x))
             (integer (PyLong_FromLongLong x))
             (t (PyLong_FromDouble (float x 0d0))))))
    (if* (realp x)
       then (pycheckn (make-pyobject (convert! x) 'pylong))
       else (values *pynull* nil))))

(defun make-pyfloat (x)
  (if* (realp x)
     then (pycheckn
           (make-pyobject (PyFloat_FromDouble (float (the real x) 0d0))
                          'pyfloat))
     else (values *pynull* nil)))

(defun make-pycomplex (x)
  (if* (complexp x)
     then (pycheckn
           (make-pyobject (PyComplex_FromDoubles (float (realpart x) 0d0)
                                                 (float (imagpart x) 0d0))
                          'pycomplex))
     else (values *pynull* nil)))

(defun make-pybytes (x)
  (flet ((convert! (x)
           (with-native-string (str x :native-length-var len :external-format :utf-8)
             (make-pyobject (PyBytes_FromStringAndSize str len) 'pybytes))))
    (typecase x
      ((or simple-string (simple-array (unsigned-byte 8) (*)))
       (convert! x))
      (list
       (make-pybytes (ignore-errors (coerce x '(simple-array (unsigned-byte 8) (*))))))
      (t
       (values *pynull* nil)))))

(defun make-pybytearray (x)
  (declare (type (or pyobject
                     simple-string
                     (simple-array (unsigned-byte 8) (*)))
                 x))
  (typecase x
    (pyobject (pycheckn (PyByteArray_FromObject x)))
    ((or simple-string
         (simple-array (unsigned-byte 8) (*)))
     (with-native-string (str x :native-length-var len :external-format :utf-8)
       (pycheckn (PyByteArray_FromStringAndSize str len))))
    (t (values *pynull* nil))))

(defun make-pyunicode (x)
  (if* (or (stringp x)
           (typep x '(simple-array (unsigned-byte 8) (*))))
     then  (with-native-string (cstr x :native-length-var len
                                       :external-format :utf-8)
             (pycheckn
              (make-pyobject (PyUnicode_FromStringAndSize cstr len)
                             'pyunicode)))
     else (values *pynull* nil)))

(defun make-pytuple (x)
  (flet ((convert! (obs)
           (let ((ob-tuple (PyTuple_New (length obs))))
             (if* (pynull ob-tuple)
                then (values *pynull* (pyexcept))
                else (loop for ob in obs
                           for idx from 0
                           do (PyTuple_SetItem ob-tuple idx (pystealref ob)))
                     (make-pyobject ob-tuple 'pytuple)))))
    (typecase x
      (list (loop with obs = (list)
                  for val in x
                  do (multiple-value-bind (ob exception)
                         (to-pyobject val)
                       (if* exception
                          then (dolist (ob obs) (pydecref ob))
                               (return-from make-pytuple (values *pynull* exception))
                          else (push ob obs)))
                  finally (return (convert! (nreverse obs)))))
      (array (loop with obs = (list)
                   for idx from 0 below (length x)
                   do (multiple-value-bind (ob exception)
                          (to-pyobject (aref x idx))
                        (if* exception
                           then (dolist (ob obs) (pydecref ob))
                                (return-from make-pytuple (values *pynull* exception))
                           else (push ob obs)))
                   finally (return (convert! (nreverse obs)))))
      (t (values *pynull* (make-pyobject-conversion-error :from x :to 'pytuple))))))

(defun make-pylist (x)
  (flet ((convert! (obs)
           (let ((ob-list (PyList_New (length obs))))
             (if* (pynull ob-list)
                then (values *pynull* (pyexcept))
                else (loop for ob in obs
                           for idx from 0
                           do (PyList_SetItem ob-list idx (pystealref ob)))
                     (make-pyobject ob-list 'pylist)))))
    (typecase x
      (list (loop with obs = (list)
                  for val in x
                  do (multiple-value-bind (ob exception)
                         (to-pyobject val)
                       (if* exception
                          then (dolist (ob obs) (pydecref ob))
                               (return-from make-pylist (values *pynull* exception))
                          else (push ob obs)))
                  finally (return (convert! (nreverse obs)))))
      (array (loop with obs = (list)
                   for idx from 0 below (length x)
                   do (multiple-value-bind (ob exception)
                          (to-pyobject (aref x idx))
                        (if* exception
                           then (dolist (ob obs) (pydecref ob))
                                (return-from make-pylist (values *pynull* exception))
                           else (push ob obs)))
                   finally (return (convert! (nreverse obs)))))
      (t (values *pynull* (make-pyobject-conversion-error :from x :to 'pylist))))))

;; (defun make-pydict! (ht)
;;   (let ((ob (PyDict_New)))
;;     (pycheckn ob)
;;     (with-hash-table-iterator (next ht)
;;       (while ()
;;         (multiple-value-bind (more? k v) (next)
;;           (if* more?
;;              then (with-native-string (str k :external-format :utf-8)
;;                     (PyDict_SetItemString ob str v))
;;              else (return ob)))))))

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
(defmethod to-pyobject ((x list))       (make-pylist x))
(defmethod to-pyobject ((x array))      (make-pylist x))
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
               ((or pyobject (unsigned-byte #+32bit 32 #+64bit 64)) (PyObject_IsInstance ob type)))))
    (declare (type (integer -1 1) res))
    (= (pycheckz res) 1)))

(defun pylen (ob)
  (check-type ob pyobject)
  (pycheckz (PyObject_Length ob)))

(defun pystr (ob)
  (let ((ob_unicode (pycheckn (PyObject_Str ob))))
    (unwind-protect (pyunicode-to-string ob_unicode)
      (pydecref ob_unicode))))

(defun pyrepr (ob)
  (let ((ob_unicode (pycheckn (PyObject_Repr ob))))
    (unwind-protect (pyunicode-to-string ob_unicode)
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
