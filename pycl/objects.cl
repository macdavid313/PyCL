;;;; abstract.cl
(in-package #:pycl)

;;; Conditions
(define-condition null-pyobject-error (simple-pycl-error)
  ()
  (:documentation "Prevent applying NULL pyobject(s) to Python C APIs."))

(define-condition pyobject-conversion-error (simple-pycl-error)
  ()
  (:documentation "Conversion errors between Lisp and Python."))

;;; Utilities
(defun check-null-pyobject (ob)
  (declare (type pyobject ob))
  (when (pynull ob)
    (error 'null-pyobject-error :msg "Illegal argument: NULL pyobject")))

(defun pyobject-conversion-error (&key from to)
  (error 'pyobject-conversion-error
         :msg (format nil "don't know to convert a ~a to a ~a" from to)))

(defun make-pyobject* (prototype pytype)
  (declare (type (or pyobject (unsigned-byte #+32bit 32 #+64bit 64)) prototype)
           (type symbol pytype))
  (let ((ob (make-instance pytype :foreign-type 'PyObject
                                  :foreign-address (etypecase prototype
                                                     (pyobject (pystealref prototype))
                                                     ((unsigned-byte #+32bit 32 #+64bit 64) prototype)))))
    (if* (member pytype '(pynone pybool) :test 'eq)
       then ob                          ; static pyobject, skip finalization
       else (setf (pyobject-finalization ob)
                  (schedule-finalization ob 'pydecref))
            ob)))

;;; Abstract objects protocol
(defgeneric to-pyobject (thing)
  (:documentation "Default method for converting a lisp value to a pyobject instance.")
  (:method (thing) (pyobject-conversion-error :from thing :to 'pyobject))
  (:method ((ob pyobject)) ob))

(defgeneric from-pyobject (ob)
  (:documentation "Default method for converting a PyObject pointer to a pyobject instance.")
  (:method (ob) (pyobject-conversion-error :from ob :to t))
  (:method :before ((ob pyobject)) (check-null-pyobject ob)))

(defgeneric pyhasattr (ob attr)
  (:documentation "Returns t if a pyobject `ob' has attribute `attr'; returns nil otherwise.")
  (:method :before ((ob pyobject) attr)
    (declare (ignore attr))
    (check-null-pyobject ob))
  (:method ((ob pyobject) attr)
    (with-native-string (str (string+ attr) :external-format :utf-8)
      (= 1 (PyObject_HasAttrString ob str)))))

(defgeneric pyattr (ob attr)
  (:documentation "Retrieve an attribute `attr' from pyobject ob. This is the equivalent of the
Python expression `ob.attr'.")
  (:method :before ((ob pyobject) attr)
    (declare (ignore attr))
    (check-null-pyobject ob))
  (:method ((ob pyobject) attr)
    (with-native-string (str (string+ attr) :external-format :utf-8)
      (values (pycheckn (PyObject_GetAttrString ob str))
              (= 1 (PyObject_HasAttrString ob str)))))
  (:method ((ob pyobject) (attr pyobject))
    (check-null-pyobject attr)
    (pycheckn (PyObject_GetAttr ob attr))))

(defmethod (setf pyattr) (new (ob pyobject) attr)
  (check-null-pyobject ob)
  (let ((ob-new (to-pyobject new)))
    (with-native-string (str (string+ attr) :external-format :utf-8)
      (if* (and (pynull ob-new) (= 1 (PyObject_HasAttrString ob str)))
         then (pycheckz (PyObject_DelAttrString ob str))
         else (pycheckz (PyObject_SetAttrString ob str ob-new))))))

(defmethod (setf pyattr) (new (ob pyobject) (attr pyobject))
  (check-null-pyobject ob)
  (check-null-pyobject attr)
  (let ((ob-new (to-pyobject new)))
    (if* (and (pynull ob-new) (= 1 (PyObject_HasAttrString ob attr)))
       then (pycheckz (PyObject_DelAttrString ob attr))
       else (pycheckz (PyObject_SetAttrString ob attr ob-new)))))

(defgeneric pyrepr (ob)
  (:documentation "Compute a string representation of pyobject `ob'. This is the equivalent of the
Python expression `repr(ob)'")
  (:method ((ob pyobject))
    (let ((ob-unicode (pycheckn (PyObject_Repr ob))))
      (unwind-protect (values (native-to-string (PyUnicode_AsUTF8 ob-unicode)
                                                :external-format :utf-8))
        (pydecref ob-unicode)))))

(defgeneric pystr (ob)
  (:documentation "Compute a string representation of pyobject `ob'. This is the equivalent of the
Python expression `str(ob)'")
  (:method ((ob pyobject))
    (let ((ob-unicode (pycheckn (PyObject_Str ob))))
      (unwind-protect (values (native-to-string (PyUnicode_AsUTF8 ob-unicode)
                                                :external-format :utf-8))
        (pydecref ob-unicode)))))

(defgeneric pybytes (ob)
  (:documentation "Compute a bytes representation of object `ob'.")
  (:method :before ((ob pyobject))
    (check-null-pyobject ob))
  (:method ((ob pyobject))
    (let ((ob_bytes (pycheckn (PyObject_Bytes ob))))
      (with-static-fobjects ((buffer '(* :char)  :allocation :c)
                             (size   'Py_ssize_t :allocation :c))
        (pycheckz (PyBytes_AsStringAndSize ob_bytes buffer size))
        (pydecref ob_bytes)
        (native-to-octets (fslot-value-typed '(* :char) :c buffer)
                          :length (fslot-value-typed 'Py_ssize_t :c size)
                          :null-terminate nil)))))

(defgeneric pyisinstance (ob type)
  (:documentation "Return t if `ob' is an instance of the class `type' or a subclass of `type', or
nil if not.")
  (:method :before ((ob pyobject) type)
    (declare (ignore type))
    (check-null-pyobject ob))
  (:method ((ob pyobject) (type pyobject))
    (check-null-pyobject type)
    (= (pycheckz (PyObject_IsInstance ob type))
       1))
  (:method ((ob pyobject) (type symbol))
    (= (multiple-value-bind (ptr exists-p) (pyglobalptr type)
         (if* exists-p
            then (pycheckz (PyObject_IsInstance ob ptr))
            else (error 'simple-pycl-error :msg (string+ "Don't know how to handle python type: " type))))
       1))
  (:method ((ob pyobject) (type integer))
    (check-type type (unsigned-byte #+32bit 32 #+64bit 64))
    (assert (not (zerop type)))
    (= (pycheckz (PyObject_IsInstance ob type))
       1)))

(defgeneric pyhash (ob)
  (:documentation "Compute and return the hash value of an pyobject `ob'")
  (:method :before ((ob pyobject))
    (check-null-pyobject ob))
  (:method ((ob pyobject))
    (pycheckz (PyObject_Hash ob))))

(defgeneric pyistrue (ob)
  (:documentation "Returns t if the pyobject `ob' is considered to be true, and nil otherwise.")
  (:method :before ((ob pyobject))
    (check-null-pyobject ob))
  (:method ((ob pyobject))
    (= (pycheckz (PyObject_IsTrue ob))
       1)))

(defgeneric pynot (ob)
  (:documentation "Returns t if the pyobject `ob' is considered to be true, and nil otherwise.")
  (:method :before ((ob pyobject))
    (check-null-pyobject ob))
  (:method ((ob pyobject))
    (= (pycheckz (PyObject_Not ob))
       1)))

(defgeneric pytype (ob)
  (:documentation "Returns a type symbol corresponding to the object type of pyobject `ob'")
  (:method :before ((ob pyobject))
    (check-null-pyobject ob))
  (:method ((ob pyobject))
    (let ((ob-type (pycheckn (PyObject_Type ob))))
      (values ob-type (pyglobalptr ob-type)))))

(defgeneric pyitem (ob key)
  (:documentation "Return element of `ob' corresponding to the object `key'.")
  (:method :before ((ob pyobject) key)
    (declare (ignore key))
    (check-null-pyobject ob))
  (:method ((ob pyobject) key)
    (setq key (to-pyobject key))
    (check-null-pyobject key)
    (pycheckn (PyObject_GetItem ob key))))

(defmethod (setf pyitem) (v (ob pyobject) key)
  (check-null-pyobject ob)
  (setq v (to-pyobject v)
        key (to-pyobject key))
  (check-null-pyobject key)
  (= (pycheckz (PyObject_SetItem ob key v))
     0))

(defgeneric pylen (ob)
  (:documentation "Return the length of pyobject `ob'. This is the equivalent to the Python
expression `len(ob)'.")
  (:method :before ((ob pyobject))
    (check-null-pyobject ob))
  (:method ((ob pyobject))
    (pycheckz (PyObject_Length ob))))

(defgeneric pydir (ob)
  (:documentation "This is equivalent to the Python expression `dir(ob)', returning a (possibly
empty) list of strings appropriate for the object argument.")
  (:method ((ob pyobject))
    (let ((ob-list (pycheckn (PyObject_Dir ob))))
      (unwind-protect (loop with lst = (list)
                            for i from 0 below (pylen ob-list)
                            for ob-unicode = (pycheckn (PySequence_GetItem ob-list i))
                            do (progn (push (native-to-string (PyUnicode_AsUTF8 ob-unicode)
                                                              :external-format :utf-8)
                                            lst)
                                      (pydecref ob-unicode))
                            finally (return (nreverse lst)))
        (pydecref ob-list)))))

(defgeneric pyiter (ob)
  (:documentation "This is equivalent to the Python expression `iter(ob)'.")
  (:method :before ((ob pyobject))
    (check-null-pyobject ob))
  (:method ((ob pyobject))
    (make-pyobject* (pycheckn (PyObject_GetIter ob))
                    'pyiterator)))

(defclass pynumber   (pyobject) ())
(defclass pysequence (pyobject) ())
(defclass pymapping  (pyobject) ())
(defclass pyiterator (pyobject) ())

(defun pynumberp (ob)
  (and (typep ob 'pyobject)
       (= 1 (PyNumber_Check ob))))

(defun pysequencep (ob)
  (and (typep ob 'pyobject)
       (= 1 (PySequence_Check ob))))

(defun pymappingp (ob)
  (and (typep ob 'pyobject)
       (= 1 (PyMapping_Check ob))))

;;; Concrete objects protocol
;; bool
(defclass pybool      (pyobject) ())
;; number
(defclass pylong      (pynumber) ())
(defclass pyfloat     (pynumber) ())
(defclass pycomplex   (pynumber) ())
;; sequence
(defclass pybytes     (pysequence) ())
(defclass pybytearray (pysequence) ())
(defclass pyunicode   (pysequence) ())
(defclass pytuple     (pysequence) ())
(defclass pylist      (pysequence) ())
;; container
(defclass pydict      (pymapping) ())
(defclass pyset       (pyobject) ())

(defun make-pybool (x)
  (make-pyobject* (pyglobalptr (if x 'Py_True 'Py_False))
                  'pybool))

(defun make-pylong (x)
  (flet ((convert (x)
           (declare (type real x))
           (typecase x
             (unsigned-byte (pycheckn (PyLong_FromUnsignedLongLong x)))
             (integer (pycheckn (PyLong_FromLongLong x)))
             (t (pycheckn (PyLong_FromDouble (float x 0d0)))))))
    (if* (realp x)
       then (make-pyobject* (convert x) 'pylong)
       else (pyobject-conversion-error :from x :to 'pylong))))

(defun make-pyfloat (x)
  (if* (realp x)
     then (make-pyobject* (pycheckn (PyFloat_FromDouble (float (the real x) 0d0)))
                          'pyfloat)
     else (pyobject-conversion-error :from x :to 'pyfloat)))

(defun make-pycomplex (x)
  (declare (type complex x))
  (make-pyobject* (pycheckn (PyComplex_FromDoubles (float (realpart x) 0d0)
                                                   (float (imagpart x) 0d0)))
                  'pycomplex))

(defun make-pybytes (x)
  (flet ((convert (x)
           (with-native-string (str x :native-length-var len
                                      :external-format :utf-8)
             (make-pyobject* (PyBytes_FromStringAndSize str len) 'pybytes))))
    (typecase x
      ((or simple-string (simple-array (unsigned-byte 8) (*)))
       (convert x))
      (list
       (convert (coerce x '(simple-array (unsigned-byte 8) (*)))))
      (t (pyobject-conversion-error :from x :to 'pybytes)))))

(defun make-pybytearray (x)
  (make-pyobject* (etypecase x
                    (pyobject (pycheckn (PyByteArray_FromObject x)))
                    ((or simple-string
                         (simple-array (unsigned-byte 8) (*)))
                     (with-native-string (str x :native-length-var len :external-format :utf-8)
                       (pycheckn (PyByteArray_FromStringAndSize str len)))))
                  'pybytes))

(defun make-pyunicode (x)
  (typecase x
    ((or simple-string (simple-array (unsigned-byte 8) (*)))
     (with-native-string (cstr x :native-length-var len
                                 :external-format :utf-8)
       (pycheckn
        (make-pyobject* (PyUnicode_FromStringAndSize cstr len)
                        'pyunicode))))
    (t (pyobject-conversion-error :from x :to 'pyunicode))))

(defun make-pytuple (x)
  (flet ((convert (obs)
           (let ((ob-tuple (pycheckn (PyTuple_New (length obs)))))
             (loop for ob in obs
                   for idx from 0
                   do (PyTuple_SetItem ob-tuple idx (pystealref (to-pyobject ob)))
                   finally (return (make-pyobject* ob-tuple 'pytuple))))))
    (typecase x
      (list (convert x))
      (array (loop with obs = (list)
                   for idx from 0 below (length x)
                   do (push (aref x idx) obs)
                   finally (return (convert (nreverse obs)))))
      (t (pyobject-conversion-error :from x :to 'pytuple)))))

(defun make-pylist (x)
  (flet ((convert (obs)
           (let ((ob-list (pycheckn (PyList_New (length obs)))))
             (loop for ob in obs
                   for idx from 0
                   do (PyList_SetItem ob-list idx (pystealref (to-pyobject ob)))
                   finally (return (make-pyobject* ob-list 'pylist))))))
    (typecase x
      (list (convert x))
      (array (loop with obs = (list)
                   for idx from 0 below (length x)
                   do (push (aref x idx) obs)
                   finally (return (convert (nreverse obs)))))
      (t (pyobject-conversion-error :from x :to 'pylist)))))

(defun make-pydict (mapping)
  (let ((ob-dict (pycheckn (PyDict_New))))
    (typecase mapping
      (hash-table (with-hash-table-iterator (next mapping)
                    (while ()
                      (multiple-value-bind (more? k v) (next)
                        (if* more?
                           then (pycheckz (PyDict_SetItem ob-dict (to-pyobject k) (to-pyobject v)))
                           else (return ob-dict))))))
      (cons (loop for (k . v) in mapping
                  do (pycheckz (PyDict_SetItem ob-dict (to-pyobject k) (to-pyobject v)))
                  finally (return ob-dict)))
      (t (pyobject-conversion-error :from mapping :to 'pydict)))))

(defmethod to-pyobject ((x (eql nil)))  (make-pynone))
(defmethod to-pyobject ((x symbol))     (make-pyunicode (symbol-name x)))
(defmethod to-pyobject ((x integer))    (make-pylong x))
(defmethod to-pyobject ((x real))       (make-pyfloat x))
(defmethod to-pyobject ((x complex))    (make-pycomplex x))
(defmethod to-pyobject ((x list))       (make-pylist x))
(defmethod to-pyobject ((x array))      (make-pylist x))
(defmethod to-pyobject ((x string))     (make-pyunicode x))
(defmethod to-pyobject ((x hash-table)) (make-pydict x))

(defun from-pylong (ob)
  (with-static-fobjects ((overflow :int :allocation :c))
    (let ((res (PyLong_AsLongLongAndOverflow ob overflow)))
      (case (fslot-value-typed :int :c overflow)
        (1  (error 'simple-pycl-error :msg (format "~a is greater than PY_LLONG_MAX" ob)))
        (-1 (error 'simple-pycl-error :msg (format "~a is less than PY_LLONG_MIN" ob)))
        (t res)))))

(defun from-pyfloat (ob)
  (let ((res (PyFloat_AsDouble ob)))
    (when python-exception-occurred
      (pyerror 'PyFloat_AsDouble))
    res))

(defun from-pycomplex (ob)
  (let ((r (PyComplex_RealAsDouble ob))
        (i (PyComplex_ImagAsDouble ob)))
    (complex r i)))

(defun from-pybytes (ob)
  (with-static-fobjects ((buffer '(* :char) :allocation :c)
                         (len 'Py_ssize_t :allocation :c))
    (pycheckz (PyBytes_AsStringAndSize ob buffer len))
    (native-to-octets (fslot-value-typed '(* :char) :c buffer)
                      :length (fslot-value-typed 'Py_ssize_t :c len)
                      :null-terminate t)))

(defun from-pybytearray (ob)
  (let ((ptr (pycheckn (PyByteArray_AsString ob))))
    (native-to-octets ptr :null-terminate t)))

(defun from-pyunicode (ob)
  (values (native-to-string (PyUnicode_AsUTF8 ob) :external-format :utf-8)))

(defun from-pysequence (ob-seq)
  (loop with lst = (list)
        for i from 0 below (pylen ob-seq)
        for ob = (pycheckn (PySequence_GetItem ob-seq i))
        do (progn (push (from-pyobject ob) lst)
                  (pydecref ob))
        finally (return (nreverse lst))))

(defmethod from-pyobject ((ob pyobject))
  (multiple-value-bind (ptype ltype) (pytype ob)
    (declare (ignorable ptype))
    (ecase ltype
      (PyBool_Type (= (foreign-pointer-address ob) (pyglobalptr 'Py_True)))
      (PyLong_Type (from-pylong ob))
      (PyFloat_Type (from-pyfloat ob))
      (PyComplex_Type (from-pycomplex ob))
      (PyBytes_Type (from-pybytes ob))
      (PyByteArray_Type (from-pybytearray ob))
      (PyUnicode_Type (from-pyunicode ob))
      ((PyTuple_Type PyList_Type) (from-pysequence ob)))))

;; (defun pyimport! (module)
;;   (declare (type simple-string module))
;;   (with-native-string (str module :external-format :utf-8)
;;     (the pyobject (PyImport_ImportModule str))))

;; (defun check-args/pysequence-getter-setter (ob-seq idx)
;;   (check-type ob-seq pyobject)
;;   (assert (= 1 (PySequence_Check ob-seq)))
;;   (check-type idx (unsigned-byte #+32bit 32 #+64bit 64))
;;   (assert (< idx (PyObject_Size ob-seq))))

;; (defun pysequence-get (ob-seq idx)
;;   (check-args/pysequence-getter-setter ob-seq idx)
;;   (pycheckn
;;    (cond ((pytypep ob-seq 'PyTuple_Type) (PyTuple_GetItem ob-seq idx))
;;          ((pytypep ob-seq 'PyList_Type) (PyList_GetItem ob-seq idx))
;;          (t (PySequence_GetItem ob-seq idx)))))

;; (defun pysequence-set (ob-seq idx new-val)
;;   (check-args/pysequence-getter-setter ob-seq idx)
;;   (setq new-val (to-pyobject new-val))
;;   (pycheckz
;;    (cond ((pytypep ob-seq 'PyTuple_Type) (PyTuple_SetItem ob-seq idx (pystealref new-val)))
;;          ((pytypep ob-seq 'PyList_Type) (PyList_SetItem ob-seq idx (pystealref new-val)))
;;          (t (if* (pynull new-val)
;;                then (PySequence_DelItem ob-seq idx)
;;                else (PySequence_SetItem ob-seq idx new-val))))))

;; (defun pyhaskey (ob key)
;;   (check-type ob pyobject)
;;   (check-type key string)
;;   (assert (= 1 (PyMapping_Check ob)))
;;   (with-native-string (str key :external-format :utf-8)
;;     (= 1 (PyMapping_HasKeyString ob str))))

;; (defun pymapping-get (ob key)
;;   (check-type ob pyobject)
;;   (check-type key string)
;;   (assert (= 1 (PyMapping_Check ob)))
;;   (with-native-string (str key :external-format :utf-8)
;;     (values (pycheckn (PyMapping_GetItemString ob str))
;;             (= 1 (PyMapping_HasKeyString ob str)))))

;; (defun pymapping-set (ob key new-val)
;;   (check-type ob pyobject)
;;   (check-type key string)
;;   (assert (= 1 (PyMapping_Check ob)))
;;   (setq new-val (to-pyobject new-val))
;;   (with-native-string (str key :external-format :utf-8)
;;     (= 0 (pycheckz (PyMapping_SetItemString ob str new-val)))))
