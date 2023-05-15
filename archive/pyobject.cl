;;;; pyobject.cl
(in-package #:pycl)

;;; Python C Types
(def-foreign-type Py_ssize_t :unsigned-nat)

(def-foreign-type PyObject
    ;; a mirror of PyObject C struct (for non-debugging Python builds)
    ;; we will not directly access the slots
    ;; and it will only be used for type annotation
    (:struct
     (ob_refcnt Py_ssize_t)
     (ob_type (* :void))))

(defclass pyptr (foreign-pointer)
  ()
  (:documentation "A foreign pointer for PyObject"))

(defmethod print-object ((x pyptr) stream)
  (let ((*print-base* 16))
    (print-unreadable-object (x stream :type t :identity nil)
      (format stream "@ #x~a" (foreign-pointer-address x)))))

(defmethod foreign-pointer-type ((x pyptr))
  'PyObject)

(defun make-pyptr (address)
  (declare (type #+32bit (unsigned-byte 32)
                 #+64bit (unsigned-byte 64)
                 address))
  (make-instance 'pyptr :foreign-address address))

(defstruct pyobject
  (ref (make-pyptr 0) :type pyptr))

(defgeneric to-pyobject (thing)
  (:documentation "Converts a value to a pyobject, which is a reference to PyObject.")
  (:method ((o pyobject)) o)            ; self -> self
  (:method ((ref pyptr))                ; pyptr -> pyobject
    (let ((pyobj (make-pyobject :ref ref)))
      ;; TODO: register finalizer?
      pyobj)))

(defgeneric from-pyobject (pyobj output-type-spec)
  (:documentation "Converts a Python object to a value by the given lisp type specifier."))

(defun pyobject-eq (x y)
  (= (foreign-pointer-address (pyobject-ref x))
     (foreign-pointer-address (pyobject-ref y))))

(defun pynull-p (pyobj)
  (declare (type pyobject pyobj))
  (zerop (foreign-pointer-address (pyobject-ref pyobj))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun convert-pyobject-ff-call/arg (action val ctype ltype)
    "Convert a pyobject to the corresponding foreign address."
    (declare (ignore ctype ltype))
    (case action
      (:convert (foreign-pointer-address (pyobject-ref val)))
      (:convert-type 'integer)
      (:identify :arg)
      (:check (check-type val pyobject))))

  (defun convert-pyobject-ff-call/ret (action val ctype ltype)
    "Convert a foriegn address to a pyobject."
    (declare (ignore ctype ltype))
    (case action
      (:convert (make-pyobject :ref (make-pyptr val)))
      (:convert-type 'integer)
      (:identify :return)
      (:allocate nil)
      (:will-allocate nil))))

(defmacro def-foreign-pycall (fname (&rest args) &body options)
  (labels ((rewrite-args ()
             (mapcar (lambda (arg)
                       (if* (equal (second arg) '(* PyObject))
                          then (append arg '(pyobject convert-pyobject-ff-call/arg))
                          else arg))
                     args))
           (rewrite-ret ()
             (let ((ret (getf options :returning)))
               (when (and ret (listp ret) (equal (car ret) '(* PyObject)))
                 (setf (getf options :returning)
                       (append ret '(pyobject convert-pyobject-ff-call/ret))))
               (append options '(:arg-checking nil)))))
    `(def-foreign-call ,fname (,@(rewrite-args))
       ,@(rewrite-ret))))

(def-foreign-pycall Py_DecRef ((obj (* PyObject)))
  :returning :void
  :call-direct t)

(defun pydecref (x)
  (Py_DecRef x)
  (setf (pyobject-ref x) (make-pyptr 0))
  x)

(def-foreign-pycall Py_IncRef ((obj (* PyObject)))
  :returning :void
  :call-direct t)

(defun pyincref (x)
  (Py_IncRef x)
  x)

(defun pystealref (o)
  (declare (type pyobject o))
  (let ((original-ref (pyobject-ref o)))
    (setf (pyobject-ref o) (make-pyptr 0))
    original-ref))

(defun pyreturn (x)
  (foreign-pointer-address (pyobject-ref (pyincref (to-pyobject x)))))

(def-foreign-pycall PyObject_IsInstance ((inst (* PyObject))
                                         (cls (* PyObject)))
  :returning :int
  :call-direct t)

(defun pytypep (o type)
  (declare (type pyobject o type))
  (and (not (pynull-p o))
       (= 1 (PyObject_IsInstance o type))))

(def-foreign-pycall PyObject_Type ((o (* PyObject)))
  :returning ((* PyObject))
  :strings-convert nil
  :call-direct t)

(defun pytype-of (o)
  (declare (type pyobject o))
  (assert (not (pynull-p o)))
  (PyObject_Type o))

(def-foreign-pycall PyObject_GetAttrString ((o (* PyObject)) (attr (* :char) simple-string))
  :returning ((* PyObject))
  :strings-convert t)

(defun pyattr (o attr)
  (declare (type pyobject o)
           (type simple-string attr))
  (assert (not (pynull-p o)))
  (let ((val (PyObject_GetAttrString o attr))
        err)
    (declare (type pyobject val))
    (when (and (pynull-p val) (pyerr-occurred-p))
      (setq err (make-python-error "PyObject_GetAttrString"))
      (when (not (= (foreign-pointer-address (pyobject-ref (python-error-type err)))
                    PyExc_AttributeError))
        (error err))
      (pyerr-clear))
    (when (pynull-p val)
      (error "Attribute error: ~s" attr))
    val))

(def-foreign-pycall PyObject_SetAttrString ((o (* PyObject))
                                            (attr (* :char) simple-string)
                                            (v (* PyObject)))
  :returning :int
  :strings-convert t)

(defun (setf pyattr) (val o attr)
  (assert (not (pynull-p o)))
  (when (and (= -1 (PyObject_SetAttrString o (string+ attr) (to-pyobject val)))
             (pyerr-occurred-p))
    (let ((err (make-python-error "PyObject_SetAttrString")))
      (when (not (= (foreign-pointer-address (pyobject-ref (python-error-type err)))
                    PyExc_AttributeError))
        (error err))
      (pyerr-clear)
      (error "Attribute error: ~s" attr)))
  0)

(def-foreign-pycall PyObject_HasAttrString ((o (* PyObject))
                                            (attr (* :char) simple-string))
  :returning :int
  :strings-convert t)

(defun pyhasattr-p (o attr)
  (assert (not (pynull-p o)))
  (= 1 (PyObject_HasAttrString o (string+ attr))))

;;; pybuffer
(def-foreign-type Py_buffer
    (:struct
     (buf (* :void))
     (obj (* PyObject))
     (len Py_ssize_t)
     (itemsize Py_ssize_t)

     (readonly :int)
     (ndim :int)
     (format (* :char))
     (shape (* Py_ssize_t))
     (strides (* Py_ssize_t))
     (suboffsets (* Py_ssize_t))
     (internal (* :void))))
