;;;; pyobject.cl
(in-package #:pycl)

(defun pyobject-p (thing)
  (and (foreign-pointer-p thing)
       (eq 'PyObject (foreign-pointer-type thing))))

(deftype pyobject ()
  '(satisfies pyobject-p))

;; (declaim (ftype (function (t) pyobject) to-pyobject))
;; (defgeneric to-pyobject (thing)
;;   (:documentation "Convert a lisp value to a PyObject foreign pointer."))

;; (defgeneric from-pyobject (pyobj output-type-spec)
;;   (:documentation "Convert a PyObject foreign pointer to a value by the given lisp type specifier."))

(defun pyobject-eq (x y)
  (check-type x pyobject)
  (check-type y pyobject)
  (= (foreign-pointer-address x)
     (foreign-pointer-address y)))

(defun pynull (ob)
  (check-type ob pyobject)
  (zerop (foreign-pointer-address ob)))

(defun pystealref (ob)
  (declare (type pyobject ob))
  (let ((original-fp (foreign-pointer-address ob)))
    (setf (foreign-pointer-address ob) 0)
    original-fp))

;;; lifetime management --------
;;; GIL, GC, and weak references
;;;
#-os-threads
(defmacro with-python-gil ((&key safe) &body body)
  (declare (ignore safe))
  `(progn ,@body))

#+os-threads
(defmacro with-python-gil ((&key (safe t)) &body body)
  (let ((g (gensym "g"))
        (res (gensym "res")))
    (if* safe
       then `(let ((,g (PyGILState_Ensure))
                   ,res)
               (declare (type (mod 1) ,g)
                        (dynamic-extent ,g))
               (setq ,res (progn ,@body))
               (PyGILState_Release ,g)
               ,res)
       else `(let ((,g (PyGILState_Ensure)))
               (unwind-protect (progn ,@body)
                 (PyGILState_Release))))))

;; (defvar *lifetime-policy* :default ; (:new :borrow :steal :deferred)
;;   "A special variable that represents different policies for lifetime management")

;; (defun pyincref (o)
;;   (check-type o pyobject)
;;   (with-python-gil ()
;;     (Py_IncRef o)
;;     o))

;; (defun pydecref (o)
;;   (check-type o pyobject)
;;   (with-python-gil ()
;;     (Py_DecRef o))
;;   (setf (foreign-pointer-address o) 0)
;;   o)

;;; pyobject APIs
;; (defun pyreturn (o)
;;   (declare (type pyobject o))
;;   (foreign-pointer-address (pyincref (to-pyobject o))))

;; (defun pytypep (o type)
;;   (check-type o pyobject)
;;   (check-type type pyobject)
;;   (and (not (pynull o))
;;        (= 1 (with-python-gil ()
;;               (PyObject_IsInstance o type)))))

;; (defun pytype-of (o)
;;   (assert (not (pynull o)))
;;   ;; (with-python-gil ()
;;   ;;   (PyObject_Type o))
;;   (fslot-value-typed 'PyObject                   ; type
;;                      :c                          ; allocation
;;                      (foreign-pointer-address o) ; fp
;;                      'pycl.sys::ob_type)) ; fslot

;;; Exceptions
;;; see also conditions.cl
