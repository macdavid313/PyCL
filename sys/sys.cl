;;;; sys.cl
(in-package :pycl.sys)

;;; GIL
#-smp
(defmacro with-python-gil ((&key safe) &body body)
  (declare (ignore safe))
  `(progn ,@body))

#+smp
(defmacro with-python-gil ((&key (unwind-protect t)) &body body)
  (let ((g (gensym "g"))
        (res (gensym "res")))
    (if* unwind-protect
       then `(let ((,g (PyGILState_Ensure))
                   ,res)
               (declare (type (mod 1) ,g))
               (setq ,res (progn ,@body))
               (PyGILState_Release ,g)
               ,res)
       else `(let ((,g (PyGILState_Ensure)))
               (unwind-protect (progn ,@body)
                 (PyGILState_Release))))))

;;; pyobject
;;; APIs and Utilities
(defmacro @pyobject (address)
  `(and (/= 0 ,address)
        (make-instance 'pyptr :foreign-address ,address
                              :foreign-type 'PyObject)))

(defun pyobject-p (thing)
  (and (typep thing 'pyptr)
       (eq 'PyObject (foreign-pointer-type thing))))

(define-compiler-macro pyobject-p (thing)
  `(and (typep ,thing 'pyptr)
        (eq 'PyObject (foreign-pointer-type ,thing))))

(deftype pyobject ()
  '(satisfies pyobject-p))

(defun pyobject-eq (x y)
  (and (pyobject-p x)
       (pyobject-p y)
       (= (foreign-pointer-address x)
          (foreign-pointer-address y))))

(define-compiler-macro pyobject-eq (x y)
  `(and (pyobject-p ,x)
        (pyobject-p ,y)
        (= (foreign-pointer-address ,x)
           (foreign-pointer-address ,y))))

(defun pyincref (ob)
  (when (pyobject-p ob)
    (Py_IncRef ob))
  ob)

(define-compiler-macro pyincref (ob)
  `(progn (when (pyobject-p ,ob)
            (Py_IncRef ,ob))
          ,ob))

(defun pydecref (ob)
  (when (pyobject-p ob)
    (Py_DecRef ob)
    (setf (foreign-pointer-address ob) 0))
  nil)

(define-compiler-macro pydecref (ob)
  `(prog1 nil
     (when (pyobject-p ,ob)
       (Py_DecRef ,ob)
       (setf (foreign-pointer-address ,ob) 0))))

(defun pydecref* (&rest obs)
  (dolist (ob obs nil)
    (when (pyobject-p ob)
      (Py_DecRef ob)
      (setf (foreign-pointer-address ob) 0))))

(define-compiler-macro pydecref* (&rest obs)
  (flet ((transform (ob)
           `(when (pyobject-p ,ob)
              (Py_DecRef ,ob)
              (setf (foreign-pointer-address ,ob) 0))))
    `(progn ,@(mapcar #'transform obs)
            nil)))

(defmacro pystealref (ob-var)
  "The caller (thief) will take the ownership so you are NOT responsible anymore.
This macro should always be used \"in place\" e.g. (PyList_SetItem ob_list idx (pystealref ob_item))"
  `(when (pyobject-p ,ob-var)
     (prog1 (foreign-pointer-address ,ob-var)
       (setf (foreign-pointer-address ,ob-var) 0)
       (setf ,ob-var nil))))

;;; Conditions
(define-condition pycl-condition (condition)
  ()
  (:report report-pycl-condition))

(defgeneric report-pycl-condition (c stream)
  (:documentation "Report pycl and python related condition."))

(defmethod print-object ((c pycl-condition) stream)
  "Default printer for pycl-condition."
  (print-unreadable-object (c stream :type t :identity t)
    (report-pycl-condition c stream)))
