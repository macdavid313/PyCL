;;;; sys.cl
(in-package :pycl.sys)

;;; GIL
(def-foreign-call (check-python-gil "PyGILState_Check") (:void)
  :returning :boolean
  :arg-checking nil
  :call-direct t
  :allow-gc :always)

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
(defun make-pyobject (address)
  (if* (= 0 address)
     then +pynull+
     else (make-instance 'pyptr :foreign-address address
                                :foreign-type 'PyObject)))

(define-compiler-macro make-pyobject (address)
  `(if* (= 0 ,address)
      then +pynull+
      else (make-instance 'pyptr :foreign-address ,address
                                 :foreign-type 'PyObject)))

(defun pyobject-p (thing)
  (and (typep thing 'pyptr)
       (eq 'PyObject (foreign-pointer-type thing))))

(define-compiler-macro pyobject-p (thing)
  `(and (typep ,thing 'pyptr)
        (eq 'PyObject (foreign-pointer-type ,thing))))

(deftype pyobject ()
  '(satisfies pyobject-p))

(defun pynull (thing)
  (if* (pyobject-p thing)
     then (eq thing +pynull+)
     else thing))

(define-compiler-macro pynull (thing)
  `(if* (pyobject-p ,thing)
      then (eq ,thing +pynull+)
      else ,thing))

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
  (when (not (pynull ob))
    (Py_IncRef ob))
  ob)

(define-compiler-macro pyincref (ob)
  `(progn (when (not (pynull ,ob))
            (Py_IncRef ,ob))
          ,ob))

(defun pydecref (ob)
  (when (not (pynull ob))
    (Py_DecRef ob)
    (setf (foreign-pointer-address ob) 0))
  +pynull+)

(define-compiler-macro pydecref (ob)
  `(prog1 +pynull+
     (when (not (pynull ,ob))
       (Py_DecRef ,ob)
       (setf (foreign-pointer-address ,ob) 0))))

(defun pydecref* (&rest obs)
  (dolist (ob obs nil)
    (when (not (pynull ob))
      (Py_DecRef ob)
      (setf (foreign-pointer-address ob) 0))))

(define-compiler-macro pydecref* (&rest obs)
  (flet ((transform (ob)
           `(when (not (pynull ,ob))
              (Py_DecRef ,ob)
              (setf (foreign-pointer-address ,ob) 0))))
    `(progn ,@(mapcar #'transform obs)
            nil)))

(defmacro pystealref (ob-var)
  "The caller (thief) will take the ownership so you are NOT responsible anymore.
This macro should always be used \"in place\" e.g. (PyList_SetItem ob_list idx (pystealref ob_item))"
  `(when (not (pynull ,ob-var))
     (prog1 (foreign-pointer-address ,ob-var)
       (setf ,ob-var +pynull+))))

;;; Conditions
(define-condition pycl-condition (condition)
  ()
  (:report report-pycl-condition))

(defgeneric report-pycl-condition (c stream)
  (:documentation "Report pycl and python related condition."))
