;;;; sys.cl
(in-package :pycl.sys)

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

(defmacro @pyobject (address)
  `(and (/= 0 ,address)
        (make-instance 'pyptr :foreign-address ,address
                              :foreign-type 'PyObject)))

(defun pyobject-p (thing)
  (and (typep thing 'pyptr)
       (eq 'PyObject (foreign-pointer-type thing))))

(deftype pyobject ()
  '(satisfies pyobject-p))

(defun pyobject-eq (x y)
  (and (pyobject-p x)
       (pyobject-p y)
       (= (foreign-pointer-address x)
          (foreign-pointer-address y))))

(defun pyincref (ob)
  (when (pyobject-p ob)
    (with-python-gil ()
      (Py_IncRef ob)))
  ob)

(defun pydecref (ob)
  (when (pyobject-p ob)
    (with-python-gil ()
      (Py_DecRef ob)))
  nil)

(defun pydecref* (&rest obs)
  (with-python-gil ()
    (dolist (ob obs)
      (when (pyobject-p ob)
        (Py_DecRef ob)
        (setf (foreign-pointer-address ob) 0))))
  nil)

(defmacro pystealref (ob-var)
  `(when (pyobject-p ,ob-var)
     (prog1 (foreign-pointer-address ,ob-var)
       (setf ,ob-var nil))))

;;; Compiler macros
(define-compiler-macro pyobject-p (thing)
  `(and (typep ,thing 'pyptr)
        (eq 'PyObject (foreign-pointer-type ,thing))))

(define-compiler-macro pyobject-eq (x y)
  `(and (pyobject-p ,x)
        (pyobject-p ,y)
        (= (foreign-pointer-address ,x)
           (foreign-pointer-address ,y))))
