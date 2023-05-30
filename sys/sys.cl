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
  "Test if thing is a VALID (non-null) python foreign pointer."
  (and (not (eq thing +pynull+))
       (typep thing 'pyptr)
       (eq 'PyObject (foreign-pointer-type thing))
       (/= 0 (foreign-pointer-address thing))))

(define-compiler-macro pyobject-p (thing)
  `(and (not (eq ,thing +pynull+))
        (typep ,thing 'pyptr)
        (eq 'PyObject (foreign-pointer-type ,thing))
        (/= 0 (foreign-pointer-address ,thing))))

(deftype pyobject ()
  '(satisfies pyobject-p))

(defun pynull (thing)
  (or (eq thing +pynull+)
      (and (typep thing 'pyptr)
           (eq 'PyObject (foreign-pointer-type thing))
           (= 0 (foreign-pointer-address thing)))))

(define-compiler-macro pynull (thing)
  `(or (eq ,thing +pynull+)
       (and (typep ,thing 'pyptr)
            (eq 'PyObject (foreign-pointer-type ,thing))
            (= 0 (foreign-pointer-address ,thing)))))

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
  (prog1 +pynull+
    (when (pyobject-p ob)
      (Py_DecRef ob)
      (setf (foreign-pointer-address ob) 0))))

(define-compiler-macro pydecref (ob)
  `(prog1 +pynull+
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
       (setf (foreign-pointer-address ,ob-var) 0))))

;;; Conditions
(define-condition pycl-condition (condition)
  ())

;;; Utilities
(declaim (ftype (function (pyobject) (unsigned-byte #+32bit 32 #+64bit 64)) pyunicode-to-native))
(defun pyunicode-to-native (ob)
  (declare (type pyobject ob)
           (optimize (speed 3) (safety 0) (space 0)))
  (if* (pynull ob)
     then 0
     else (let ((ob_bytes (PyUnicode_AsUTF8String ob)))
            (if* (pynull ob_bytes)
               then 0
               else (Py_DecRef ob_bytes)
                    (PyBytes_AsString ob_bytes)))))
