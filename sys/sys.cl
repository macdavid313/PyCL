;;;; sys.cl
(in-package :pycl.sys)

;;; "lost and found"
;;; these Python C APIS are not generated because they are not
;;; considered to be "stable"
(def-foreign-call PyObject_DelAttrString ((o (* PyObject)) (attr :foreign-address))
  :returning :int
  :arg-checking nil
  :call-direct t
  :allow-gc :always)

(def-foreign-call (check-python-gil "PyGILState_Check") (:void)
  :returning :boolean
  :arg-checking nil
  :call-direct t
  :allow-gc :always)

(def-foreign-call PyRun_SimpleString ((command :foreign-address))
  :returning :int
  :arg-checking nil
  :call-direct t
  :allow-gc :always)

(def-foreign-call PyUnicode_AsUTF8 ((o (* PyObject)))
  :returning :foreign-address
  :arg-checking nil
  :call-direct t
  :allow-gc :always)

;;; Python and its GIL
(defstruct python
  (exe           "" :type simple-string :read-only t)
  (version       "" :type simple-string :read-only t)
  (libpython     "" :type simple-string :read-only t)
  (home          "" :type simple-string :read-only t)
  #+smp (lock   nil :type (or null mp:process-lock)))

(defmethod print-object ((py python) stream)
  (with-slots (exe version libpython home) py
    (print-unreadable-object (py stream :type t :identity t)
      (terpri stream)
      (with-stack-list (lines (string+ "  exe: "       #\" exe #\" )
                              (string+ "  version: "   #\"  version #\")
                              (string+ "  libpython: " #\" libpython #\")
                              (string+ "  home: "      #\" home #\"))
        (format stream "~{~a~^~%~}~%" lines)))))

(defvar *python* nil "The default python interpreter instance.")

#-smp
(defmacro with-python-gil ((&key safe) &body body)
  (declare (ignore safe))
  `(progn ,@body))

#+smp
(defmacro with-python-gil ((&key (unwind-protect t)) &body body)
  (let ((g (gensym "g"))
        (res (gensym "res")))
    (if* unwind-protect
       then `(mp:with-process-lock ((python-lock *python*))
               (let ((,g (PyGILState_Ensure))
                     ,res)
                 (declare (type (mod 1) ,g))
                 (setq ,res (progn ,@body))
                 (PyGILState_Release ,g)
                 ,res))
       else `(mp:with-process-lock ((python-lock *python*))
               (let ((,g (PyGILState_Ensure)))
                 (unwind-protect (progn ,@body)
                   (PyGILState_Release ,g)))))))

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

(defun pyobject-pointer-p (thing)
  (and (typep thing 'pyptr)
       (eq 'PyObject (foreign-pointer-type thing))))

(define-compiler-macro pyobject-pointer-p (thing)
  `(and (typep ,thing 'pyptr)
        (eq 'PyObject (foreign-pointer-type ,thing))))

(deftype pyobject-pointer ()
  '(satisfies pyobject-pointer-p))

(defun pyobject-p (thing)
  "Test if thing is a VALID (non-null) python foreign pointer."
  (and (not (eq thing +pynull+))
       (pyobject-pointer-p thing)
       (/= 0 (foreign-pointer-address thing))))

(define-compiler-macro pyobject-p (thing)
  `(and (not (eq ,thing +pynull+))
        (pyobject-pointer-p ,thing)
        (/= 0 (foreign-pointer-address ,thing))))

(deftype pyobject ()
  '(satisfies pyobject-p))

(defun pynull (thing)
  (or (eq thing +pynull+)
      (and (pyobject-pointer-p thing)
           (= 0 (foreign-pointer-address thing)))))

(define-compiler-macro pynull (thing)
  `(or (eq ,thing +pynull+)
       (and (pyobject-pointer-p ,thing)
            (= 0 (foreign-pointer-address ,thing)))))

(defun pyobject-eq (x y)
  (and (pyobject-pointer-p x)
       (pyobject-pointer-p y)
       (= (foreign-pointer-address x)
          (foreign-pointer-address y))))

(define-compiler-macro pyobject-eq (x y)
  `(and (pyobject-pointer-p ,x)
        (pyobject-pointer-p ,y)
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

(defun pystealref (ob)
  "The caller (thief) will take the ownership so you are NOT responsible anymore.
This macro should always be used \"in place\" e.g. (PyList_SetItem ob_list idx (pystealref ob_item))"
  (cond ((pyobject-p ob)
         (prog1 (foreign-pointer-address ob)
           (setf (foreign-pointer-address ob) 0)))
        ((pynull ob)
         +pynull+)
        (t nil)))

;;; Conditions
(define-condition pycl-condition (condition)
  ())
