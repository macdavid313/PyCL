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

(def-foreign-call (from-pyunicode! "PyUnicode_AsUTF8") ((o (* PyObject)))
  :returning ((* :char) simple-string)
  :arg-checking t
  :strings-convert t
  :allow-gc :always)

;;; Conditions
(define-condition pycl-condition (condition)
  ())

(define-condition simple-pycl-error (pycl-condition simple-error)
  ((msg :initarg :msg :initform "" :type simple-string)))

(defmethod print-object ((err simple-pycl-error) stream)
  (print-unreadable-object (err stream :type t :identity t)
    (format stream "~%  ")
    (with-slots (msg) err
      (write-line msg stream))))

;;; Utilities
(defun pyimport! (module)
  (declare (type simple-string module))
  (with-native-string (str module :external-format :utf-8)
    (the pyobject (PyImport_ImportModule str))))

(defun pyhasattr! (ob attr)
  (declare (type pyobject ob)
           (type simple-string attr))
  (with-native-string (str attr :external-format :utf-8)
    (= 1 (PyObject_HasAttrString ob str))))

(defun pyattr! (ob attr)
  (declare (type pyobject ob)
           (type simple-string attr))
  (with-native-string (str attr :external-format :utf-8)
    (values (PyObject_GetAttrString ob str)
            (= 1 (PyObject_HasAttrString ob str)))))

(defun (setf pyattr!) (new ob attr)
  (declare (type pyobject ob new)
           (type simple-string attr))
  (with-native-string (str attr :external-format :utf-8)
    (if* (and (pynull new) (pyhasattr! ob attr))
       then (PyObject_DelAttrString ob str)
       else (PyObject_SetAttrString ob str new))))

;;; Python and its GIL
(defvar-nonbindable *python* nil
  "The default python interpreter instance.")

(defstruct (python (:print-function print-python-struct))
  (exe            "" :type simple-string :read-only t)
  (version        "" :type simple-string :read-only t)
  (libpython      "" :type simple-string :read-only t)
  (home           "" :type simple-string :read-only t)
  ;; private slots start here ------------------------
  ;; Mapping between a symbol and the foreign address, e.g. 'PyExc_IOError ->
  ;; #x0000ffff
  (globalptr (make-hash-table :test 'eq :size #.(length +libpython-extern-variables+))
   :type hash-table :read-only t)
  ;; Mapping between a foreign address and the corresponding name as a symbol,
  ;; e.g. #x0000ffff -> 'PyExc_IOError
  (inv-globalptr (make-hash-table :test '= :size #.(length +libpython-extern-variables+))
   :type hash-table :read-only t)
  ;; SMP only slots start here ---
  ;; gc process
  #+smp (gc-process nil :type mp:process)
  ;; gc process gate
  #+smp (gc-process-gate (mp:make-gate nil) :read-only t)
  ;; gc invoking process gate
  #+smp (gc-invoking-process-gate nil)
  ;; garbage queue
  #+smp (gc-queue (make-instance 'mp:queue :name "Python Garbage Queue")
         :type mp:queue :read-only t))

(defun print-python-struct (py stream depth)
  (declare (ignore depth))
  (with-slots (exe version libpython home) py
    (print-unreadable-object (py stream :type t :identity t)
      (terpri stream)
      (with-stack-list (lines (string+ "  exe: "       #\" exe #\" )
                              (string+ "  version: "   #\"  version #\")
                              (string+ "  libpython: " #\" libpython #\")
                              (string+ "  home: "      #\" home #\"))
        (format stream "~{~a~^~%~}~%" lines)))))

(defun pyglobalptr (symbol-or-address)
  (let ((k symbol-or-address))
    (typecase symbol-or-address
      (symbol (gethash k (python-globalptr *python*)))
      ((unsigned-byte #+32bit 32 #+64bit 64) (gethash k (python-inv-globalptr *python*)))
      (foreign-pointer (gethash (foreign-pointer-address k) (python-inv-globalptr *python*)))
      (t nil))))

(defun (setf pyglobalptr) (new-addr sym)
  (when (and (symbolp sym)
             (typep new-addr '(unsigned-byte #+32bit 32 #+64bit 64)))
    (multiple-value-bind (old-addr exists-p)
        (gethash sym (python-globalptr *python*))
      (when exists-p
        (warn "Redefining python global pointer '~a from ~s to ~s" sym old-addr new-addr)
        (remhash old-addr (python-inv-globalptr *python*))))
    (setf (gethash sym (python-globalptr *python*)) new-addr
          (gethash new-addr (python-inv-globalptr *python*)) sym)))

#-smp
(defmacro with-python-gil ((&key (unwind-protect t)) &body body)
  (declare (ignore unwind-protect))
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
                 (PyGILState_Release ,g))))))

;;; pyobject
;;; APIs and Utilities
(defun pyptr-eq (x y)
  (and (typep x 'foreign-python-pointer)
       (typep y 'foreign-python-pointer)
       (eq (foreign-pointer-type x) (foreign-pointer-type y))
       (= (foreign-pointer-address x)
          (foreign-pointer-address y))))

(defun pynull (thing)
  (or (eq thing *pynull*)
      (and (typep thing 'pyptr)
           (= 0 (foreign-pointer-address thing)))))

(defun pyincref (ob)
  (declare (type pyobject ob))
  (when (typep ob 'pyobject)
    (Py_IncRef ob)
    ob))

(defun pydecref (ob)
  (declare (type pyobject ob))
  (when (typep ob 'pyobject)
    (Py_DecRef ob)
    (setf (foreign-pointer-address ob) 0))
  ob)

(defun pydecref* (&rest obs)
  (dolist (ob obs)
    (when (typep ob 'pyobject)
      (Py_DecRef ob)
      (setf (foreign-pointer-address ob) 0))))

(defun pystealref (ob)
  "The caller (thief) will take the ownership so you are NOT responsible anymore.
This macro should always be used \"in place\" e.g. (PyList_SetItem ob_list idx (pystealref ob_item))"
  (declare (type pyobject ob))
  (if* (pynull ob)
     then *pynull*
     else (prog1 (foreign-pointer-address ob)
            (setf (foreign-pointer-address ob) 0))))

#+smp
(defun pymarkgc (ob)
  (when (typep ob 'pyobject)
    (schedule-finalization ob 'pydecref :queue (python-gc-queue *python*)))
  ob)

#+smp
(defun pygc (&key threshold)
  (check-type *python* python)
  (when (null (python-gc-process *python*))
    (start-python-gc-process))
  (flet ((run-gc ()
           (let ((saved (PyEval_SaveThread)))
             (unwind-protect
                  (progn (setf (python-gc-invoking-process-gate *python*) (mp:make-gate nil))
                         (mp:open-gate (python-gc-process-gate *python*))
                         (mp:process-wait "wait for python gc finish"
                                          'mp:gate-open-p (python-gc-invoking-process-gate *python*)))
               (PyEval_RestoreThread saved)))))
    (if* (typep threshold '(integer 0 *))
       then (when (>= (mp:queue-length (python-gc-queue *python*))
                      threshold)
              (run-gc))
       else (run-gc)))
  (mp:queue-length (python-gc-queue *python*)))

#+smp
(defun start-python-gc-process ()
  (flet ((process ()
           (loop
             (mp:process-wait "waiting for (pygc) is called"
                              #'mp:gate-open-p (python-gc-process-gate *python*))
             (let ((queue (python-gc-queue *python*)))
               (when (not (mp:queue-empty-p queue))
                 (with-python-gil (:unwind-protect nil)
                   (loop for finalization = (mp:dequeue queue :wait nil)
                         until (mp:queue-empty-p queue)
                         do (call-finalizer finalization)))))
             (mp:close-gate (python-gc-process-gate *python*))
             (mp:open-gate (python-gc-invoking-process-gate *python*)))))
    (setf (python-gc-process *python*)
          (mp:process-run-function "python-gc-process" #'process))))

;;; python exception
(define-condition python-exception (simple-pycl-error)
  ((type :initarg :type :initform nil :accessor python-exception-type :type (or null symbol))
   (msg :initarg :msg :initform "" :accessor python-exception-msg :type simple-string))
  (:documentation "A simple condition that represents a python exception. User is responsible to
construct the \"msg\"."))

(defmethod print-object ((exc python-exception) stream)
  (print-unreadable-object (exc stream :type t :identity t)
    (with-slots (msg) exc
      (format stream "- python exception caught: ~%~a" msg))))

(define-symbol-macro python-exception-occurred
    (not (pynull (PyErr_Occurred))))

(defun make-python-exception ()
  (if* python-exception-occurred
     then (with-static-fobjects ((ob_type* #1='(* PyObject) :allocation :c)
                                 (ob_value* #1# :allocation :c)
                                 (ob_traceback* #1# :allocation :c))
            (PyErr_Fetch ob_type* ob_value* ob_traceback*)
            (PyErr_NormalizeException ob_type* ob_value* ob_traceback*)
            (let ((ob_type (fslot-value-typed #1# :c ob_type*))
                  (ob_value (fslot-value-typed #1# :c ob_value*))
                  (ob_traceback (fslot-value-typed #1# :c ob_traceback*)))
              (prog1 (make-instance 'python-exception
                                    :type (pyglobalptr ob_type)
                                    :msg (format-python-exception ob_type ob_value ob_traceback))
                (PyErr_Clear))))
     else (make-instance 'simple-pycl-error
                          :msg "trying to catch a python exception but none has occurred")))

(defun format-python-exception (ob_type       ; stolen
                                ob_value      ; sotlen
                                ob_traceback) ; stolen
  (flet ((format-exception (ob)
           (with-output-to-string (out)
             (dotimes (idx (PyList_Size ob))
               (let (ob_unicode         ; borrowed
                     line)
                 (setq ob_unicode (PyList_GetItem ob idx))
                 (setq line (from-pyunicode! ob_unicode))
                 (write-string (string+ #\Space #\Space line)
                               out))))))
    (let* ((ob_module (pyimport! "traceback")) ; new
           (ob_formatter                       ; new
             (if* (= 0 ob_traceback)
                then (pyattr! ob_module "format_exception_only")
                else (pyattr! ob_module "format_exception")))
           (ob_tuple (PyTuple_New (if (= 0 ob_traceback) 2 3))) ; new
           ob_list)                                             ; new
      (if* (pynull ob_tuple)
         then (prog1 "None"
                (pydecref* ob_module ob_formatter ob_tuple))
         else (PyTuple_SetItem ob_tuple 0 ob_type)
              (PyTuple_SetItem ob_tuple 1 ob_value)
              (when (/= 0 ob_traceback)
                (PyTuple_SetItem ob_tuple 2 ob_traceback))
              (setq ob_list (PyObject_CallObject ob_formatter ob_tuple))
              (prog1 (format-exception ob_list)
                (pydecref* ob_module ob_formatter ob_tuple ob_list))))))

(defun pyexcept ()
  (make-python-exception))

(defun pyerror ()
  (error (pyexcept)))

(defun pycheckn (val)
  (if* (pynull val)
     then (values *pynull* (pyexcept))
     else (values val nil)))

(defun pycheckz (val)
  (if* (= -1 (minusp val))
     then (values *pynull* (pyexcept))
     else (values val nil)))
