;;;; pyerror.cl
(in-package #:pycl)

(define-condition python-callback-error (error)
  (err)
  (:report (lambda (c stream)
             (print-unreadable-object (c stream :type t :identity nil)
               (write-string (string+ "(in a Lisp function called from Python)"
                                      #\Newline
                                      "Lisp: "
                                      ;; TODO
                                      ))))))

(define-condition python-error (error)
  ((msg :initarg :msg :accessor python-error-msg :type simple-string)
   (type :initarg :type :reader python-error-type :type pyobject)
   (val :initarg :val :reader python-error-val :type pyobject)
   (traceback :initarg :traceback :reader python-error-traceback :type pyobject))
  (:report (lambda (c stream)
             (with-slots (msg type val traceback) c
               (print-unreadable-object (c stream :type t :identity nil)
                 (when (not (zerop (length msg)))
                   (write-string (string+ " (" msg ") ") stream))
                 (if* (pynull-p type)
                    then (write-string "None" stream)
                    else (write-string (string+ (pystring type) #\Newline (pystring val) #\Newline)))
                 (when (not (pynull-p traceback))
                   ;; TODO: need pycall implementation?
                   ))))))

(def-foreign-call (%PyErr_Fetch "PyErr_Fetch") ((ptype (* (* PyObject)))
                                                (pvalue (* (* PyObject)))
                                                (ptraceback (* (* PyObject))))
  :returning :void
  :call-direct t
  :strings-convert nil)

(def-foreign-call (%PyErr_NormalizeException "PyErr_NormalizeException") ((exec (* (* PyObject)))
                                                                          (val (* (* PyObject)))
                                                                          (tb (* (* PyObject))))
  :returning :void
  :call-direct t
  :strings-convert nil)

(defun make-python-error (msg &optional e)
  (declare (type (or nil python-error) e)
           (ignorable e))
  (if* e
     then (make-condition 'python-error :msg (string+ msg)
                                        :type (python-error-type e)
                                        :val (python-error-val e)
                                        :traceback (python-error-traceback e))
     else (with-stack-fobjects ((ptype* #1='(* PyObject))
                                (pvalue* #1#)
                                (ptraceback* #1#))
            (%PyErr_Fetch ptype* pvalue* ptraceback*)
            (%PyErr_NormalizeException ptype* pvalue* ptraceback*)
            (make-condition 'python-error :msg (string+ msg)
                                          :type (to-pyobject (make-pyptr (fslot-value-typed #1# :foreign ptype*)))
                                          :val (to-pyobject (make-pyptr (fslot-value-typed #1# :foreign pvalue*)))
                                          :traceback (to-pyobject (make-pyptr (fslot-value-typed #1# :foreign ptraceback*)))))))

(def-foreign-call PyErr_Occurred ()
  :returning :foreign-address
  :strings-convert nil)

(defun pyerr-occurred-p ()
  (not (zerop (PyErr_Occurred))))

(def-foreign-call (pyerr-clear "PyErr_Clear") ()
  :returning :void
  :strings-convert nil)

(defun pyerr-check (&key (msg "") val)
  (when (pyerr-occurred-p)
    (%pyerror msg))
  val)

(def-foreign-pycall PyErr_Restore ((type (* PyObject))
                                   (value (* Pyobject))
                                   (traceback (* PyObject)))
  :returning :void
  :call-direct t
  :strings-convert nil)

(def-foreign-pycall PySequence_Size ((o (* PyObject)))
  :returning Py_ssize_t
  :call-direct t
  :strings-convert nil)

(def-foreign-pycall PySequence_GetItem ((o (* PyObject))
                                        (i Py_ssize_t))
  :returning ((* PyObject))
  :call-direct t
  :strings-convert nil)

(defun pylen (o)
  (let ((len (PySequence_Size o)))
    (when (= len -1)
      (error (make-python-error 'PySequence_Size)))))

(defun %pyerror (msg &key e ptype pvalue ptraceback)
  (if* (typep e 'python-error)
     then (%pyerror msg :ptype (python-error-type e)
                        :pvalue (python-error-val e)
                        :ptraceback (python-error-traceback e))
     else (let ((pargs (pyattr pvalue "args"))
                arg)
            (if* (not (pynull-p pargs))
               then (when (plusp (pylen pargs))
                      (setq arg (PySequence_GetItem arg 0))
                      ;; TODO: pyclwrap?
                      arg)
               else (make-condition 'python-error :msg msg
                                                  :type ptype
                                                  :value pvalue
                                                  :traceback ptraceback)))))
