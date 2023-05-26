;;;; conditions.cl
(in-package #:pycl)

(define-condition pycl-condition (condition)
  ()
  (:report report-pycl-condition))

(defgeneric report-pycl-condition (c stream)
  (:documentation "Report pycl and python related condition"))

(define-condition python-error (pycl-condition error)
  ((msg :initarg :msg :accessor python-error-msg :type simple-string)
   (type :initarg :type :reader python-error-type :type foreign-pointer)
   (val :initarg :val :reader python-error-val :type foreign-pointer)
   (traceback :initarg :traceback :reader python-error-traceback :type foreign-pointer)))

(defun make-python-error (msg &optional e)
  (declare (type (or nil python-error) e)
           (ignorable e))
  (if* e
     then (make-condition 'python-error :msg (string+ msg)
                                        :type (python-error-type e)
                                        :val (python-error-val e)
                                        :traceback (python-error-traceback e))
     else (with-static-fobjects ((ptype* #1='(* PyObject) :allocation :c)
                                 (pvalue* #1# :allocation :c)
                                 (ptraceback* #1# :allocation :c))
            (PyErr_Fetch ptype* pvalue* ptraceback*)
            (PyErr_NormalizeException ptype* pvalue* ptraceback*)
            (make-condition 'python-error :msg (string+ msg)
                                          :type (fslot-value-typed #1# :c ptype*)
                                          :val (fslot-value-typed #1# :c pvalue*)
                                          :traceback (fslot-value-typed #1# :c ptraceback*)))))

;; (defmethod report-pycl-condition ((e python-error) stream)
;;   (with-slots (msg type val traceback) c
;;     (print-unreadable-object (c stream :type t :identity nil)
;;       (when (not (zerop (length msg)))
;;         (write-string (string+ " (" msg ") ") stream))
;;       (if* (pynull-p type)
;;          then (write-string "None" stream)
;;          else (write-string (string+ (pystring type) #\Newline (pystring val) #\Newline)))
;;       (when (not (pynull-p traceback))
;;         ;; TODO: need pycall implementation?
;;         ))))

;; (define-condition pycl-error (pycl-condition error)
;;   (err)
;;   (:report report-pycl-condition))

;; (defmethod report-pycl-condition ((err pycl-error) stream)
;;   (print-unreadable-object (err stream :type t :identity nil)
;;     (write-string (string+ "(in a Lisp function called from Python)"
;;                            #\Newline
;;                            "Lisp: "
;;                            ;; TODO
;;                            ))))
