;;;; conditions.cl
(in-package #:pycl)

(define-condition pycl-condition (condition)
  ()
  (:report report-pycl-condition))

(defgeneric report-pycl-condition (c stream)
  (:documentation "Report pycl and python related condition"))

(define-condition simple-python-error (pycl-condition simple-error)
  ())

(define-condition python-error (pycl-condition error)
  ((msg :initarg :msg :accessor python-error-msg :type simple-string)
   (type :initarg :type :reader python-error-type :type foreign-pointer)
   (value :initarg :value :reader python-error-value :type foreign-pointer)
   (traceback :initarg :traceback :reader python-error-traceback :type foreign-pointer)))

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
