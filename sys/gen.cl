;;;; gen.cl
(in-package #:cl-user)

(use-package :util.string)

(defun collect-global-pointers ()
  (let (collected)
    (dolist (form *manifest-forms*)
      (when (eq 'extern (car form))
        (destructuring-bind (_ sym ftype) form
          (when (and (match-re "^Py" (symbol-name sym) :case-fold nil)
                     (listp ftype)
                     (eq (car ftype) :pointer))
            (push (symbol-name sym) collected)))))
    collected))

(defun translate-foreign-type (x)
  (etypecase x
    (keyword (case x
               ((:function-pointer :pointer) :foreign-address)
               (:char '(:char fixnum))
               (t x)))
    (symbol (case x
              (size_t :unsigned-nat)
              (t x)))
    (list (case (first x)
            (:pointer :foreign-address)
            (t (error "don't know how to translate compound type ~s" x))))))

(defun has-variadic-args-p (name))

(defun translate-foreign-function (form)
  (flet ((to-translate-p ()
           (destructuring-bind (_ name params return-type &optional variadic-p) form
             (and (not variadic-p)
                  (match-re "^Py" name :case-fold nil)
                  (not (find name '("PyBytes_FromFormatV" "PyUnicode_FromFormatV" "PyErr_FormatV" "PyOS_vsnprintf")
                             :test 'string=)))))
         (translate-param (param)
           (when (= 1 (length param))
             (setq param (cons (gensym "arg") param)))
           (let ((ftype (translate-foreign-type (second param))))
             (if* (listp ftype)
                then (cons (first param) ftype)
                else (list (first param) ftype)))))
    (when (to-translate-p)
      (destructuring-bind (_ name params return-type &optional variadic-p) form
        `(ff:def-foreign-call ,(intern name) ,(if params (mapcar #'translate-param params) '(:void))
           :strings-convert nil
           :returning ,(translate-foreign-type return-type)
           :allow-gc :always
           :call-direct ,(not (null params))
           :arg-checking nil)))))

(defun write-form (form)
  (when form
    (pprint form *standard-output*)
    (terpri *standard-output*)))

(defun main ()
  ;; header (comments and package declarations)
  (format *standard-output* ";;;; capi.cl~%")
  (format *standard-output* ";;;; this file is automatically generated, DO NOT modify~%")
  (dolist (form '((in-package #:pycl.sys)
                  (eval-when (:compile-toplevel)
                    (declaim (optimize speed (safety 0) (space 0))))))
    (write-form form))
  ;; foreign pointers
  ;; they should only be initialized after libpython has been loaded
  ;; as in (def-foreign-variable ...) or (def-foreign-constant ...)
  (write-form `(defconstant +libpython-foreign-pointers+
                 (list ,@(collect-global-pointers))))
  ;; foreign functions
  (dolist (form *manifest-forms*)
    (write-form
     (when (eq 'function (car form))
       (translate-foreign-function form))))
  ;; exports
  (write-form '(eval-when (:compile-toplevel :load-toplevel :execute)
                (do-symbols (sym)
                  (when (match-re "^Py" (symbol-name sym) :case-fold nil)
                    (export sym))))))

(eval-when (:load-toplevel :execute)
  (defparameter *manifest-forms*
    (with-open-file (in (string+ (directory-namestring *load-pathname*) "libpython.binding.sexp"))
      (do ((all-forms)
           (form (read in nil nil) (read in nil nil)))
          ((null form) (nreverse all-forms))
        (push form all-forms))))
  (main))
