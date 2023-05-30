;;;; exception.cl
(in-package #:pycl)

(define-condition python-exception (pycl-condition error)
  ((type :initarg :type :accessor python-exception-type :type (or null symbol))
   (place :initarg :place :accessor python-exception-place :type symbol)
   (msg :initarg :msg :accessor python-exception-msg :type (or null simple-string)))
  (:documentation "A simple condition that represents a python exception. User is responsible to
construct the \"msg\"."))

(defmethod print-object ((exc python-exception) stream)
  (print-unreadable-object (exc stream :type t)
    (with-slots (type place msg) exc
      (when place
        (format stream "from (~a), caught python exception: ~%~a"
                place msg)))))

(defun pyerror (&optional place)
  (if* (pynull (PyErr_Occurred))
     then (error 'python-exception :type nil :place place :msg "None") ; will it happen at all?
     else (with-static-fobjects ((ob_type* #1='(* PyObject) :allocation :c)
                                 (ob_value* #1# :allocation :c)
                                 (ob_traceback* #1# :allocation :c))
            (PyErr_Fetch ob_type* ob_value* ob_traceback*)
            (PyErr_NormalizeException ob_type* ob_value* ob_traceback*)
            (let* ((ob_type (make-pyobject (fslot-value-typed #1# :c ob_type*)))
                   (ob_value (make-pyobject (fslot-value-typed #1# :c ob_value*)))
                   (ob_traceback (make-pyobject (fslot-value-typed #1# :c ob_traceback*)))
                   (exc (make-instance 'python-exception
                                       :type (pyglobalptr ob_type)
                                       :place place
                                       :msg (format-python-exception ob_type ob_value ob_traceback))))
              (PyErr_Clear)
              (error exc)))))

(defun format-python-exception (ob_type       ; stolen
                                ob_value      ; sotlen
                                ob_traceback) ; stolen
  (flet ((format-exception (ob)
           (with-output-to-string (out)
             (dotimes (idx (PyList_Size ob))
               (let (ob_unicode         ; borrowed
                     bytes)
                 (setq ob_unicode (PyList_GetItem ob idx))
                 (setq bytes (pyunicode-to-native ob_unicode))
                 (when (/= 0 bytes)
                   (write-string (string+ #\Space #\Space
                                          (native-to-string bytes :external-format :utf-8))
                                 out)))))))
    (let (ob_formatter                  ; borrowed
          ob_tuple                      ; new
          ob_list)                      ; new
      (if* (pynull ob_traceback)
         then (setq ob_formatter (pyglobalptr '%python-callable/traceback.format_exception_only%))
              (setq ob_tuple (PyTuple_New 2))
         else (setq ob_formatter (pyglobalptr '%python-callable/traceback.format_exception%))
              (setq ob_tuple (PyTuple_New 3)))
      (if* (pynull ob_tuple)
         then "None"
         else (PyTuple_SetItem ob_tuple 0 (pystealref ob_type))
              (PyTuple_SetItem ob_tuple 1 (pystealref ob_value))
              (when (not (pynull ob_traceback))
                (PyTuple_SetItem ob_tuple 2 (pystealref ob_traceback)))
              (setq ob_list (PyObject_CallObject ob_formatter ob_tuple))
              (prog1 (format-exception ob_list)
                (pydecref* ob_tuple ob_list))))))

(defun pycheck (res checker place)
  (declare (ftype (function (thing) (or t nil)) checker)
           (type symbol place))
  (if* (funcall checker res)
     then res
     else (pyerror place)))

(defmacro pycheckn (form &optional place)
  (let ((res (gensym "res"))
        (place (if place place (car form))))
    `(let ((,res ,form))
       (pycheck ,res #.(complement 'pynull) ',place))))

(defmacro pycheckz (form &optional place)
  (let ((res (gensym "res"))
        (place (if place place (car form))))
    `(let ((,res ,form))
       (pycheck ,res #.(complement 'minusp) ',place))))
