;;;; conditions.cl
(in-package #:pycl)

(define-condition pycl-condition (condition)
  ()
  (:report report-pycl-condition))

(defgeneric report-pycl-condition (c stream)
  (:documentation "Report pycl and python related condition"))

(define-condition python-exception (pycl-condition error)
  ((place :initarg :place :accessor python-exception-place :type symbol)
   (msg :initarg :msg :accessor python-exception-msg :type (or null simple-string))))

(defmethod report-pycl-condition ((exc python-exception) stream)
  (with-slots (place msg) exc
    (if* msg
       then (format stream "from (~a), caught python exception: ~%~a"
                           place msg)
       else (format stream "from (~a), a python exception has occurred"
                           place))))

(defun pyexcept (place)
  (if* (null (PyErr_Occurred))
     then (make-instance 'python-exception :place place)
     else (with-stack-fobjects ((ob_type* #1='(* PyObject))
                                (ob_value* #1#)
                                (ob_traceback* #1#))
            (PyErr_Fetch ob_type* ob_value* ob_traceback*)
            (PyErr_NormalizeException ob_type* ob_value* ob_traceback*)
            (let ((ob_type (@pyobject (fslot-value-typed #1# :foreign ob_type*)))
                  (ob_value (@pyobject (fslot-value-typed #1# :foreign ob_value*)))
                  (ob_traceback (@pyobject (fslot-value-typed #1# :foreign ob_traceback*))))
              (prog1 (make-instance 'python-exception
                                    :place place
                                    :msg (format-python-exception ob_type ob_value ob_traceback))
                (PyErr_Clear))))))

(defun format-python-exception (ob_type ob_value ob_traceback)
  (flet ((format-exception (ob)
           (let (lines)
             (dotimes (idx (PyList_Size ob))
               (let (ob_unicode         ; borrowed
                     ob_bytes           ; new
                     bytes)
                 (setq ob_unicode (PyList_GetItem ob idx))
                 (when (not (null ob_unicode))
                   (setq ob_bytes (PyUnicode_AsUTF8String ob_unicode))
                   (when (not (null ob_bytes))
                     (setq bytes (PyBytes_AsString ob_bytes))
                     (when (/= 0 bytes)
                       (push (native-to-string bytes) lines))))
                 (pydecref* ob_bytes)))
             (nreverse lines))))
    (with-output-to-string (stream)
      (let (ob_formatter                ; new
            ob_tuple                    ; new
            ob_list)                    ; new
        (if* (null ob_traceback)
           then (setq ob_formatter (%load-format-exception-only-func))
                (setq ob_tuple (PyTuple_New 2))
           else (setq ob_formatter (%load-format-exception-func))
                (setq ob_tuple (PyTuple_New 3)))
        (PyTuple_SetItem ob_tuple 0 (pystealref ob_type))
        (PyTuple_SetItem ob_tuple 1 (pystealref ob_value))
        (when (not (null ob_traceback))
          (PyTuple_SetItem ob_tuple 2 (pystealref ob_traceback)))
        (setq ob_list (PyObject_CallObject ob_formatter ob_tuple))
        (prog1 (dolist (line (format-exception ob_list))
                 (write-string line stream))
          (pydecref* ob_formatter ob_tuple ob_list))))))

(defun pycheckv (res checker place)
  (declare (ftype (function (thing) (or t nil)) checker)
           (type symbol place))
  (if* (funcall checker res)
     then res
     else (pyexcept place)))

(defmacro pycheckn (form &optional place)
  (let ((res (gensym "res"))
        (place (if place place (car form))))
    `(let ((,res ,form))
       (pycheckv ,res #.(complement 'null) ',place))))

(defmacro pycheckz (form &optional place)
  (let ((res (gensym "res"))
        (place (if place place (car form))))
    `(let ((,res ,form))
       (pycheckv ,res #.(complement 'minusp) ',place))))

(defun %load-format-exception-func ()
  (let (ob_module
        ob_format_exception)
    ;; traceback module
    (with-native-string (module "traceback" :external-format :utf-8)
      (setq ob_module (PyImport_ImportModule module))
      (when (null ob_module)
        (error 'start-python-error :reason "Cannot load 'traceback' module")))
    ;; traceback.format_exception
    (with-native-string (str "format_exception" :external-format :utf-8)
      (setq ob_format_exception (PyObject_GetAttrString ob_module str))
      (when (null ob_format_exception)
        (pydecref ob_module)
        (error 'start-python-error :reason "Cannot load 'format_exception' function from traceback module")))
    ;; clean up and return
    (prog1 ob_format_exception
      (pydecref ob_module))))

(defun %load-format-exception-only-func ()
  (let (ob_module
        ob_format_exception_only)
    ;; traceback module
    (with-native-string (module "traceback" :external-format :utf-8)
      (setq ob_module (PyImport_ImportModule module))
      (when (null ob_module)
        (error 'start-python-error :reason "Cannot load 'traceback' module")))
    ;; traceback.format_exception_only
    (with-native-string (str "format_exception_only" :external-format :utf-8)
      (setq ob_format_exception_only (PyObject_GetAttrString ob_module str))
      (when (null ob_format_exception_only)
        (pydecref ob_module)
        (error 'start-python-error :reason "Cannot load 'format_exception_only' function from traceback module")))
    ;; clean up and return
    (prog1 ob_format_exception_only
      (pydecref ob_module))))
