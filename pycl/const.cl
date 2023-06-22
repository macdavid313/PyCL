;;;; const.cl
;;;; Constants and extra foreign definitions
(in-package #:pycl)

(defconstant +Py_single_input+    256)
(defconstant +Py_file_input+      257)
(defconstant +Py_eval_input+      258)
(defconstant +Py_func_type_input+ 345)

(defconstant +Py_LT+ 0)
(defconstant +Py_LE+ 1)
(defconstant +Py_EQ+ 2)
(defconstant +Py_NE+ 3)
(defconstant +Py_GT+ 4)
(defconstant +Py_GE+ 5)

(defconstant +Py_METH_VARARGS+  #x0001)
(defconstant +Py_METH_KEYWORDS+ #x0002)
(defconstant +Py_METH_NOARGS+   #x0004)
(defconstant +Py_METH_O+        #x0008)
(defconstant +Py_METH_CLASS+    #x0010)
(defconstant +Py_METH_STATIC+   #x0020)

(def-foreign-type PyNumberMethods
    (:struct
     (add (* :void))
     (subtract (* :void))
     (multiply (* :void))
     (remainder (* :void))
     (divmod (* :void))
     (power (* :void))
     (negative (* :void))
     (positive (* :void))
     (absolute (* :void))
     (bool (* :void))
     (invert (* :void))
     (lshift (* :void))
     (rshift (* :void))
     (and (* :void))
     (xor (* :void))
     (or (* :void))
     (int (* :void))
     (%reserved (* :void))
     (float (* :void))
     (inplace-add (* :void))
     (inplace-subtract (* :void))
     (inplace-multiply (* :void))
     (inplace-remainder (* :void))
     (inplace-power (* :void))
     (inplace-lshift (* :void))
     (inplace-rshift (* :void))
     (inplace-and (* :void))
     (inplace-xor (* :void))
     (inplace-or (* :void))
     (floordivide (* :void))
     (truedivide (* :void))
     (inplace-floordivide (* :void))
     (inplace-truedivide (* :void))
     (index (* :void))
     (matrixmultiply (* :void))
     (inplace-matrixmultiply (* :void))))

(def-foreign-type PySequenceMethods
    (:struct
     (length (* :void))
     (concat (* :void))
     (repeat (* :void))
     (item (* :void))
     (%was-item (* :void))
     (ass-item (* :void))
     (%was-ass-slice (* :void))
     (contains (* :void))
     (inplace-concat (* :void))
     (inplace-repeat (* :void))))

(def-foreign-type PyMappingMethods
    (:struct
     (length (* :void))
     (subscript (* :void))
     (ass-subscript (* :void))))

(def-foreign-type PyBufferProcs
    (:struct
     (get (* :void))
     (release (* :void))))

(def-foreign-type Py_buffer
    (:struct
     (buf (* :void))
     (obj (* :void))
     (len Py_ssize_t)
     (itemsize Py_ssize_t)
     (ndim :int)
     (format (* :char))
     (shape (* Py_ssize_t))
     (strides (* Py_ssize_t))
     (suboffsets (* Py_ssize_t))
     (internal (* :void))))

(def-foreign-type PyMemoryViewObject
    (:struct
     (ob-base PyVarObject)
     (mbuf (* PyObject))
     (hash Py_hash_t)
     (flags :int)
     (exports Py_ssize_t)
     (view Py_buffer)
     (weakreflist (* PyObject))))

(def-foreign-type PyTypeObject
    ;; https://docs.python.org/3/c-api/typeobj.html#pytypeobject-definition
    (:struct
     (ob_base PyVarObject)
     (tp_name (* :char))
     (tp_basicsize Py_ssize_t)
     (tp_itemsize Py_ssize_t)
     ;; Methods to implement standard operations
     (tp_dealloc (* :void))
     (tp_vectorcall-offset Py_ssize_t)
     (tp_getattr (* :void))
     (tp_setattr (* :void))
     (tp_as_async (* :void))
     (tp_repr (* :void))
     ;; Method suites for standard classes
     (tp_as_number (* PyNumberMethods))
     (tp_as_sequence (* PySequenceMe))
     (tp_as_mapping (* PyMappingMethods))
     ;; More standard operations (here for binary compatibility)
     (tp_hash (* :void))
     (tp_call (* :void))
     (tp_str (* :void))
     (tp_getattro (* :void))
     (tp_setattro (* :void))
     ;; Functions to access object as input/output buffer
     (tp_as_buffer (* PyBufferProcs))
     ;; Flags to define presence of optional/expanded features
     (tp_flags :unsigned-long)
     (tp_doc (* :char))              ; Documentation string
     ;; Assigned meaning in release 2.0
     ;; call function for all accessible objects
     (tp_traverse (* :void))
     ;; delete references to contained objects
     (tp_clear (* :void))
     ;; Assigned meaning in release 2.1
     ;; rich comparisons
     (tp_richcompare (* :void))
     ;; weak reference enabler
     (tp_weaklistoffset Py_ssize_t)
     ;; Iterators
     (tp_iter (* :void))
     (tp_iternext (* :void))
     ;; Attribute descriptor and subclassing stuff
     (tp_methods (* PyMethodDef))
     (tp_members (* PyMemberDef))
     (tp_getset (* PyGetSetDef))
     ;; Strong reference on a heap type, borrowed reference on a static type
     (tp_base (* PyTypeObject))
     (tp_dict (* PyObject))
     (tp_descr_get (* :void))
     (tp_descr_set (* :void))
     (tp_dictoffset Py_ssize_t)
     (tp_init (* :void))
     (tp_alloc (* :void))
     (tp_new (* :void))
     (tp_free (* :void))                ; Low-level free-memory routine
     (tp_is_gc (* :void))               ; For PyObject_IS_GC
     (tp_bases (* PyObject))
     (tp_mro (* PyObject))              ; method resolution order
     (tp_cache (* PyObject))
     (tp_subclasses (* PyObject))
     (tp_weaklist (* PyObject))
     (tp_del (* :void))
     ;; Type attribute cache version tag. Added in version 2.6
     (tp_version_tag :unsigned-int)
     (tp_finalize (* :void))
     (tp_vectorcall (* :void))))
