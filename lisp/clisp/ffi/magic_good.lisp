;; This file was automatically generated by SWIG (http://www.swig.org).
;; Version 1.3.39
;;
;; Do not make changes to this file unless you know what you are doing--modify
;; the SWIG interface file instead.

(defpackage :magic
  (:use :common-lisp :ffi)
  (:export
	:MAGIC_NONE
	:MAGIC_DEBUG
	:MAGIC_SYMLINK
	:MAGIC_COMPRESS
	:MAGIC_DEVICES
	:MAGIC_MIME_TYPE
	:MAGIC_CONTINUE
	:MAGIC_CHECK
	:MAGIC_PRESERVE_ATIME
	:MAGIC_RAW
	:MAGIC_ERROR
	:MAGIC_MIME_ENCODING
	:MAGIC_MIME
	:MAGIC_APPLE
	:MAGIC_NO_CHECK_COMPRESS
	:MAGIC_NO_CHECK_TAR
	:MAGIC_NO_CHECK_SOFT
	:MAGIC_NO_CHECK_APPTYPE
	:MAGIC_NO_CHECK_ELF
	:MAGIC_NO_CHECK_TEXT
	:MAGIC_NO_CHECK_CDF
	:MAGIC_NO_CHECK_TOKENS
	:MAGIC_NO_CHECK_ENCODING
	:MAGIC_NO_CHECK_ASCII
	:MAGIC_NO_CHECK_FORTRAN
	:MAGIC_NO_CHECK_TROFF
	:magic_open
	:magic_close
	:magic_file
	:magic_descriptor
	:magic_buffer
	:magic_error
	:magic_setflags
	:magic_load
	:magic_compile
	:magic_check
	:magic_errno))

(in-package :magic)

(default-foreign-language :stdc)

(defconstant MAGIC_NONE #x000000)

(defconstant MAGIC_DEBUG #x000001)

(defconstant MAGIC_SYMLINK #x000002)

(defconstant MAGIC_COMPRESS #x000004)

(defconstant MAGIC_DEVICES #x000008)

(defconstant MAGIC_MIME_TYPE #x000010)

(defconstant MAGIC_CONTINUE #x000020)

(defconstant MAGIC_CHECK #x000040)

(defconstant MAGIC_PRESERVE_ATIME #x000080)

(defconstant MAGIC_RAW #x000100)

(defconstant MAGIC_ERROR #x000200)

(defconstant MAGIC_MIME_ENCODING #x000400)

(defconstant MAGIC_MIME (and #x000010 #x000400))

(defconstant MAGIC_APPLE #x000800)

(defconstant MAGIC_NO_CHECK_COMPRESS #x001000)

(defconstant MAGIC_NO_CHECK_TAR #x002000)

(defconstant MAGIC_NO_CHECK_SOFT #x004000)

(defconstant MAGIC_NO_CHECK_APPTYPE #x008000)

(defconstant MAGIC_NO_CHECK_ELF #x010000)

(defconstant MAGIC_NO_CHECK_TEXT #x020000)

(defconstant MAGIC_NO_CHECK_CDF #x040000)

(defconstant MAGIC_NO_CHECK_TOKENS #x100000)

(defconstant MAGIC_NO_CHECK_ENCODING #x200000)

(defconstant MAGIC_NO_CHECK_ASCII #x020000)

(defconstant MAGIC_NO_CHECK_FORTRAN #x000000)

(defconstant MAGIC_NO_CHECK_TROFF #x000000)

(ffi:def-c-type magic_t ffi:c-pointer)

(defvar magic_library "/usr/lib/libmagic.so") 
(ffi:def-call-out magic_open
	(:name "magic_open")
	(:arguments (arg0 ffi:int))
	(:return-type magic_t)
	(:library "/usr/lib/libmagic.so"))

(ffi:def-call-out magic_close
	(:name "magic_close")
	(:arguments (arg0 magic_t))
	(:library "/usr/lib/libmagic.so"))

(ffi:def-call-out magic_file
	(:name "magic_file")
	(:arguments (arg0 magic_t)
		(arg1 ffi:c-string))
	(:return-type ffi:c-string)
	(:library "/usr/lib/libmagic.so"))

(ffi:def-call-out magic_descriptor
	(:name "magic_descriptor")
	(:arguments (arg0 magic_t)
		(arg1 ffi:int))
	(:return-type ffi:c-string)
	(:library "/usr/lib/libmagic.so"))

(ffi:def-call-out magic_buffer
	(:name "magic_buffer")
	(:arguments (arg0 magic_t)
		(arg1 (ffi:c-pointer NIL))
		(arg2 size_t))
	(:return-type ffi:c-string)
	(:library "/usr/lib/libmagic.so"))

(ffi:def-call-out magic_error
	(:name "magic_error")
	(:arguments (arg0 magic_t))
	(:return-type ffi:c-string)
	(:library "/usr/lib/libmagic.so"))

(ffi:def-call-out magic_setflags
	(:name "magic_setflags")
	(:arguments (arg0 magic_t)
		(arg1 ffi:int))
	(:return-type ffi:int)
	(:library "/usr/lib/libmagic.so"))

(ffi:def-call-out magic_load
	(:name "magic_load")
	(:arguments (arg0 magic_t)
		(arg1 ffi:c-string))
	(:return-type ffi:int)
	(:library "/usr/lib/libmagic.so"))

(ffi:def-call-out magic_compile
	(:name "magic_compile")
	(:arguments (arg0 magic_t)
		(arg1 ffi:c-string))
	(:return-type ffi:int)
	(:library "/usr/lib/libmagic.so"))

(ffi:def-call-out magic_check
	(:name "magic_check")
	(:arguments (arg0 magic_t)
		(arg1 ffi:c-string))
	(:return-type ffi:int)
	(:library "/usr/lib/libmagic.so"))

(ffi:def-call-out magic_errno
	(:name "magic_errno")
	(:arguments (arg0 magic_t))
	(:return-type ffi:int)
	(:library "/usr/lib/libmagic.so"))
