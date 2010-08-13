;;; This file was automatically generated by SWIG (http://www.swig.org).
;;; Version 1.3.39
;;;
;;; Do not make changes to this file unless you know what you are doing--modify
;;; the SWIG interface file instead.


;;;SWIG wrapper code starts here

(cl:defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(cl:progn ,@(cl:loop for value in enums
                        for index = 0 then (cl:1+ index)
                        when (cl:listp value) do (cl:setf index (cl:second value)
                                                          value (cl:first value))
                        collect `(cl:defconstant ,value ,index))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'swig-lispify)
    (cl:defun swig-lispify (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 ((lower digit) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit 
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c)))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t ""))))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list name) cl:nil cl:nil))
            fix)
           package))))))

;;;SWIG wrapper code ends here


(cffi:defcstruct PANEL
	(win :pointer)
	(below :pointer)
	(above :pointer)
	(user :pointer))

(cffi:defcfun ("panel_window" panel_window) :pointer
  (arg0 :pointer))

(cffi:defcfun ("update_panels" update_panels) :void)

(cffi:defcfun ("hide_panel" hide_panel) :int
  (arg0 :pointer))

(cffi:defcfun ("show_panel" show_panel) :int
  (arg0 :pointer))

(cffi:defcfun ("del_panel" del_panel) :int
  (arg0 :pointer))

(cffi:defcfun ("top_panel" top_panel) :int
  (arg0 :pointer))

(cffi:defcfun ("bottom_panel" bottom_panel) :int
  (arg0 :pointer))

(cffi:defcfun ("new_panel" new_panel) :pointer
  (arg0 :pointer))

(cffi:defcfun ("panel_above" panel_above) :pointer
  (arg0 :pointer))

(cffi:defcfun ("panel_below" panel_below) :pointer
  (arg0 :pointer))

(cffi:defcfun ("set_panel_userptr" set_panel_userptr) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("panel_userptr" panel_userptr) :pointer
  (arg0 :pointer))

(cffi:defcfun ("move_panel" move_panel) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int))

(cffi:defcfun ("replace_panel" replace_panel) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("panel_hidden" panel_hidden) :int
  (arg0 :pointer))

