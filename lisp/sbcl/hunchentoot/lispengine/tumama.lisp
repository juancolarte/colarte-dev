(in-package :lispengine)

(defun tumama ()
 (format 
    nil 
    "Hi, <b> This is tumama.lisp </b>   <br> ~a  ~% " (parameter "val")))

(push  (create-prefix-dispatcher "/star" 'tumama)  hunchentoot:*dispatch-table*)

