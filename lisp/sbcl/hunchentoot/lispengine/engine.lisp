(defpackage :lispengine
  (:nicknames :lispen)
  (:use :cl :hunchentoot :cl-who)
  (:export :startit))

(in-package :lispengine)
(load "loader.lisp")

(setf hunchentoot::*MESSAGE-LOG-PATHNAME* "/usr/home/colarte/dev/lispengine/message.log")
(setf hunchentoot::*ACCESS-LOG-PATHNAME* "/usr/home/colarte/dev/lispengine/access.log")

(defun slurp-stream (stream)
  (let ((seq (make-string (file-length stream))))
		(read-sequence seq stream)
	  seq))

(defun main-handler-old ()
 (format 
    nil 
    "Hi, <b> Uri is : ~a </b>  <br> 
    method iss :~a ~% <br> and pars are ~a  ~% 
    <br> scriptname   are ~a" 
    (request-uri*)
    (request-method*) 
    (get-parameters*)
    (script-name*)))
    
(setf hunchentoot::*default-handler* #'main-handler-old) 

(defun startit ()
    (start (make-instance 'hunchentoot:acceptor :port 8888)))


;;and the static file path
(defvar *static-web-files* (merge-pathnames "document-root/static/" *default-pathname-defaults*))

(push 
 (hunchentoot:create-folder-dispatcher-and-handler "/static/" *static-web-files*)
 hunchentoot:*dispatch-table*)
