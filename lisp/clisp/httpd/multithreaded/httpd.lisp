(load 'html)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  for CLISP hack
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-UNIX
(eval-when (:execute :load-toplevel :compile-toplevel)
  (let ((encoding (make-encoding :charset "ISO-8859-1" :line-terminator :unix)))
    (system::set-default-file-encoding encoding)
    (system::set-foreign-encoding encoding)
    (system::set-misc-encoding encoding)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  CL-Webapp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clisp
(defpackage :cl-webapp 
    (:use :cl :ext :socket :html :mt) 
    (:shadowing-import-from :ext :address)
    (:shadowing-import-from :ext :dir) 
    (:nicknames :webapp) 
    (:export :serve :threaded-server))
#-clisp
(defpackage :cl-webapp (:use :cl :port) (:nicknames :webapp) (:export :serve))
(in-package :cl-webapp)

(defclass <request> ()
  ((header :accessor header-of :initform nil)
   (method :accessor method-of :initform nil)
   (url    :accessor url-of    :initform nil)
   (param  :accessor param-of   :initform nil)))

(defclass <application> ()
  ((base-url  :accessor base-url-of :initarg :base-url)
   (name :accessor name-of :initarg :name)))

(defclass <server> ()
  ((server-socket :accessor server-socket-of :initform nil)
   (applications  :accessor applications-of  :initform (make-hash-table :test #'equal))
   (default       :accessor default-of       :initform (make-instance '<application>))
   (port          :accessor port-of          :initform nil)
   (request       :accessor request-of       :initform nil)))

(defgeneric parse-http-header (self str))
(defgeneric parse-parameter (self str))
(defgeneric parse-url (self str))
(defgeneric start-server (self port))
(defgeneric request (self s))
(defgeneric parse-request (self s))
(defgeneric dispatch (self s))
(defgeneric debug-show (req))
(defgeneric page-view (application request))

(defmethod parse-http-header ((self <request>) str)
  (let* ((p (position #\: str))
	 (end (position-if (complement #'(lambda (c) (or (eql c #\return) (eql c #\linefeed))))
			   str
			   :from-end t))
	 (v (and p
		 (list (url-decode-string (subseq str 0 p))
		       (url-decode-string (subseq str (+ p 2) (and end (1+ end))))))))
    (push v (header-of self))))

(defmethod parse-parameter ((self <request>) str)
  (let ((p1 (position #\= str))
	(p2 (position #\& str)))
    (cond (p1
	   (push (list (url-decode-string (subseq str 0 p1))
		       (url-decode-string (subseq str (1+ p1) p2)))
		 (param-of self))
	   (when p2
	     (parse-parameter self (subseq str (1+ p2)))))
          ((string= str "") nil )
          (t (url-decode-string str)))))

(defun url-decode-string (s)
  (with-output-to-string (out)
    (do ((i 0 (1+ i)))
        ((>= i (length s)) out)
      (let ((c (char s i)))
        (case c
          (#\% (setf c (code-char (parse-integer s
						 :start (+ i 1) :end (+ i 3)
						  :radix 16 :junk-allowed t)))
	       (unless (or (null c) (eql c #\linefeed)) (write-char c out))
	       (incf i 2))
          (#\+ (write-char #\Space out))
          (otherwise (write-char c out)))))))

(defun add-content-type (data &optional content)
  (if content 
    (format t "HTTP/1.0 200 OK~%Content-Type: ~A~2%~A" content data )
    (format t "HTTP/1.0 200 OK~%Content-Type: text/html~2%~A" data)))

(defun gen-html-page (title body)
  (add-content-type (html (head (title title)) (body body))))

(defmethod parse-url ((self <request>) str)
  (let ((p1 (position #\Space str))
	(p2 (position #\Space str :from-end t)))
    ;;(write-line "[request]") ; for debug
    (write-line str)           ; for debug
    (when (and p1 p2 (> p2 p1))
      (let ((method (subseq str 0 p1))
	    (url    (subseq str (+ p1 1) p2))
	    (args   nil))
	(when (and ;;(string= method "GET")
		   (setf p1 (position #\? url)))
	  (setf args (subseq url (1+ p1))
		url  (subseq url 0 p1))
	  (parse-parameter self args))
	(setf (method-of self) method
	      (url-of self)    url)))))

(defmethod start-server ((self <server>) port)
  (unless (server-socket-of self)
    (unwind-protect
	(progn
	  (setf (server-socket-of self)
		#+clisp
		(socket-server port)
		#-clisp
		(port:open-socket-server port)
		(port-of self)          port)
	  (loop
	   (request self
		    #+clisp
		    (socket-accept (server-socket-of self))
		    #-clisp
		    (port:socket-accept (server-socket-of self))
		    )))
      #+clisp(socket-server-close (server-socket-of self))
      #-clisp(port:socket-server-close (server-socket-of self))
      (setf (server-socket-of self) nil))))


(defmethod request ((self <server>) s)
  (make-thread #'(lambda ()
  (ignore-errors
  (unwind-protect
      (progn
	(parse-request self s)
	(dispatch self s))
    (finish-output s)
    (close s))))))

(defmethod parse-request ((self <server>) s)
  (let ((request (make-instance '<request>)))
    (setf (request-of self) request)
    ;; parse url & arguments
    (parse-url request (read-line s))
    ;; parse http header
    (loop for line = (read-line s nil :eof)
	  until (<= (length line) 2)
	  do
	  ;;(write-line line) ; for debug
	  (parse-http-header request line))
    ;; parse POSTed data
    (let ((h (assoc "Content-Length" (header-of request) :test #'equal)))
      (when (and h (listen s))
	(let* ((length (parse-integer (second h)))
	       ;;(str (make-string (1- length)))
	       (str (make-string length)))
	  #+clisp(read-char s) ;; chop NEWLINE
	  ;;#+clisp
	  ;;(read-char-sequence str s)
	  ;;#+cmu
	  (read-sequence str s)
	  (parse-parameter request str))))))

(defmethod dispatch ((self <server>) s)
  (let* ((request (request-of self))
	 (url (url-of request))
	 (application (or (gethash (subseq url 0 (position #\/ url :start (if (>= (length url) 1) 1 0)))
				   (applications-of self))
			  (default-of self))))
    (let ((*standard-output* s))
      (page-view application request))))

(defmethod debug-show ((req <request>))
  (format t "<pre>~%header: ~S~%method: ~S~%url: ~S~%param: ~S~%</pre>~%"
	  (header-of req) (method-of req) (url-of req) (param-of req)))

(defmethod page-view ((application <application>) (request <request>))
  (write-line "HTTP/1.0 200 OK")
  (write-line "Content-Type: text/html;")
  (write-line "")
  (write-line "<html><body>")
  (write-line "<h1>Webapp for Common Lisp</h1>")
  (write-line "<p>Howdy</p>")
  ;;(debug-show request)
  ;; POST TEST
  ;;(write-line "<form method=\"POST\" action=\"/lispypost?name=lambda&lang=lisp\">")
  ;;(write-line "<textarea name=contents cols=80 rows=10></textarea>")
  ;;(write-line "<input type=submit value=\"submit\">")
  ;;(write-line "</form>")
  (write-line "</body></html>"))

(defvar *server* (make-instance '<server>))

(defvar *server-thread* nil)

(defun serve (&optional (port 8080))
  (start-server *server* port))

(defun threaded-server ()
    (setf *server-thread* (MAKE-THREAD #'serve)))
    
(defmacro defwebapp (class name-key name base-url-key base-url &rest option)
  (assert (eq name-key :name))
  (assert (eq base-url-key :base-url))
  `(setf (gethash ,base-url (webapp::applications-of webapp::*server*))
    (make-instance ',class :name ,name :base-url ,base-url ,@option)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sample Application 1 - 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <hello> (<application>) ())

(defwebapp <hello>
    :name "Sample Application - 1"
    :base-url "/hello")

(defmethod page-view ((app <hello>) (req <request>))
    "Hello Application"
    (write-line  (gen-html-page "Sample Application" (h1 "Hello"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sample Application 2 - static html
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <static-html> (<application>)
  ((document-root :accessor document-root-of :initarg :document-root)))

(defwebapp <static-html>
    :name "Sample Application - 2"
    :base-url "/html"
    :document-root
    #+clisp "~/public_html"
    #+cmu   "home:public_html")

(defmethod page-view ((app <static-html>) (req <request>))
  (let* ((url  (url-of req))
	 (path (concatenate
		'string
		(document-root-of app)
		(subseq url (length (base-url-of app)))))
	 (type (subseq url (- (length url) 4)))
	 (file (and (not (eq (char path (1- (length path))) #\/))
		    (probe-file path))))
    (cond (file
	   (write-line "HTTP/1.0 200 OK")
	   (cond ((string-equal type ".css")
		  (write-line "Content-Type: text/css"))
		 ((or (string-equal type "html")
		      (string-equal type ".htm"))
		  (write-line "Content-Type: text/html"))
		 ((string-equal type ".png")
		  (write-line "Content-Type: image/png"))
		 ((or (string-equal type ".jpg")
		      (string-equal type ".jpeg"))
		  (write-line "Content-Type: image/jpeg"))
		 ((string-equal type ".gif")
		  (write-line "Content-Type: image/gif"))
		 (t
		  (write-line "Content-Type: text/plain")))
	   (write-line "")
	   (with-open-file (s file :direction :input
			      #+clisp :element-type #+clisp 'unsigned-byte)
	     (let* ((size (file-length s))
		    #+clisp
		    (buf  (make-array size :element-type 'unsigned-byte))
		    #-clisp
		    (buf  (make-string size)))
	       (read-sequence buf s)
	       #+clisp
	       (write-sequence (ext:convert-string-from-bytes buf 'charset:iso-8859-1) *standard-output*)
	       #-clisp
	       (write-sequence buf *standard-output*))))
	  (t
	   (write-line "HTTP/1.0 404 OK")
	   (write-line "Content-Type: text/html")
	   (write-line "")
	   (write-line "<html><body>")
	   (write-line "<h1>Webapp - Static Contents</h1>")
	   (write-line "<pre>")
	   (format t "~A(~A) is NOT FOUND" url path)
	   (write-line "</pre>")
	   (write-line "</body></html>")))))

