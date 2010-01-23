(setq custom:*default-file-encoding*  charset:iso-8859-1)

	;; HTTP SERVER....
(defpackage :httpd
  (:use :common-lisp :socket :html)
  (:export start service-publish  request-parameter-value
	   request-parameters read-headers))

(in-package :httpd)
;; Utility functions used by various parts of the server.
;; They have been moved here so as not to clutter other files.
(setf *DEBUG* nil)

(defun strcat (&rest strs)
  (eval `(concatenate 'string ,@strs)))
 
(defun link (name address)
    (format nil "<a href=\"~A\"> ~A </a>" address name)) 

(defun minpos (cc s &key start)
  "Returns the first occurance of any member of sequence cc in sequence s."
  (let ((l (length s)))
    (apply #'min (map 'list #'(lambda (c)
                                (or (position c s :start (or start 0)) l))
                      cc))))

(defun splits-string (cc s)
  "list; string -> list string
Splits the sequence s on members of the sequence cc"
  (do ((a 0 (1+ z))
       (z (minpos cc s) (minpos cc s :start (1+ z)))
       (ss nil (cons (subseq s a z) ss)))
      ((equal z (length s))
       (nreverse (cons (subseq s a) ss)))))

(defun intern-keyword (name)
  "Interns name into the KEYWORD package."
  (intern (string-upcase name) "KEYWORD"))

(defun string-list-key-list (qs)
  "Turns the 0th, 2nd, 4th, ... string in qs into a keyword."
  (do ((s qs (cddr s))
       (q '() (let ((n (car s))
                    (v (cadr s)))
                (cons v (cons (intern-keyword n) q)))))
      ((null s) (nreverse q))))

(defmacro http-format (destination control-string &rest args)
  `(format ,destination
           ,(concatenate 'string control-string "~C~C")
           ,@(append args `(,(code-char 13) ,(code-char 10)))))

(defun assocr (item alist &key key test)
  (cdr (assoc item alist :key key :test test)))

(defvar *published-urls* nil)

(defun header-lookup (name headers)
  (let ((found (assoc name headers :test #'string=)))
    (if found (cdr found))))

(defun header-lookup-int (name headers)
  "Looks up a header by name and returns its' value as an integer"
  (parse-integer (header-lookup name headers)))

(defun service-lookup (method uri)
  (cdr
   (assoc (list (string method) uri)
          *published-urls*
          :test #'equal)))

(defun service-publish (method uri service)
  (setf *published-urls*
        (cons (cons (list (string method) uri) service) *published-urls*)))

(defun serve (method uri version stream)
  "method; uri; version; stream -> nil"
  (let* ((path (first (splits-string "?" uri)))
         (service (service-lookup method path)))
    (or service 
	(setf service (service-lookup "GET" "error")))
    (http-format stream "HTTP/1.1 200 OK")
    (funcall service stream uri)))

(defun add-content-type (data &optional content)
  (if content 
    (format nil "Content-Type: ~A~2%~A" content data )
    (format nil "Content-Type: text/html~2%~A" data)))
    
(defun gen-html-page (title body)
  (add-content-type (html (head (title title)) (body body))))

;;; BEGIN CODE TO HANDLE FORTUNES
(setf *RANDOM-STATE* (MAKE-RANDOM-STATE T))

(defvar *ALL-FORTUNES* '())

(defun data-to-list (file-name)
  (let ((fortunes-list '())
	(aux-string ""))
    (with-open-file (stream file-name)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	  ((null line))
	(if (not (string-equal "%" line))

	    (setf aux-string (concatenate 'string aux-string (format nil "~A" line)))

	    (progn 
	      (if (not (string-equal "" aux-string))
	        (push aux-string fortunes-list))
	      (setf aux-string "")))))
    (reverse fortunes-list)))
;; populate fortunes list
(let ((files  (list "data/freebsd-tips" "data/murphy" "data/fortune" "data/startrek")))

      (dolist (file files)
	(setf *ALL-FORTUNES* 
	      (append 
	        *ALL-FORTUNES* 
	        (data-to-list file)))))
		    
(defun get-fortune  ()
    (nth (random (length *ALL-FORTUNES*)) *ALL-FORTUNES*))

(defun error-service (stream uri)
  (http-format stream "~A" 
    (gen-html-page 
      "Error Page" 
      (strcat (p "This : ")(p (i uri))(p "does not exist")))))

(defun root-service (stream uri)
 (http-format stream "~A" 
   (gen-html-page "Root Page"  (p (get-fortune)) )))

;   (gen-html-page "Root Page" (strcat (p "Default Page")(p uri)))))

;publish services notice different approach
(service-publish 'GET "error" #'error-service)
(service-publish 'GET "/" #'root-service)

(defun start ()
  (ext:exit-on-error)
  (let ((server (socket-server 8888)))
    (unwind-protect
      ;; Main work loop.
      (loop as stream = (socket-accept server
                                       :element-type 'character
                                       :buffered t)
            do
	    (if *DEBUG*
		(unwind-protect
            	    (multiple-value-call #'serve (read-request stream) stream)
            	    (close stream))
		(ignore-errors
        	    (unwind-protect
            		(multiple-value-call #'serve (read-request stream) stream)
            		(close stream)))))
      ;; Cleanup forms.
      (socket-server-close server))))

(defun read-request (stream)
  (let ((r (splits-string " " (read-line stream nil nil))))
    (let ((request-type (first r))
          (request-uri (second r))
          (http-version (third r)))
      (values request-type request-uri http-version))))

(defun read-headers (stream)
  (let ((h (make-hash-table :test #'equal)))
    (do ((l (read-line stream nil nil) (read-line stream nil nil)))
        ((string= l ""))
      (let* ((split (splits-string ":" l))
             (header-name (first split))
             (header-value (second split)))
        (setf (gethash header-name h) header-value)))
    h))

