;; My Own Web Server ... just because i want to.
;;; Ideal: Make web server in lisp that can load dinamic .lisp files...
;;; Reality: Web server that interprets (more or less) .lisp files
;;; Camilo Olarte 2006 juan.c.olarte@gmail.com

(defun strcat (&rest strs)
  "return string concatenation"
  (eval `(concatenate 'string ,@strs)))

(defun read_da_file (file_name)
    "returns a string containing a file read."
    (with-open-file (stream file_name)
	(let ((strin ""))
	    (do ((line (read-line stream nil)
            	       (read-line stream nil)))
    		((null line))
    	    (setq strin (strcat strin line)))
         strin)))

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

(defun put_http_hdr (conn)
    "Writes HTTP Headers over a connection(socket_stream)"    
  (format conn "HTTP/1.0 200 OK~%")
  (format conn "Content-Type: text/html ~2%"))

(dolist 
  (tag '(big center body div form h1 h2 h3 head html small table 
		  title tr td))
  (eval 
	`(defun ,tag (&rest args)
	  (let ((tag_val (symbol-name ',tag)))
		(format nil "~4T<~A>~%~4T~{ ~A ~}~%~4T</~A>" 
					  tag_val 		args 		tag_val)))))

(defun web_page (str_title contents conn)
    "Writes Generic Web Page over a connection(socket_stream)"    
    (format 
	  conn 
	  (strcat (html (head (title str_title)) (body contents))"~%")))

(defun do_file (file_toproc conn)
  "process a file and return its interpreted output"
    (if (equal  file_toproc "")
    	    (web_page "TITLE :: No File" "<BR> NO FILE GIVEN " conn)
    	    (progn 
			  (if (probe-file file_toproc)	
				(progn
				  (eval (read-from-string (read_da_file file_toproc)))
				  (web_page title content conn))
			    (web_page "TITLE :: Not Found" "<BR> NO Found " conn)))))

;(defun do_file (file_toproc conn)
;  "process a file and return its interpreted output"
;  (web_page "TITLE :: No File" (strcat "<BR>"  file_toproc) conn))

;; at this time a server will be created ..
;; try it at http://localhost:8000  or http://localhost:8000/file_to_interpret
(let  ((a-server-socket (socket-server 8000)))
    (princ "Server Started.")
    (loop
       (let* ((connection (socket-accept a-server-socket))
			(line (read-line connection)) 
			(second_argument (second (splits-string " " line)))
			(file_toload (subseq second_argument 1 (length second_argument))))
  		  (put_http_hdr connection)
  		  (do_file file_toload connection)
		(close connection)))
     (socket-server-close a-server-socket))