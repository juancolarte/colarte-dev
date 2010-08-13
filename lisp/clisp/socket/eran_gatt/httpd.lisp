;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  HTTP.LSP - An object-oriented http server written in CLisp.
;;;
;;;  Copyright (c) 1999 by Erann Gat, all rights reserved.
;;;
;;;  This program may be used and distributed in accordance with the
;;;  GNU General Public Licence (GPL) as published by the Free Software
;;;  Foundation (www.fsf.org)
;;;
;;;  Please send comments, improvements, and bug reports to Erann Gat
;;;  <gat@jpl.nasa.gov>.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  General notes:
;;;
;;;  This program really ought to be in three files, but I ran into
;;;  a problem with CLisp (which has since been fixed) that caused me
;;;  to merge them together into one to get things to work.  Some day
;;;  I'll unmerge them.  For now, you should be aware that there are
;;;  three major sections in this file.  The first is a set of general
;;;  utilities that I use in all my programs.  These include some
;;;  top-level macros that I use for defining classes and methods.
;;;  These macros are:
;;;
;;;  (DEFINE-CLASS class-name &rest slot-specs)
;;;
;;;  where a slotspec is either just a slot name or a list (slot-name
;;;  initial-value).
;;;
;;;  (DEFINE-METHOD (method-name (self-var classname &rest slot-names)
;;;                              &rest args)
;;;                 &body body)
;;;
;;;  DEFINE-METHOD is syntactic sugar for DEFMETHOD and WITH-SLOTS.
;;;
;;;  Some other weird things I use a lot: RECEIVE is a synonym for
;;;  MULTIPLE-VALUE-BIND.  ITERATE is like named LET.
;;;
;;;  All these things come from long ago when I was a fan of T and
;;;  OakLisp.
;;;
;;;  The second part of the file is the server proper.
;;;
;;;  The general idea behind the design of this http server is that
;;;  interacting with it should be fundamentally an interaction with
;;;  Lisp objects.  Instead of a read-eval-print loop it's a
;;;  submit-request - eval - general-html loop.  I'm sure I have not
;;;  realized that vision in the best possible way, but it's not
;;;  bad either.
;;;
;;;  The third part of the file is a set of examples.  This is probably
;;;  the best place to start.
;;;
;;;  Quickstart:
;;;
;;;  1.  Load this file.  Then compile it and load it again.  (I haven't
;;;      gone through to put eval-whens in the right place to allow it to
;;;      compile without first being loaded.)
;;;  2.  Call serve.  Serve takes an optional port number. The defult is
;;;      1234.
;;;
;;;  This code has been tested with the 1999-07-22 version of CLisp.
;;;  It will probably not run under earlier versions.  (This code
;;;  depends on several functions that had bugs in earlier versions.)
;;;

(in-package :user)



;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  General utilities
;;;
(defun concatenate-symbol (&rest symbols)
  (let ( (*print-case* (readtable-case *readtable*)) )
    (declare (special *print-case* *readtable*))
    (intern (format nil "~{~A~}" symbols))))



(defmacro iterate (name args &rest body)
  `(labels ((,name ,(mapcar #'car args) ,@body))
     (,name ,@(mapcar #'cadr args))))



(defmacro define-synonym (s1 s2)
  `(setf (symbol-function ',s1) (function ,s2)))



(defun sym= (s1 s2)
  (and (symbolp s1) (symbolp s2) (string= (symbol-name s1) (symbol-name s2))))



(defmacro fn (args &body body)
  `#'(lambda ,args (declare (ignorable ,@args)) ,@body))



(defun hex (n)
  (format t "#x~X" n) (values))



(defun sqr (x) (* x x))



(defun rsq (&rest numbers) ; Root of the sum of the squares
  (let ( (result 0) )
    (dolist (n numbers) (incf result (* n n)))
    (sqrt result)))



(defmacro deletef (thing place &rest args)
  `(setf ,place (delete ,thing ,place ,@args)))



(defmacro while (condition &body body)
  (let ( (tag (gensym "LABEL")) )
    `(tagbody ,tag (when ,condition ,@body (go ,tag)))))



(defmacro until (condition &body body)
  (let ( (tag (gensym "LABEL")) )
    `(tagbody ,tag (unless ,condition ,@body (go ,tag)))))



;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  CLOS utilities
;;;
(defmacro define-class (name &rest slots)
  (let ( (id (if (atom name) name (car name))) )
    `(progn
       (defclass ,id
         ,(if (atom name) '() (cdr name))
         ,(mapcar #'(lambda (slot)
                      (if (and (consp slot) (cddr slot))
                        (error "Too many items in slot specification: ~S" slot))
                      (let ( (name (if (symbolp slot) slot (first slot)))
                             (initform (if (symbolp slot) nil (second slot))) )
                        `(,name :initarg ,(intern (symbol-name name) 'keyword)
                                :accessor ,(concatenate-symbol id '- name)
                                :initform ,initform)))
                  slots))
       (defun ,(concatenate-symbol 'make- id) (&rest args)
         (apply #'make-instance ',id args))
       (defun ,(concatenate-symbol id '?) (arg)
         (typep arg ',id))
       ',id)))



(defun extract-declarations (body)
  (iterate loop ( (declarations nil) (body body) )
    (if (and (consp body) (consp (car body))
             (eq (caar body) 'declare))
      (loop (cons (car body) declarations) (cdr body))
      (values declarations body))))



(defmacro define-method ((operation (selfarg type &rest instance-vars)
              &rest args)
                         &body body)
  (multiple-value-bind (declarations body) (extract-declarations body)
    `(defmethod ,@(if (atom operation) (list operation) operation)
                ((,selfarg ,type) ,@args)
       ,@declarations
       ,(if instance-vars
          `(with-slots ,instance-vars ,selfarg ,@body)
          `(progn ,@body)))))



(defmacro define-print-method ((class &rest ivars) &rest args)
  `(define-method (print-object (self ,class ,@ivars) stream)
     (format stream ,@args)))



(defmacro define-standard-print-method (class)
  `(define-print-method (,class) "#<~ [unrecognised ~A~ keyword occurred here: args NIL]  #x~X>" ',class (%address-of self)))



;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Mappers
;;;
(define-synonym walk mapc)



(defun map1 (fn mapped-arg &rest unmapped-args)
  (mapcar #'(lambda (arg) (apply fn arg unmapped-args)) mapped-arg))



(defun walk1 (fn mapped-arg &rest unmapped-args)
  (mapc #'(lambda (arg) (apply fn arg unmapped-args)) mapped-arg))



(defun mapleaves (fn tree)
  (iterate loop ( (tree tree) )
    (if (atom tree)
      (funcall fn tree)
      (cons (loop (car tree)) (and (cdr tree) (loop (cdr tree)))))))



(defun mapleaves! (fn tree)
  (iterate loop ( (tree tree) )
    (if (atom tree)
      (funcall fn tree)
      (progn
        (setf (car tree) (loop (car tree)))
        (setf (cdr tree) (and (cdr tree) (loop (cdr tree))))
        tree))))



(defun mappend (fn &rest lists) (apply #'append (apply #'mapcar fn lists)))
(define-synonym mappend! mapcan)
(defun mappend1 (fn &rest lists) (apply #'append (apply #'map1 fn lists)))



(define-synonym mapcdr maplist)
(define-synonym walkcdr mapl)



(defun map! (fn l)
  (walkcdr #'(lambda (l) (setf (car l) (funcall fn (car l)))) l))



(defmacro maplet (bindings &body body)
  `(mapcar (fn ,(mapcar #'car bindings) ,@body) ,@(mapcar #'second bindings)))



(defmacro walklet (bindings &body body)
  `(walk (fn ,(mapcar #'car bindings) ,@body) ,@(mapcar #'second bindings)))



(defmacro n-of (form n)
  (let ( (countvar (gensym "N")) )
    `(iterate loop ( (,countvar ,n) )
       (if (zerop ,countvar) nil (cons ,form (loop (1- ,countvar)))))))



;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Scheme/T stuff
;;;
(defmacro receive (vars form &body body)
  (if (atom vars)
    `(let ( (,vars (multiple-value-list ,form)) )
       ,@body)
    `(multiple-value-bind ,vars ,form ,@body)))



(defmacro bind (bindings &body body)
  (let ( (tempvars (n-of (gensym "TEMP") (length bindings))) )
    `(let ,(mapcar #'list tempvars (mapcar #'car bindings))
       (unwind-protect
         (progn
           ,@(mapcar (fn (binding) (cons 'setf binding)) bindings)
           ,@body)
         ,@(mapcar (fn (binding tempvar) `(setf ,(car binding) ,tempvar))
                   bindings tempvars)))))



(defmacro tcond (&rest clauses)
  (iterate loop1 ( (clauses clauses) )
    (let ( (clause (car clauses)) )
      (cond
       ( (null clauses) nil )
       ( (sym= (second clause) '=>)
         (assert (= (length clause) 3) () "Illegal COND clause: ~S" clause)
     (let ( (tempvar (gensym "TEMP")) )
       `(let ( (,tempvar ,(car clause)) )
          (if ,tempvar
                  (funcall ,(caddr clause) ,tempvar)
        ,(loop1 (cdr clauses))))) )
       ( (null (cdr clause))
         `(or ,(car clause) ,(loop (cdr clauses))) )
       (t
        `(if ,(car clause)
           (progn ,@(cdr clause))
           ,(loop (cdr clauses))))))))



(define-synonym eq? eq)
(define-synonym null? null)
(define-synonym zero? zerop)
(define-synonym number? numberp)
(define-synonym atom? atom)
(define-synonym procedure? functionp)



;;;;;;;;;;;;;;;;;
;;;
;;;  TCP utitilies
;;;
#+MCL
(progn
  (defun open-client-socket (host port)
    (ccl::open-tcp-stream host port))



  (defun connection-state (s)
    (ccl::opentransport-stream-connection-state s))



  (defun open-server-socket (port)
    (let ( (s (ccl::open-tcp-stream nil port)) )
      (unwind-protect
      (process-wait "Waiting for connection"
            (fn () (eq (connection-state s) :dataxfer)))
    (unless (eq (connection-state s) :dataxfer) (close s)))
      s))
)



#+CLISP
(import 'system::fixnump)



(defvar *server-stream* nil)
(defvar *http-trace* nil)
(defvar *http-root-object*)
(defvar *safe-readtable* (copy-readtable))
(dolist (c (list #\# #\,))
  (set-syntax-from-char c #\a *safe-readtable*))



;;;;;;;;;;;;;;;
;;;
;;; The top-level function
;;;
#+MCL
(defun serve (&optional (port 1234))
  (loop
    (let ( (s (open-server-socket port)) )
      (process-run-function "server" 'serve-socket s))))



#+CLISP
(defun serve (&optional (port 1234))
  (let ( (server (socket-server port)) )
    (unwind-protect
    (loop
      (serve-socket (socket-accept server)))
      (socket-server-close server)))
  (values))



;;;;;;;;;;;;;;
;;;
;;; HTTP utilities
;;;
(defun http-print (&rest items)
  (let ( (s *server-stream*) )
    (dolist (item items)
      (if (stringp item)
        (princ item s)
        (princ (http-encode-literal-string (princ-to-string item)) s)))
    (princ #\return s)
    (princ #\linefeed s)))



#+MCL
(defun http-read-line ()
  (ccl::telnet-read-line *server-stream*))



#+CLISP
(defun http-read-line ()
  (let* ( (s (read-line *server-stream*))
      (len (1- (length s))) )
    (if (and (>= len 0) (eql (char s len) #\CR))
    (setf s (subseq s 0 len)))
    s))



(defun http-encode-literal-string (s)
  (unless (stringp s) (setf s (princ-to-string s)))
  (with-output-to-string (out)
    (do ( (i 0 (1+ i)) )
        ( (>= i (length s)) out )
      (let ( (c (char s i)) )
        (princ (case c (#\< "&lt;") (#\> "&gt") (otherwise c)) out)))))



(defmacro safely (&body body)
  `(let ( (*readtable* *safe-readtable*) )
     (declare (special *readtable*))
     (receive (val err) (ignore-errors (progn ,@body))
       (if (typep err 'condition) err val))))



(defun parse-http-header (h)
  (let ( (index (position #\: h)) )
    (and index
         (list (intern (string-upcase (subseq h 0 index)))
               (subseq h (+ index 2))))))



(defun parse-url (s)
  (let* ( (index1 (position #\Space s))
          (index2 (position #\Space s :from-end t)) )
    (and index1
         index2
         (> index2 index1)
         (let ( (method (intern (subseq s 0 index1)))
                (url (subseq s (+ index1 2) index2))
                (form-slots "") )
           (when (and (eq method 'get) (setf index1 (position #\? url)))
             (setf form-slots (subseq url (1+ index1)))
             (setf url (subseq url 0 index1)))
           (values url (parse-form-slots form-slots))))))



(defun parse-form-slots (s)
  (let* ( (index1 (position #\= s))
          (index2 (position #\& s)) )
    (cond ( index1
            (cons (intern (string-upcase (subseq s 0 index1)))
                  (cons (parse-url-encoded-string
                         (subseq s (1+ index1) index2))
                        (and index2
                             (parse-form-slots (subseq s (1+ index2)))))) )
          ( (string= s "") nil )
          (t s))))



(defun parse-url-encoded-string (s)
  (with-output-to-string (out)
    (do ( (i 0 (1+ i)) )
        ( (>= i (length s)) out )
      (let ( (c (char s i)) )
        (case c
          (#\% (setf c (code-char (parse-integer s :start (+ i 1) :end (+ i 3)
                                                 :radix 16 :junk-allowed t)))
           (unless (or (null c) (eql c #\linefeed)) (princ c out))
           (incf i 2))
          (#\+ (princ #\Space out))
          (otherwise (princ c out)))))))



(defun emit-http-preamble ()
  (http-print "HTTP/1.0 200 OK")
  (http-print "Content-Type: text/html")
  (http-print "")
  (http-print "<body bgcolor=\"#FFFFFF\"><title>Test server</title><h3>"))



;;;;;;;;;;;;;;
;;;
;;;  HTTP namespace
;;;
;;;  This should probably be rewritten to store HTTP bindings on the
;;;  symbol's property list.
;;;

(defvar *http-namespace* '())



(defmacro define-http (name value)
  `(setf (getf *http-namespace* ',name) ,value))



(defun get-http-object (name) (getf *http-namespace* name))



;;;;;;;;;;;;;;;;;
;;;
;;;  Top-level server loop
;;;
(defun serve-socket (s)
  (unwind-protect
    (let ( (*server-stream* s)
           url form-slots )
      (declare (special *server-stream*))
      (emit-http-preamble)
      ; Get the URL
      (setf url (http-read-line))
      ; Parse the URL
      (multiple-value-setq (url form-slots) (parse-url url))
      ; Get the headers (ignored for now)
      (loop for header = (parse-http-header (http-read-line))
      while header append header)
      ; Get the headers from a POST operation
      (if (null form-slots)
        (setf form-slots
              (loop while (listen s)
                    append (parse-form-slots (http-read-line)))))
      (when *http-trace* (print (list url form-slots)))
      ; This is the heart of the server
      (let ( (obj (if (string= url "")
              :http-root-object
            (safely (read-from-string url)))) )
    ; If the URL is not a symbol just evaluate it
    (if (not (symbolp obj))
        (setf obj (safely (eval obj)))
      ; If the URL is a symbol get the http object bound to the symbol
      (progn
        (setf obj (get-http-object obj))
        (if (null? obj) (setf obj (http-not-found url)))
        ;; If the object is a procedure, execute it, otherwise
        ;; just use the object itself
        (if (procedure? obj)
        (setf obj (safely (funcall obj form-slots))))))
    ; Handle errors here
        (if (typep obj 'condition) (setf obj (http-error obj)))
    (if *http-trace* (print obj))
    ; Emit the result
        (http-emit obj)
    ; Generate something for NIL so we don't get mysterious blank pages
    (if (null obj) (http-print "<Action returned NIL>")))
      )
    (finish-output s)
    (close s)))



(defun http-error (condition)
  (list "ERROR: " condition))



(defun http-not-found (url)
  (list "404 NOT FOUND<P>The URL you requested, "
        url
        ", could not be found."))



;;;;;;;;;;;;;;;;;;
;;;
;;; HTMLisp objects
;;;
(define-method (http-emit (l list))
  (dolist (i l) (http-emit i)))



(define-method (http-emit (thing t))
  (http-print thing))



(define-method (http-emit (c condition))
  (http-print "ERROR: " c "<P>"))



;;;;;;;;;;
;;;
;;;  Hyperlink
;;;
(define-class hyperlink text url)



(define-method (http-emit (h hyperlink text url))
  (http-print (format nil "<a href=\"~A\">~A</a>" url text)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Forms
;;;
(define-class form (method 'post) (button-label 'submit)
                   action items reset-button?)



(define-method (http-emit (f form method action items
                 button-label reset-button?))
  (http-print "<FORM METHOD=" method " ACTION=" action ">")
  (dolist (item items) (http-emit item))
  (http-print "<INPUT TYPE=SUBMIT VALUE=\"" button-label "\">")
  (if reset-button? (http-print "<INPUT TYPE=RESET>"))
  (http-print "</FORM>"))



;;;;;;;;;;;;
;;;
;;; Menu (form item)
;;;
(define-class html-menu id items default)



(define-method (http-emit (m html-menu id items default))
  (http-print
   "<SELECT NAME=\""
   id
   "\">")
  (dolist (item items)
    (http-print "<OPTION ")
    (if (eq item default) (http-print "SELECTED "))
    (http-print "VALUE=\"" item "\">" item))
  (http-print "</SELECT>"))



;;;;;;;;;;;;;
;;;
;;; Text box (form item)
;;;
(define-class text-box id size maxlength password?)



(define-method (http-emit (tb text-box id size maxlength password?))
  (http-print
   "<INPUT TYPE="
   (if password? 'password 'text)
   " NAME=" id)
  (if size (http-print " SIZE=" size))
  (if maxlength (http-print " MAXLENGTH=" maxlength))
  (http-print ">"))



;;;;;;;;;;;;
;;;
;;; Text area (i.e. a big text box) (form item)
;;;
(define-class text-area id rows cols default-text (wrap? t))



(define-method (http-emit (ta text-area id rows cols default-text wrap?))
  (http-print "<TEXTAREA NAME=" id)
  (if rows (http-print " ROWS=" rows))
  (if cols (http-print " COLS=" cols))
  (if wrap? (http-print "WRAP"))
  (http-print ">")
  (if default-text (http-print default-text))
  (http-print "</TEXTAREA>"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tables
;;;
(define-class table title contents (borderwidth 1) (bordercolor 0))



(define-method (http-emit (tb table title contents borderwidth bordercolor))
  (http-print "<TABLE BORDER=" borderwidth " BORDERCOLOR=" bordercolor ">")
  (if title (http-print "<CAPTION>" title "</CAPTION>"))
  (dolist (row contents)
    (http-print "<TR><TH>" (car row))
    (dolist (item (cdr row))
      (http-print "<TD>")
      (http-emit item)))
  (http-print "</TABLE>"))



;;;;;;;;;;
;;;
;;;  A handy utility
;;;
(defmacro with-form-slots (argvar slotnames &body body)
  `(let (,@(loop for slotname in slotnames
                 collect `(,slotname (getf ,argvar ',slotname))))
     ,@body))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Example
;;;

(define-http :http-root-object
             (list
              "Welcome to the CLisp http server example page!<P>"
          "Please log in:<p>"
              (make-form
               :action 'verify-password ; Links to next object
               :items
               (list
                "User ID: "
                (make-text-box :id 'user-id)
                "<P>Password: "
                (make-text-box :id 'password :password? t)
                "<P>"))))



(define-http verify-password
    (fn (args)
    (with-form-slots args (user-id password)
      (declare (ignore password))
      (list
       "Welcome "
       user-id
       "<P>"
       (make-selector-form user-id)))))



(defvar *months* '(jan feb mar apr may jun jul aug sept oct nov dec))



(defun make-date-selector ()
  (receive (s m h date month year)
           (decode-universal-time (get-universal-time))
    (declare (ignore s m h))
    (list
     (make-html-menu :id 'month :items *months*
             :default (nth (1- month) *months*))
     (make-html-menu :id 'day :items (loop for i from 1 to 31 collect i)
             :default date)
     (make-html-menu :id 'year
             :items (loop for i from year to (+ year 5) collect i)
             :default year))))



(defun make-selector-form (id)
  (make-form
   :action 'display-schedule
   :button-label "View schedule"
   :items
   (list
    "<P>Select a date: "
    (make-date-selector)
    "<INPUT TYPE=HIDDEN NAME=USER-ID VALUE=" id "><P>")))



(define-http display-schedule
  (fn (args)
    (with-form-slots args (month day year user-id)
      (list
       "<CENTER>Schedule of all conference rooms for " month day "," year "<P>"
       (make-scheduling-form (1+ (position (intern (string-upcase month)) *months*))
                             (parse-integer day)
                             (parse-integer year))
       "</H3><P>Click on an <b>available</b> slot to sign up for a room."
       (make-selector-form user-id)))))



(defun make-scheduling-form (m d y)
  (make-table
   :contents
   (cons
    (list "" "Room 1" "Room 2" "Room 3")
    (loop for time from 6 to 23
          collect
          (cons (format nil "~A:00" time)
                (loop for room in '(room1 room2 room2)
            collect
              (make-hyperlink
               :text "AVAILABLE"
               :url `(sign-up ',room ,m ,d ,y ,time))))))))



(defun sign-up (room m d y time)
  (list
   (format nil "You are now signed up for ~A on ~A ~A, ~A at ~A<p>"
       room
       (nth m *months*)
       d y time)
   "Here's some other example stuff:<p>"
   "Please choose one of the following options:<p>"
   (make-hyperlink :text "Option 1" :url "hyperlink1")
   "<P>"
   (make-hyperlink :text "Option 2" :url "(hyperlink2 123)")
   (make-form :action 'form1
          :button-label 'update
          :items (list
              "Select some options:"
              (make-html-menu :id 'menu1
                      :items '(1 2 3 4 5)
                      :default 3)
              (make-html-menu :id 'menu2
                      :items '(a b c d e f))
              "<P>Enter some text:"
              (make-text-box :id 'text1)
              "<P>Enter a password:"
              (make-text-box :id 'password1 :password? t)
              "<P>"
              (make-text-area :id 'text-area-1 :rows 5
                      :default-text "Hello")))
   (make-table :contents '((row1 1 2 3)
               (row2 4 5 6)
               (row3 7 8 9 10 11)))
   (make-table :contents `((forms ,(make-form :action 'form2
                          :button-label 'update
                          :items (list "Form2"))
                  ,(make-form :action 'form3
                          :button-label 'update
                          :items (list "Form3"))
                  ,(make-form :action 'form4
                          :button-label 'update
                          :items (list "Form4")))))
   ))



;(define-http hyperlink1 "This is hyperlink 1")
;(defun hyperlink2 (arg) (list "Hyperlink2 with argument" arg))
;(define-http form1 "Form 1 result")
;(define-http form2 "Form 2 result")
;(define-http form3 "Form 3 result")
;(define-http form4 "Form 4 result")

