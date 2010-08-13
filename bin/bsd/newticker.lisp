#!/usr/local/bin/clisp

; rss feed scroller.
;
;Desktop Rss Feed Scroller
;
;needs : curl, xmlstarlet, clisp, xrootconsole
;
;Adjust paths to your own environment.
;create a spool file : /home/colarte/bin/spool
;cp  newticker.lisp in your PATH. (/home/colarte/bin/newticker.lisp)
;cp rss2list.sh in in your PATH. (/home/colarte/bin/rss2list.sh)
;xrootconsole /home/colarte/bin/spool to see logs in X root console 
;customize your feeds editing /home/colarte/bin/newticker.lisp in the bottom .. 
;main way is thru  macro initialize
;start feed scroll  /home/colarte/bin/newticker.lisp

(defstruct feed name url content date)
(defvar counter 0)
(defvar *feeds* '())
(defun makefeed (name url) (make-feed :url url :name name))
(defmacro init-feed (name url)
    `(push (makefeed ,name ,url) *feeds*))
(defun write-log (msg)
     (with-open-file (stream "/home/colarte/bin/spool" :direction :output :if-exists :append)
     (format stream "~a ~%" msg)))
(defun cmd-2-list (command)
    (with-open-stream (stream (make-pipe-input-stream command))
      (let ((answer '()))
        (do ((line (read-line stream nil)
            (read-line stream nil)))
            ((null line))
            (push line answer))
       answer)))
(defun update-feeds ()
    (dolist (feed *feeds*)
      (setf (feed-content feed)
            (cmd-2-list (format nil "/home/colarte/bin/rss2list.sh ~a |grep -v '^ $'" (feed-url feed))) )
      (setf (feed-date feed) (current-time nil))))
(defun show-feed (feed)
    (write-log (format nil "~a : ~a" (feed-name feed) (feed-date feed)))
    (dolist (title (feed-content feed))
      (incf counter)
      (if (eq counter 600) ; 10 minutos actualizar feeds
          (progn
            (update-feeds )
            (setf counter 0)))
      (write-log  title)
      (sleep 1)))
(defun start-scroll ()
    (update-feeds)
    (loop
       (dolist (feed *feeds*)
         (show-feed feed))))
(init-feed "cnn" "http://rss.cnn.com/rss/cnn_topstories.rss")
(init-feed "wired" "http://www.wired.com/news_drop/netcenter/netcenter.rdf")
(init-feed "reddit" "http://reddit.com/toplinks.rss")
; rss initializations useless after this point
(run-shell-command "xrootconsole /home/colarte/bin/spool &")
(start-scroll)
; (start-scroll) should be in the very last of this script.