; Horizontal multi row scrooler can scroll files or commands
(defpackage :horscroll
  (:use #:common-lisp #:ncurses)
  (:export start-scrolls))

(in-package :horscroll)

;; Utility functions used by various parts of the server.
(defstruct (scrolled (:conc-name s.) (:constructor add.scroll))
        name string length row program initial offset (buffer ""))

(defun proc_commands (commands)
    (mapcar #'(lambda(x) 
		(add.scroll :name (first x) :row (second x) 
		:program (third x))) commands)) 


(defun start-scrolls (&optional commands onelinerp oneliner_row)
  (initscr)
  (let ((oneliner (if onelinerp onelinerp nil))
        (actual-scroller 0)
        (oneliner-row (if oneliner_row oneliner_row 0))
        (scroll-list (if commands 
                         (proc_commands commands)
                         (list (add.scroll :name "/etc/passwd" :row 0)
                           (add.scroll :name "top -n"         :row 2 :program t )
                           (add.scroll :name "df"             :row 4 :program t ))))
        (win-length (getmaxx *stdscr*)))
       ; let boundary
       (labels 
            ((slurp-file (filename)
                 "Dump the contents of a file into a string."
                 (with-open-file (stream filename)
                     (let ((seq (make-string (file-length stream))))
                         (read-sequence seq stream)
                       seq)))
             (showticker (scroll)
                 (if oneliner 
                     (move oneliner-row 0)
                     (move (s.row scroll) 0))
                 (waddstr *stdscr* (s.buffer scroll))
                 (refresh))
             (fill-spaces (string max-len)
                 (let ((frmstr 
                          (concatenate 
                              'string 
                              "~" (format  nil "~D" max-len) "<~a~>")))
                  (format nil frmstr string)))
             (execute-command (command)
                 (with-open-stream (stream (ext:make-pipe-input-stream command))
                     (let ((answer ""))
                          (do ((line (read-line stream nil)
                              (read-line stream nil)))
                              ((null line))
                              (setf answer (concatenate 'string answer line)))
                     answer)))
             (initialize-scroll (scroller)
                 (if (s.program scroller)
                     (setf (s.string scroller) (remove  #\newline 
                                                        (execute-command 
                                                           (s.name scroller))))
                     (setf (s.string scroller) (remove #\newline 
                                                       (slurp-file 
                                                          (s.name scroller)))))
                 (setf (s.length scroller) (length (s.string scroller)))
                 (setf (s.initial scroller) 0 (s.offset scroller) 1))
             (process-scroll (scroller)
                 (setf (s.buffer scroller) (subseq (s.string scroller) 
                                                   (s.initial scroller) 
                                                   (s.offset scroller)))
                 (if (< (- (s.offset scroller) (s.initial scroller)) win-length)
                     (setf (s.buffer scroller) 
                           (fill-spaces (s.buffer scroller) win-length)))
                 (showticker scroller)
                 (if (equal (s.length scroller) (s.offset scroller))
                    (if oneliner
                        (if (equal actual-scroller (- (length scroll-list) 1))
                            (progn
                                (setf actual-scroller 0)
                                (mapcar #'initialize-scroll scroll-list))
                            (incf actual-scroller))
                        (initialize-scroll scroller))
                     )

                 (incf (s.offset scroller))
                 (if (> (s.offset scroller) win-length) 
                     (incf (s.initial scroller))))
             (proc-all ()
                 (mapcar #'process-scroll scroll-list)
                 (sleep .12))
             (proc-oneliner ()
                (process-scroll (nth actual-scroller scroll-list)) 
                (sleep .09)))
        ;labels boundary:
        (mapcar #'initialize-scroll scroll-list)
        (noecho)
        (curs_set 0)
        (timeout 0)
        (loop
           (setf ch (getch))
           (cond 
               ((equal ch 10) 
                   (return))
               ((equal ch -1) 
                   (if oneliner 
                       (proc-oneliner)
                       (proc-all)))))
        (endwin))))

#|
  Usage : 
(start-scrolls)

(start-scrolls '(("top -n" 2 t)
                ("df" 4 t)
                ("/etc/passwd" 8)) nil 0)
|#