(load "ncurses")
(in-package :ncurses)

(initscr)
(raw)
(noecho)
(format t "~A~%" (getch))
(format t "~A~%" (code-char 3))
(with-ncurses
 (waddstr *stdscr* "hello")
 (wrefresh *stdscr*)
 (wmove *stdscr* 1 5))
(endwin)

(format t "~A~%" LINES)

(init)
(cleanup)

(defun translate-key-to-SEP (key-num)
  (cond
    ((< 0 key-num 27) (concatenate 'string "\\C" (string (code-char (+ 96 key-num)))))
    (t (string (code-char key-num)))))

(with-ncurses 
 (let ((key (getch)))
 (format t "~A:~A~%" (translate-key-to-SEP key) key)))

(defmacro with-ncurses (&body body)
  `(unwind-protect 
	(progn
	  (init)
	  ,@body)
     (cleanup)))
