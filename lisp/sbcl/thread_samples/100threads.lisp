(use-package :sb-thread)

(defparameter  *a* 0)

(defun  a-thread ()
    (make-thread 
	(lambda () 
	(loop 
	  (sleep 3)
	    (print "im up in ")
	    (print *a*)) :name "a-thread")))



(dotimes (x 100)
    (progn 
	(incf *a*)
	(print "howdy")
	(print *a*)
	(a-thread)))

