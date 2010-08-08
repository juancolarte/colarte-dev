(in-package :mt)

(defun thread-1 ()
        (print "howdy 1"))

(dotimes (x 99)
  (MAKE-THREAD #'thread-1))

