(in-package :mt)

(defun this ()
    (print "howdY"))

(defun that ()
    (print "hO"))

(defun thread-1 ()
  (loop
    (this)
    (sleep 3)))

(defun thread-2 ()
  (loop
    (that)
    (sleep 3)))


(defvar *thread1* (MAKE-THREAD #'thread-1))
(defvar *thread2* (MAKE-THREAD #'thread-2))

  ;(MAKE-THREAD #'thread-1 :initial-bindings
  ;'((*a* . 1) (print "me"))  ))

