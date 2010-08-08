(in-package :mt)

(defvar list-val '(0))

(defun thread-1 ()
  (let ((my-val (first list-val)))
      (loop
         (print "howdy")
         (print my-val)
        (sleep 3))))

(dotimes (x 3)
  (push x list-val)
  (MAKE-THREAD #'thread-1 ))


  ;(MAKE-THREAD #'thread-1 :initial-bindings
  ;'((*a* . 1) (print "me"))  ))

