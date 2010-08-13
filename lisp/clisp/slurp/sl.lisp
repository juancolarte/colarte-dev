(setf *pass-file* "/etc/passwd")

(defun slurp-stream1 (stream)
  (with-output-to-string (out)
    (do ((x (read-char stream nil stream) (read-char stream nil stream)))
        ((eq x stream))
      (write-char x out))))

(defun slurp-stream2 (stream)
  (with-output-to-string (out)
    (loop
     (multiple-value-bind (line nl) (read-line stream nil stream)
       (when (eq line stream)
         (return))
       (write-string line out)
       (unless nl
         (write-char #\Newline out))))))

(defun slurp-stream3 (stream)
  (with-output-to-string (out)
    (let ((seq (make-array 1024 :element-type 'character
                           :adjustable t
                           :fill-pointer 1024)))
      (loop
       (setf (fill-pointer seq) (read-sequence seq stream))
       (when (zerop (fill-pointer seq))
         (return))
       (write-sequence seq out)))))


(defun slurp-stream4 (stream)
  (let ((seq (make-string (file-length stream))))
    (read-sequence seq stream)
    seq))


(let ((file-pass (open *pass-file*)))

    ;(print "Testing slurp-stream1")
    ;(time (slurp-stream1 file-pass))
    ;(print "Testing slurp-stream2")
    ;(time (slurp-stream2 file-pass)) 
    ;(print "Testing slurp-stream3")
    ;(time (slurp-stream3 file-pass)) 
    (print "Testing slurp-stream4")
    (time (slurp-stream4 file-pass)) 

    )
    





#|(with-open-file (stream "/etc/passwd")
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (print line)))

 |# 
;(with-output-to-string (stream)
;    (princ "I'm writing to memory!" stream))
;returns I'm writing to memory

