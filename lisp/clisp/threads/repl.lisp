(defpackage :threaded-repl
    (:use :common-lisp :mt :socket)
    (:export make-my-day))

(in-package :threaded-repl)

(defun thread-1 ()
    (let ((server (socket:socket-server 9090)))
    (format t "~&waiting for a connection on ~S:~D~%"
          (socket:socket-server-host server) (socket:socket-server-port server))
  (unwind-protect
      ;; infinite loop, terminate with control-c
      (loop (with-open-stream (socket (socket:socket-accept server))
              (multiple-value-bind (local-host local-port) (socket:socket-stream-local socket)
                (multiple-value-bind (remote-host remote-port) (socket:socket-stream-peer socket)
                  (format t "~&connection: ~S:~D -- ~S:~D~%"
                          remote-host remote-port local-host local-port)))
              ;; loop is terminated when the remote host closes the connection or on ext:exit
              (loop (when (eq :eof (socket:socket-status (cons socket :input))) (return))

                    (print (ignore-errors (eval (read socket))) socket)

                    ;; flush everything left in socket
                    (loop :for c = (read-char-no-hang socket nil nil) :while c)
                    (terpri socket))))
    ;; make sure server is closed
    (socket:socket-server-close server))))

(defun make-my-day ()
    (make-thread #'thread-1))