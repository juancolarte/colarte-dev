(require 'sb-bsd-sockets)
(use-package :sb-bsd-sockets)

(declaim (optimize (speed 3) (debug 0) (safety 1)))

(defun my-listen (port)
  (let ((socket (get-server-socket port))
        (output (format nil
                        "HTTP/1.1 200 OK~%~A~2%"
			"Content-Type: text/html")))
    (handler-case
        (progn
          (format t "listening on ~D~%" port)
          (loop
             for client = (get-client-socket socket)
             for stream = (get-socket-stream client)
             do (progn
                  (read-line stream)
                  (princ output stream)
                  (princ "Hello World" stream)
                  (close-socket client))))
      (condition (c) (format t "error occured: ~A~%" c)))
    (close-socket socket)))

(defun get-server-socket (port)
  (let ((socket (make-instance 'inet-socket :type :stream)))
           (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
           (socket-bind socket #(127 0 0 1) port)
           (socket-listen socket 10)
           socket))

(defun get-client-socket (server-socket)
  (socket-accept server-socket))

(defun get-socket-stream (socket)
  (socket-make-stream socket :input t :output t))

(defun close-socket (socket)
  (socket-close socket))

(my-listen 8000)
