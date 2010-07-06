(defpackage :threads-engine
  (:nicknames :threaden)
  (:use :cl :sb-thread)
  (:export :show-log))

(in-package :threaden)

(defvar *log* nil)

(defstruct thread-log threadname time)

(defun log-thread (name)
 (push  (make-thread-log :threadname name :time (get-universal-time)) *log*))

(defun show-log ()
    (format nil "~A ~2%" *log*))

(defmacro in-thread (&body body)
    `(sb-thread:make-thread (lambda () ,@body)))

(defvar *threads* nil)

(defclass component ()
  ((name :accessor name :initarg :name)
   (component-log :accessor component-log)
   (working-thread :accessor w-thread)))

(defmethod component-dispatch ((obj component))
  (setf (w-thread obj)
    (in-thread
      (loop
        (log-thread (name obj))
        (sleep 4)))))

(defun make-component-object (name)
  (let ((a-component (make-instance 'component :name name)))
      ;;;
       (component-dispatch a-component)
       (push a-component *threads*)))

(make-component-object "thread 1 ")
(make-component-object "thread 2 ")
(make-component-object "thread 3 ")