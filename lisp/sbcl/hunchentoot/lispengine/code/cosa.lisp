(in-package :lispengine)

(defun cosa ()
  (with-html-output (*standard-output*)
    (:html 
	(:table :border 0 
	  (:tr 
		(:td " eeerda  ") (:td " mi nuevo .asp ")) 
	  (:tr 
		(:td "3") (:td " en lisp carajo"))))))

(push  (create-prefix-dispatcher "/cosa" 'cosa)  hunchentoot:*dispatch-table*)




