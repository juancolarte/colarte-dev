(in-package :lispengine)

(defun formin ()
  (let ((string1 (parameter "new-item-title")))

  (with-html-output-to-string (*standard-output* nil)
    (:form :method :post
    (:table
    (:input :type :hidden :name "item-id" )
	(:tr
      (:td
       (:input :type :text
        :name "new-item-title" )))
     (:tr
      (:td
       (:input :type :text
        :name "new-item-description" :value string1 ))
      (:td
       (:input :type :text
        :name "new-item-file"))
      (:td (:input :type :submit :name "update-item" ))))))))
      
(push  (create-prefix-dispatcher "/formin" 'formin)  hunchentoot:*dispatch-table*)
