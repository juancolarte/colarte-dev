(in-package :cl-webapp)

(defclass <hallo> (<application>) ())
(defwebapp <hallo>
    :name "Somple Application - 1"
    :base-url "/hallo")

(defmethod page-view ((app <hello>) (req <request>))
    "Hallo Application"
    (write-line  
	(gen-html-page 
	    "Sample Machination" 
	     (h1 (format nil "~A" (param-of req)) ))))
