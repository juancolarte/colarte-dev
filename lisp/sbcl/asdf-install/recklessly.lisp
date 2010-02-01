(defun asdf-install::recklessly-install (&rest packages)
    (handler-bind
	((error (lambda (condition)
	    (declare (ignore condition))
	    (or (continue)
		(and (find-restart 'asdf-install::skip-gpg-check)
		     (invoke-restart 'asdf-install::skip-gpg-check))))))
		(apply 'asdf-install:install packages)))

(export 'asdf-install::recklessly-install 'asdf-install)
