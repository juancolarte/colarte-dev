all: html.fas httpd.fas  httpd 

html.fas:
	clisp -c html.lisp

httpd.fas:html.fas 
	clisp -i html.fas -c httpd.lisp

httpd:httpd.fas html.fas
	clisp -i html.fas -i httpd.fas -x "(ext:saveinitmem 'httpd' :init-function #'cl-webapp:threaded-server :executable t :quiet t)"

clean:
	rm -fr *.fas *.lib httpd


