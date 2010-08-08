all: html.fas httpd.fas httpd 

html.fas: 
	clisp -c html.lisp

httpd.fas: html.fas
	clisp -i html.fas -c httpd.lisp

httpd: httpd.fas 
	clisp -i html.fas -i httpd.fas -x "(ext:saveinitmem 'httpd' :init-function #'httpd:start :executable t)"

run: 
	clisp -E C  -i html.lisp -i httpd.lisp -x "(httpd:start)"

clean:
	rm -fr *.fas *.lib httpd

