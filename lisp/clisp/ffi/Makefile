all: magic.lisp magic.fas

magic.fas:magic.lisp
	clisp -c magic.lisp

magic.lisp: magic.i
	swig -clisp -extern-all -generate-typedef magic.i

clean:
	rm *.lib *.fas
	
