all: ncurses.fas ncurses.bin

ncurses.bin:ncurses.fas
	clisp -M withcffi.mem -i ncurses.fas -x "(saveinitmem 'ncurses.bin' :executable t :quiet t)"


ncurses.fas:
	clisp -M withcffi.mem -c ncurses.lisp

