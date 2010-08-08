(defpackage :ncurses
  (:use :common-lisp :cffi)
  (:export 
   #:addstr
   #:cleanup
   #:clear
   #:clrtoeol
   #:COLS
   #:getch
   #:init
   #:init_pair
   #:LINES
   #:move
   #:newwin
   #:refresh
   #:waddstr
   #:wmove
   #:wrefresh
   #:wclear
   #:printw

   #:*stdscr*
   #:initscr
   #:curs_set
   #:timeout
   #:noecho
   #:getmaxx
   #:endwin
   #:getch
   
   #:A_NORMAL
   #:A_ATTRIBUTES
   #:A_CHARTEXT	
   #:A_COLOR
   #:A_STANDOUT
   #:A_UNDERLINE
   #:A_REVERSE
   #:A_BLINK
   #:A_DIM 
   #:A_BOLD
   #:COLOR_BLACK
   #:COLOR_RED
   #:COLOR_GREEN
   #:COLOR_YELLOW
   #:COLOR_BLUE
   #:COLOR_MAGENTA
   #:COLOR_CYAN
   #:COLOR_WHITE))

(in-package :ncurses)

(defconstant NCURSES_ATTR_SHIFT       8)
(defmacro NCURSES_BITS (mask shift) `(ash ,mask (+ ,shift NCURSES_ATTR_SHIFT)))

(defconstant A_NORMAL	0)
;(defconstant A_ATTRIBUTES	NCURSES_BITS(~(1UL - 1UL),0))
;(defconstant A_CHARTEXT	(NCURSES_BITS(1UL,0) - 1UL))
;(defconstant A_COLOR		NCURSES_BITS(((1UL) << 8) - 1UL,0))
(defconstant A_STANDOUT	 (NCURSES_BITS 1 8))
(defconstant A_UNDERLINE (NCURSES_BITS 1 9))
(defconstant A_REVERSE	 (NCURSES_BITS 1 10))
(defconstant A_BLINK	 (NCURSES_BITS 1 11))
(defconstant A_DIM     	 (NCURSES_BITS 1 12))
(defconstant A_BOLD 	 (NCURSES_BITS 1 13))
#|
#define A_ALTCHARSET	NCURSES_BITS(1UL,14)
#define A_INVIS		NCURSES_BITS(1UL,15)
#define A_PROTECT	NCURSES_BITS(1UL,16)
#define A_HORIZONTAL	NCURSES_BITS(1UL,17)
#define A_LEFT		NCURSES_BITS(1UL,18)
#define A_LOW		NCURSES_BITS(1UL,19)
#define A_RIGHT		NCURSES_BITS(1UL,20)
#define A_TOP		NCURSES_BITS(1UL,21)
#define A_VERTICAL	NCURSES_BITS(1UL,22)

#define COLOR_PAIR(n)	NCURSES_BITS(n, 0)
#define PAIR_NUMBER(a)	((int)(((a) & A_COLOR) >> NCURSES_ATTR_SHIFT))
|#

(define-foreign-library libncurses
 (t (:default "libncurses")))

(use-foreign-library libncurses)

(defcvar ("stdscr" *stdscr*) :pointer)
(defcvar ("curscr" *curscr*) :pointer)
(defcvar ("newscr" *newscr*) :pointer)
(defcvar ("LINES" LINES) :int)
(defcvar ("COLS" COLS) :int)
(defcvar ("TABSIZE" TABSIZE) :int)

(defun move (y x) (wmove *stdscr* y x))
(defun addstr (str) (waddstr *stdscr* str))
(defun refresh () (wrefresh *stdscr*))
(defun clear () (wclear *stdscr*))

(defun init ()
 (if (null-pointer-p *stdscr*)
  (initscr)
  (refresh))
 (keypad *stdscr* 1)
 (raw)
 (noecho)
 (move 0 0))

(defun cleanup ()
 (endwin))

;;;SWIG wrapper code starts here
(cl:defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(cl:progn ,@(cl:loop for value in enums
                        for index = 0 then (cl:1+ index)
                        when (cl:listp value) do (cl:setf index (cl:second value)
                                                          value (cl:first value))
                        collect `(cl:defconstant ,value ,index))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'swig-lispify)
    (cl:defun swig-lispify (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 ((lower digit) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit 
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c)))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t ""))))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list name) cl:nil cl:nil))
            fix)
           package))))))

;;;SWIG wrapper code ends here

(cffi:defcvar ("COLORS" COLORS)
 :int)

(cffi:defcvar ("COLOR_PAIRS" COLOR_PAIRS)
 :int)

(cl:defconstant COLOR_BLACK 0)

(cl:defconstant COLOR_RED 1)

(cl:defconstant COLOR_GREEN 2)

(cl:defconstant COLOR_YELLOW 3)

(cl:defconstant COLOR_BLUE 4)

(cl:defconstant COLOR_MAGENTA 5)

(cl:defconstant COLOR_CYAN 6)

(cl:defconstant COLOR_WHITE 7)

(cffi:defcstruct cchar_t
	(attr :pointer)
	(chars :pointer))

(cffi:defcstruct _win_st
	(_cury :pointer)
	(_curx :pointer)
	(_maxy :pointer)
	(_maxx :pointer)
	(_begy :pointer)
	(_begx :pointer)
	(_flags :short)
	(_attrs :pointer)
	(_bkgd :int)
	(_notimeout :int)
	(_clear :int)
	(_leaveok :int)
	(_scroll :int)
	(_idlok :int)
	(_idcok :int)
	(_immed :int)
	(_sync :int)
	(_use_keypad :int)
	(_delay :int)
	(_line :pointer)
	(_regtop :pointer)
	(_regbottom :pointer)
	(_parx :int)
	(_pary :int)
	(_parent :pointer)
	(_yoffset :pointer)
	(_pad :pointer))

(cffi:defcstruct _win_st__pad
	(_pad_y :pointer)
	(_pad_x :pointer)
	(_pad_top :pointer)
	(_pad_left :pointer)
	(_pad_bottom :pointer)
	(_pad_right :pointer))

(cffi:defcfun ("curses_version" curses_version) :string)

(cffi:defcfun ("addch" addch) :int
  (arg0 :int))

(cffi:defcfun ("addchnstr" addchnstr) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("addchstr" addchstr) :int
  (arg0 :pointer))

(cffi:defcfun ("addnstr" addnstr) :int
  (arg0 :string)
  (arg1 :int))

(cffi:defcfun ("addstr" addstr) :int
  (arg0 :string))

(cffi:defcfun ("attroff" attroff) :int
  (arg0 :int))

(cffi:defcfun ("attron" attron) :int
  (arg0 :int))

(cffi:defcfun ("attrset" attrset) :int
  (arg0 :int))

(cffi:defcfun ("attr_get" attr_get) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer))

(cffi:defcfun ("attr_off" attr_off) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("attr_on" attr_on) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("attr_set" attr_set) :int
  (arg0 :pointer)
  (arg1 :short)
  (arg2 :pointer))

(cffi:defcfun ("baudrate" baudrate) :int)

(cffi:defcfun ("beep" beep) :int)

(cffi:defcfun ("bkgd" bkgd) :int
  (arg0 :int))

(cffi:defcfun ("bkgdset" bkgdset) :void
  (arg0 :int))

(cffi:defcfun ("border" border) :int
  (arg0 :int)
  (arg1 :int)
  (arg2 :int)
  (arg3 :int)
  (arg4 :int)
  (arg5 :int)
  (arg6 :int)
  (arg7 :int))

(cffi:defcfun ("box" box) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int))

(cffi:defcfun ("can_change_color" can_change_color) :int)

(cffi:defcfun ("cbreak" cbreak) :int)

(cffi:defcfun ("chgat" chgat) :int
  (arg0 :int)
  (arg1 :pointer)
  (arg2 :short)
  (arg3 :pointer))

(cffi:defcfun ("clearok" clearok) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("clrtobot" clrtobot) :int)

(cffi:defcfun ("clrtoeol" clrtoeol) :int)

(cffi:defcfun ("color_content" color_content) :int
  (arg0 :short)
  (arg1 :pointer)
  (arg2 :pointer)
  (arg3 :pointer))

(cffi:defcfun ("color_set" color_set) :int
  (arg0 :short)
  (arg1 :pointer))

(cffi:defcfun ("COLOR_PAIR" COLOR_PAIR) :int
  (arg0 :int))

(cffi:defcfun ("copywin" copywin) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :int)
  (arg3 :int)
  (arg4 :int)
  (arg5 :int)
  (arg6 :int)
  (arg7 :int)
  (arg8 :int))

(cffi:defcfun ("curs_set" curs_set) :int
  (arg0 :int))

(cffi:defcfun ("def_prog_mode" def_prog_mode) :int)

(cffi:defcfun ("def_shell_mode" def_shell_mode) :int)

(cffi:defcfun ("delay_output" delay_output) :int
  (arg0 :int))

(cffi:defcfun ("delch" delch) :int)

(cffi:defcfun ("delscreen" delscreen) :void
  (arg0 :pointer))

(cffi:defcfun ("delwin" delwin) :int
  (arg0 :pointer))

(cffi:defcfun ("deleteln" deleteln) :int)

(cffi:defcfun ("derwin" derwin) :pointer
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :int)
  (arg4 :int))

(cffi:defcfun ("doupdate" doupdate) :int)

(cffi:defcfun ("dupwin" dupwin) :pointer
  (arg0 :pointer))

(cffi:defcfun ("echo" echo) :int)

(cffi:defcfun ("echochar" echochar) :int
  (arg0 :int))

(cffi:defcfun ("erase" erase) :int)

(cffi:defcfun ("endwin" endwin) :int)

(cffi:defcfun ("erasechar" erasechar) :char)

(cffi:defcfun ("filter" filter) :void)

(cffi:defcfun ("flash" flash) :int)

(cffi:defcfun ("flushinp" flushinp) :int)

(cffi:defcfun ("getbkgd" getbkgd) :int
  (arg0 :pointer))

(cffi:defcfun ("getch" getch) :int)

(cffi:defcfun ("getnstr" getnstr) :int
  (arg0 :string)
  (arg1 :int))

(cffi:defcfun ("getstr" getstr) :int
  (arg0 :string))

(cffi:defcfun ("getwin" getwin) :pointer
  (arg0 :pointer))

(cffi:defcfun ("halfdelay" halfdelay) :int
  (arg0 :int))

(cffi:defcfun ("has_colors" has_colors) :int)

(cffi:defcfun ("has_ic" has_ic) :int)

(cffi:defcfun ("has_il" has_il) :int)

(cffi:defcfun ("hline" hline) :int
  (arg0 :int)
  (arg1 :int))

(cffi:defcfun ("idcok" idcok) :void
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("idlok" idlok) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("immedok" immedok) :void
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("inch" inch) :int)

(cffi:defcfun ("inchnstr" inchnstr) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("inchstr" inchstr) :int
  (arg0 :pointer))

(cffi:defcfun ("initscr" initscr) :pointer)

(cffi:defcfun ("init_color" init_color) :int
  (arg0 :short)
  (arg1 :short)
  (arg2 :short)
  (arg3 :short))

(cffi:defcfun ("init_pair" init_pair) :int
  (arg0 :short)
  (arg1 :short)
  (arg2 :short))

(cffi:defcfun ("innstr" innstr) :int
  (arg0 :string)
  (arg1 :int))

(cffi:defcfun ("insch" insch) :int
  (arg0 :int))

(cffi:defcfun ("insdelln" insdelln) :int
  (arg0 :int))

(cffi:defcfun ("insertln" insertln) :int)

(cffi:defcfun ("insnstr" insnstr) :int
  (arg0 :string)
  (arg1 :int))

(cffi:defcfun ("insstr" insstr) :int
  (arg0 :string))

(cffi:defcfun ("instr" instr) :int
  (arg0 :string))

(cffi:defcfun ("intrflush" intrflush) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("isendwin" isendwin) :int)

(cffi:defcfun ("is_linetouched" is_linetouched) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("is_wintouched" is_wintouched) :int
  (arg0 :pointer))

(cffi:defcfun ("keyname" keyname) :string
  (arg0 :int))

(cffi:defcfun ("keypad" keypad) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("killchar" killchar) :char)

(cffi:defcfun ("leaveok" leaveok) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("longname" longname) :string)

(cffi:defcfun ("meta" meta) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("move" move) :int
  (arg0 :int)
  (arg1 :int))

(cffi:defcfun ("mvaddch" mvaddch) :int
  (arg0 :int)
  (arg1 :int)
  (arg2 :int))

(cffi:defcfun ("mvaddchnstr" mvaddchnstr) :int
  (arg0 :int)
  (arg1 :int)
  (arg2 :pointer)
  (arg3 :int))

(cffi:defcfun ("mvaddchstr" mvaddchstr) :int
  (arg0 :int)
  (arg1 :int)
  (arg2 :pointer))

(cffi:defcfun ("mvaddnstr" mvaddnstr) :int
  (arg0 :int)
  (arg1 :int)
  (arg2 :string)
  (arg3 :int))

(cffi:defcfun ("mvaddstr" mvaddstr) :int
  (arg0 :int)
  (arg1 :int)
  (arg2 :string))

(cffi:defcfun ("mvchgat" mvchgat) :int
  (arg0 :int)
  (arg1 :int)
  (arg2 :int)
  (arg3 :pointer)
  (arg4 :short)
  (arg5 :pointer))

(cffi:defcfun ("mvcur" mvcur) :int
  (arg0 :int)
  (arg1 :int)
  (arg2 :int)
  (arg3 :int))

(cffi:defcfun ("mvdelch" mvdelch) :int
  (arg0 :int)
  (arg1 :int))

(cffi:defcfun ("mvderwin" mvderwin) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int))

(cffi:defcfun ("mvgetch" mvgetch) :int
  (arg0 :int)
  (arg1 :int))

(cffi:defcfun ("mvgetnstr" mvgetnstr) :int
  (arg0 :int)
  (arg1 :int)
  (arg2 :string)
  (arg3 :int))

(cffi:defcfun ("mvgetstr" mvgetstr) :int
  (arg0 :int)
  (arg1 :int)
  (arg2 :string))

(cffi:defcfun ("mvhline" mvhline) :int
  (arg0 :int)
  (arg1 :int)
  (arg2 :int)
  (arg3 :int))

(cffi:defcfun ("mvinch" mvinch) :int
  (arg0 :int)
  (arg1 :int))

(cffi:defcfun ("mvinchnstr" mvinchnstr) :int
  (arg0 :int)
  (arg1 :int)
  (arg2 :pointer)
  (arg3 :int))

(cffi:defcfun ("mvinchstr" mvinchstr) :int
  (arg0 :int)
  (arg1 :int)
  (arg2 :pointer))

(cffi:defcfun ("mvinnstr" mvinnstr) :int
  (arg0 :int)
  (arg1 :int)
  (arg2 :string)
  (arg3 :int))

(cffi:defcfun ("mvinsch" mvinsch) :int
  (arg0 :int)
  (arg1 :int)
  (arg2 :int))

(cffi:defcfun ("mvinsnstr" mvinsnstr) :int
  (arg0 :int)
  (arg1 :int)
  (arg2 :string)
  (arg3 :int))

(cffi:defcfun ("mvinsstr" mvinsstr) :int
  (arg0 :int)
  (arg1 :int)
  (arg2 :string))

(cffi:defcfun ("mvinstr" mvinstr) :int
  (arg0 :int)
  (arg1 :int)
  (arg2 :string))

(cffi:defcfun ("mvvline" mvvline) :int
  (arg0 :int)
  (arg1 :int)
  (arg2 :int)
  (arg3 :int))

(cffi:defcfun ("mvwaddch" mvwaddch) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :int))

(cffi:defcfun ("mvwaddchnstr" mvwaddchnstr) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :pointer)
  (arg4 :int))

(cffi:defcfun ("mvwaddchstr" mvwaddchstr) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :pointer))

(cffi:defcfun ("mvwaddnstr" mvwaddnstr) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :string)
  (arg4 :int))

(cffi:defcfun ("mvwaddstr" mvwaddstr) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :string))

(cffi:defcfun ("mvwchgat" mvwchgat) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :int)
  (arg4 :pointer)
  (arg5 :short)
  (arg6 :pointer))

(cffi:defcfun ("mvwdelch" mvwdelch) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int))

(cffi:defcfun ("mvwgetch" mvwgetch) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int))

(cffi:defcfun ("mvwgetnstr" mvwgetnstr) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :string)
  (arg4 :int))

(cffi:defcfun ("mvwgetstr" mvwgetstr) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :string))

(cffi:defcfun ("mvwhline" mvwhline) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :int)
  (arg4 :int))

(cffi:defcfun ("mvwin" mvwin) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int))

(cffi:defcfun ("mvwinch" mvwinch) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int))

(cffi:defcfun ("mvwinchnstr" mvwinchnstr) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :pointer)
  (arg4 :int))

(cffi:defcfun ("mvwinchstr" mvwinchstr) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :pointer))

(cffi:defcfun ("mvwinnstr" mvwinnstr) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :string)
  (arg4 :int))

(cffi:defcfun ("mvwinsch" mvwinsch) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :int))

(cffi:defcfun ("mvwinsnstr" mvwinsnstr) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :string)
  (arg4 :int))

(cffi:defcfun ("mvwinsstr" mvwinsstr) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :string))

(cffi:defcfun ("mvwinstr" mvwinstr) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :string))

(cffi:defcfun ("mvwvline" mvwvline) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :int)
  (arg4 :int))

(cffi:defcfun ("napms" napms) :int
  (arg0 :int))

(cffi:defcfun ("newpad" newpad) :pointer
  (arg0 :int)
  (arg1 :int))

(cffi:defcfun ("newterm" newterm) :pointer
  (arg0 :string)
  (arg1 :pointer)
  (arg2 :pointer))

(cffi:defcfun ("newwin" newwin) :pointer
  (arg0 :int)
  (arg1 :int)
  (arg2 :int)
  (arg3 :int))

(cffi:defcfun ("nl" nl) :int)

(cffi:defcfun ("nocbreak" nocbreak) :int)

(cffi:defcfun ("nodelay" nodelay) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("noecho" noecho) :int)

(cffi:defcfun ("nonl" nonl) :int)

(cffi:defcfun ("noqiflush" noqiflush) :void)

(cffi:defcfun ("noraw" noraw) :int)

(cffi:defcfun ("notimeout" notimeout) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("overlay" overlay) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("overwrite" overwrite) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("pair_content" pair_content) :int
  (arg0 :short)
  (arg1 :pointer)
  (arg2 :pointer))

(cffi:defcfun ("PAIR_NUMBER" PAIR_NUMBER) :int
  (arg0 :int))

(cffi:defcfun ("pechochar" pechochar) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("pnoutrefresh" pnoutrefresh) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :int)
  (arg4 :int)
  (arg5 :int)
  (arg6 :int))

(cffi:defcfun ("prefresh" prefresh) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :int)
  (arg4 :int)
  (arg5 :int)
  (arg6 :int))

(cffi:defcfun ("putp" putp) :int
  (arg0 :string))

(cffi:defcfun ("putwin" putwin) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("qiflush" qiflush) :void)

(cffi:defcfun ("raw" raw) :int)

(cffi:defcfun ("redrawwin" redrawwin) :int
  (arg0 :pointer))

(cffi:defcfun ("refresh" refresh) :int)

(cffi:defcfun ("resetty" resetty) :int)

(cffi:defcfun ("reset_prog_mode" reset_prog_mode) :int)

(cffi:defcfun ("reset_shell_mode" reset_shell_mode) :int)

(cffi:defcfun ("ripoffline" ripoffline) :int
  (arg0 :int)
  (arg1 :pointer))

(cffi:defcfun ("savetty" savetty) :int)

(cffi:defcfun ("scr_dump" scr_dump) :int
  (arg0 :string))

(cffi:defcfun ("scr_init" scr_init) :int
  (arg0 :string))

(cffi:defcfun ("scrl" scrl) :int
  (arg0 :int))

(cffi:defcfun ("scroll" scroll) :int
  (arg0 :pointer))

(cffi:defcfun ("scrollok" scrollok) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("scr_restore" scr_restore) :int
  (arg0 :string))

(cffi:defcfun ("scr_set" scr_set) :int
  (arg0 :string))

(cffi:defcfun ("setscrreg" setscrreg) :int
  (arg0 :int)
  (arg1 :int))

(cffi:defcfun ("set_term" set_term) :pointer
  (arg0 :pointer))

(cffi:defcfun ("slk_attroff" slk_attroff) :int
  (arg0 :int))

(cffi:defcfun ("slk_attr_off" slk_attr_off) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("slk_attron" slk_attron) :int
  (arg0 :int))

(cffi:defcfun ("slk_attr_on" slk_attr_on) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("slk_attrset" slk_attrset) :int
  (arg0 :int))

(cffi:defcfun ("slk_attr" slk_attr) :pointer)

(cffi:defcfun ("slk_attr_set" slk_attr_set) :int
  (arg0 :pointer)
  (arg1 :short)
  (arg2 :pointer))

(cffi:defcfun ("slk_clear" slk_clear) :int)

(cffi:defcfun ("slk_color" slk_color) :int
  (arg0 :short))

(cffi:defcfun ("slk_init" slk_init) :int
  (arg0 :int))

(cffi:defcfun ("slk_label" slk_label) :string
  (arg0 :int))

(cffi:defcfun ("slk_noutrefresh" slk_noutrefresh) :int)

(cffi:defcfun ("slk_refresh" slk_refresh) :int)

(cffi:defcfun ("slk_restore" slk_restore) :int)

(cffi:defcfun ("slk_set" slk_set) :int
  (arg0 :int)
  (arg1 :string)
  (arg2 :int))

(cffi:defcfun ("slk_touch" slk_touch) :int)

(cffi:defcfun ("standout" standout) :int)

(cffi:defcfun ("standend" standend) :int)

(cffi:defcfun ("start_color" start_color) :int)

(cffi:defcfun ("subpad" subpad) :pointer
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :int)
  (arg4 :int))

(cffi:defcfun ("subwin" subwin) :pointer
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :int)
  (arg4 :int))

(cffi:defcfun ("syncok" syncok) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("termattrs" termattrs) :int)

(cffi:defcfun ("termname" termname) :string)

(cffi:defcfun ("tigetflag" tigetflag) :int
  (arg0 :string))

(cffi:defcfun ("tigetnum" tigetnum) :int
  (arg0 :string))

(cffi:defcfun ("tigetstr" tigetstr) :string
  (arg0 :string))

(cffi:defcfun ("timeout" timeout) :void
  (arg0 :int))

(cffi:defcfun ("touchline" touchline) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int))

(cffi:defcfun ("touchwin" touchwin) :int
  (arg0 :pointer))

(cffi:defcfun ("typeahead" typeahead) :int
  (arg0 :int))

(cffi:defcfun ("ungetch" ungetch) :int
  (arg0 :int))

(cffi:defcfun ("untouchwin" untouchwin) :int
  (arg0 :pointer))

(cffi:defcfun ("use_env" use_env) :void
  (arg0 :int))

(cffi:defcfun ("vidattr" vidattr) :int
  (arg0 :int))

(cffi:defcfun ("vidputs" vidputs) :int
  (arg0 :int)
  (arg1 :pointer))

(cffi:defcfun ("vline" vline) :int
  (arg0 :int)
  (arg1 :int))

(cffi:defcfun ("vwprintw" vwprintw) :int
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("vw_printw" vw_printw) :int
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("vwscanw" vwscanw) :int
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("vw_scanw" vw_scanw) :int
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :pointer))

(cffi:defcfun ("waddch" waddch) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("waddchnstr" waddchnstr) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :int))

(cffi:defcfun ("waddchstr" waddchstr) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("waddnstr" waddnstr) :int
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :int))

(cffi:defcfun ("waddstr" waddstr) :int
  (arg0 :pointer)
  (arg1 :string))

(cffi:defcfun ("wattron" wattron) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("wattroff" wattroff) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("wattrset" wattrset) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("wattr_get" wattr_get) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer)
  (arg3 :pointer))

(cffi:defcfun ("wattr_on" wattr_on) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer))

(cffi:defcfun ("wattr_off" wattr_off) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer))

(cffi:defcfun ("wattr_set" wattr_set) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :short)
  (arg3 :pointer))

(cffi:defcfun ("wbkgd" wbkgd) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("wbkgdset" wbkgdset) :void
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("wborder" wborder) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :int)
  (arg4 :int)
  (arg5 :int)
  (arg6 :int)
  (arg7 :int)
  (arg8 :int))

(cffi:defcfun ("wchgat" wchgat) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :pointer)
  (arg3 :short)
  (arg4 :pointer))

(cffi:defcfun ("wclear" wclear) :int
  (arg0 :pointer))

(cffi:defcfun ("wclrtobot" wclrtobot) :int
  (arg0 :pointer))

(cffi:defcfun ("wclrtoeol" wclrtoeol) :int
  (arg0 :pointer))

(cffi:defcfun ("wcolor_set" wcolor_set) :int
  (arg0 :pointer)
  (arg1 :short)
  (arg2 :pointer))

(cffi:defcfun ("wcursyncup" wcursyncup) :void
  (arg0 :pointer))

(cffi:defcfun ("wdelch" wdelch) :int
  (arg0 :pointer))

(cffi:defcfun ("wdeleteln" wdeleteln) :int
  (arg0 :pointer))

(cffi:defcfun ("wechochar" wechochar) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("werase" werase) :int
  (arg0 :pointer))

(cffi:defcfun ("wgetch" wgetch) :int
  (arg0 :pointer))

(cffi:defcfun ("wgetnstr" wgetnstr) :int
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :int))

(cffi:defcfun ("wgetstr" wgetstr) :int
  (arg0 :pointer)
  (arg1 :string))

(cffi:defcfun ("whline" whline) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int))

(cffi:defcfun ("winch" winch) :int
  (arg0 :pointer))

(cffi:defcfun ("winchnstr" winchnstr) :int
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :int))

(cffi:defcfun ("winchstr" winchstr) :int
  (arg0 :pointer)
  (arg1 :pointer))

(cffi:defcfun ("winnstr" winnstr) :int
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :int))

(cffi:defcfun ("winsch" winsch) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("winsdelln" winsdelln) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("winsertln" winsertln) :int
  (arg0 :pointer))

(cffi:defcfun ("winsnstr" winsnstr) :int
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :int))

(cffi:defcfun ("winsstr" winsstr) :int
  (arg0 :pointer)
  (arg1 :string))

(cffi:defcfun ("winstr" winstr) :int
  (arg0 :pointer)
  (arg1 :string))

(cffi:defcfun ("wmove" wmove) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int))

(cffi:defcfun ("wnoutrefresh" wnoutrefresh) :int
  (arg0 :pointer))

(cffi:defcfun ("wredrawln" wredrawln) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int))

(cffi:defcfun ("wrefresh" wrefresh) :int
  (arg0 :pointer))

(cffi:defcfun ("wscrl" wscrl) :int
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("wsetscrreg" wsetscrreg) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int))

(cffi:defcfun ("wstandout" wstandout) :int
  (arg0 :pointer))

(cffi:defcfun ("wstandend" wstandend) :int
  (arg0 :pointer))

(cffi:defcfun ("wsyncdown" wsyncdown) :void
  (arg0 :pointer))

(cffi:defcfun ("wsyncup" wsyncup) :void
  (arg0 :pointer))

(cffi:defcfun ("wtimeout" wtimeout) :void
  (arg0 :pointer)
  (arg1 :int))

(cffi:defcfun ("wtouchln" wtouchln) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int)
  (arg3 :int))

(cffi:defcfun ("wvline" wvline) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :int))

(cffi:defcfun ("assume_default_colors" assume_default_colors) :int
  (arg0 :int)
  (arg1 :int))

(cffi:defcfun ("getattrs" getattrs) :int
  (arg0 :pointer))

(cffi:defcfun ("getcurx" getcurx) :int
  (arg0 :pointer))

(cffi:defcfun ("getcury" getcury) :int
  (arg0 :pointer))

(cffi:defcfun ("getbegx" getbegx) :int
  (arg0 :pointer))

(cffi:defcfun ("getbegy" getbegy) :int
  (arg0 :pointer))

(cffi:defcfun ("getmaxx" getmaxx) :int
  (arg0 :pointer))

(cffi:defcfun ("getmaxy" getmaxy) :int
  (arg0 :pointer))

(cffi:defcfun ("getparx" getparx) :int
  (arg0 :pointer))

(cffi:defcfun ("getpary" getpary) :int
  (arg0 :pointer))

(defmacro with-ncurses (&body body)
  `(unwind-protect 
       (progn
         (init)
         ,@body)
     (cleanup)))
