#! /usr/local/bin/python

# binclock
# written by Brian Gajdos <brian@linuxee.sk>, 2001

import curses
import math
import os
import sys
import time

stdscr = None

pause = 0
toggle_digital = 0

def draw_led(co, x, y, status):
    global stdscr
    if status:
	if co == 2:
	    stdscr.addstr(y, x, "*", curses.A_BOLD | curses.color_pair(12))
	else:
	    stdscr.addstr(y, x, "*", curses.A_BOLD | curses.color_pair(10))
    else:
	stdscr.addstr(y, x, "*", curses.A_BOLD | curses.color_pair(11))

def draw_column(co, x, y, cislo):
    draw_led(co, x, y, cislo & 8)
    draw_led(co, x, y+1, cislo & 4)
    draw_led(co, x, y+2, cislo & 2)
    draw_led(co, x, y+3, cislo & 1)

def recalc_coords( width, height ):
    global toggle_digital
    global pos_hrs_x, pos_homi_x, pos_min_x, pos_mise_x, pos_sec_x
    global pos_seho_x, pos_hds_x, pos_dig_y
    global pos_hrs_y, pos_min_y, pos_sec_y, pos_hds_y
    global window_is_too_small

    inner_width = 0
    inner_height = 0
    
    if toggle_digital:
	inner_height = inner_height + 2

    window_is_too_small = 0

    if inner_width > width or inner_height > height:
	window_is_too_small = 1
	return

    off_x = 69
    off_y = 0

    pos_hrs_x = off_x
    pos_homi_x = off_x +2
    pos_min_x = off_x + 3
    pos_mise_x = off_x +5
    pos_sec_x = off_x + 6
    pos_seho_x = off_x + 8
    pos_hds_x = off_x + 9
    
    pos_hrs_y = pos_min_y = pos_sec_y = pos_hds_y = off_y
    
    if toggle_digital:
	pos_dig_y = pos_hrs_y + 3


def draw_watch(year, month, day, hour, minute, second, weekday, hds):
    global stdscr
    global toggle_digital
    global pos_hrs_x, pos_homi_x, pos_min_x, pos_mise_x, pos_sec_x
    global pos_seho_x, pos_hds_x, pos_dig_y
    global pos_hrs_y, pos_min_y, pos_sec_y, pos_hds_y

    stdscr.erase()
    
    if window_is_too_small:
	stdscr.refresh()
	return

    draw_column(1, pos_hrs_x,     pos_hrs_y, hour / 10)
    draw_column(1, pos_hrs_x + 1, pos_hrs_y, hour % 10)
    draw_column(1, pos_min_x,     pos_min_y, minute / 10)
    draw_column(1, pos_min_x + 1, pos_min_y, minute % 10)
    draw_column(1, pos_sec_x,     pos_sec_y, second / 10)
    draw_column(1, pos_sec_x + 1, pos_sec_y, second % 10)
    draw_column(2, pos_hds_x,     pos_hds_y, hds / 10)
    draw_column(2, pos_hds_x + 1, pos_hds_y, hds % 10)

    if toggle_digital:
	stdscr.addstr(pos_dig_y, pos_hrs_x, "%02d" % (
	    hour), curses.A_BOLD | curses.color_pair(13) )
	stdscr.addstr(pos_dig_y, pos_homi_x, ":", curses.A_NORMAL | curses.color_pair(13) )
	stdscr.addstr(pos_dig_y, pos_min_x, "%02d" % (
	    minute), curses.A_BOLD | curses.color_pair(13) )
	stdscr.addstr(pos_dig_y, pos_mise_x, ":", curses.A_NORMAL | curses.color_pair(13) )
	stdscr.addstr(pos_dig_y, pos_sec_x, "%02d" % (
	    second), curses.A_BOLD | curses.color_pair(13) )

    stdscr.refresh()

def fetch_screensize():
    global my_width
    global my_height
    global stdscr
    my_width = 80
    my_height = 25
    my_height, my_width = stdscr.getmaxyx()

def resize_handler():
    fetch_screensize()
    recalc_coords(my_width, my_height)

def main():
    global stdscr
    global pause
    global toggle_digital
    global window_is_too_small

    stdscr = curses.initscr()
    curses.start_color()
    curses.noecho()
    curses.cbreak()
    stdscr.keypad(1)
    
    stdscr.nodelay(1)
    curses.halfdelay(1)
    
    curses.init_pair(10, curses.COLOR_RED, curses.COLOR_BLACK)
    curses.init_pair(11, curses.COLOR_BLACK, curses.COLOR_BLACK)
    curses.init_pair(12, curses.COLOR_GREEN, curses.COLOR_BLACK)
    curses.init_pair(13, curses.COLOR_BLUE, curses.COLOR_BLACK)
    
    fetch_screensize()
    recalc_coords(my_width, my_height)
    if window_is_too_small:
	curses.echo()
	curses.endwin()
	print "Screen too small."
	sys.exit(1)

    cas = time.time()

    while 1:
	try:
	    c = stdscr.getch()
	except KeyboardInterrupt:
	    break
	if c in [-1, curses.KEY_RESIZE]:
	    if c == -1:
		pass
	    elif c == curses.KEY_RESIZE:
		resize_handler()
	else:
	    if c == ord('d'):
		toggle_digital = not toggle_digital
		recalc_coords(my_width, my_height)
	    elif c == ord('q'): break
	    elif c == ord('p'):
		pause = not pause
	    else:
		pass
	if not pause:
	    cas = time.time()
	cas2 = time.localtime(cas)
        year, month, day, hour, minute, second, weekday = cas2[:7]
	hds = int(math.fmod(math.floor(cas * 100), 100))
        draw_watch(year, month, day, hour, minute, second, weekday, hds)

    curses.echo()
    curses.endwin()



main()


