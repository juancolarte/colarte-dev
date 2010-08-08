;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          horscroll.asd
;;;; Purpose:       horizontal ncurses scroller
;;;; Author:        Camilo Olarte
;;;; Date Started:  2010-07-25
;;;;
;;;;

(defsystem horscroll
    :name "horscroll"
    :author "Camilo Olarte"
    :version "0.5"
    :maintainer "Camilo Olarte <colarte@freeshell.org>"
    :licence "BSD"
    :description "Ncurses horizontal scroller /depends on ncurses  cffi"
    :long-description "This package provides an horizontal scroller with various capabilities."
    :depends-on (ncurses)
    :components ((:file "horscroll")))

