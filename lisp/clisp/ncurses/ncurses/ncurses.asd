;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ncurses.asd
;;;; Purpose:       Cffi ncurses package produced with swig
;;;; Author:        Camilo Olarte
;;;; Date Started:  2010-07-25
;;;;
;;;;

(defsystem ncurses
    :name "ncurses"
    :author "Camilo Olarte"
    :version "0.1"
    :maintainer "Camilo Olarte <colarte@freeshell.org>"
    :licence "BSD"
    :description "Ncurses cffi bindings for clisp / sbcl"
    :long-description "This package provides access to ncurses library."
    :depends-on (cffi)
    :components ((:file "ncurses")))

