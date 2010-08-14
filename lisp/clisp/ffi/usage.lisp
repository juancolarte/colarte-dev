(load "magic.fas")
(in-package :magic)


(defvar my_t (magic_open (and MAGIC_CONTINUE MAGIC_ERROR MAGIC_DEBUG MAGIC_MIME)))
(magic_load my_t nil)
(print (magic_file my_t "/home/colarte/xorg.conf"))
(magic_close my_t)



