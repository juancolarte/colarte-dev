
(setq s (socket-connect 80 "www.podval.org" :buffered nil))
(format s "GET / HTTP/1.0~2%")
(loop :for l = (read-line s nil nil) :while l :do (print l))
