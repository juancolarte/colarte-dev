(LET ((server (SOCKET:SOCKET-SERVER)))
  (FORMAT t "~&Waiting for a connection on ~S:~D~%"
          (SOCKET:SOCKET-SERVER-HOST server) (SOCKET:SOCKET-SERVER-PORT server))
  (UNWIND-PROTECT
      ;; infinite loop, terminate with Control-C
      (LOOP (WITH-OPEN-STREAM (socket (SOCKET:SOCKET-ACCEPT server))
              (MULTIPLE-VALUE-BIND (local-host local-port) (SOCKET:SOCKET-STREAM-LOCAL socket)
                (MULTIPLE-VALUE-BIND (remote-host remote-port) (SOCKET:SOCKET-STREAM-PEER socket)
                  (FORMAT T "~&Connection: ~S:~D -- ~S:~D~%"
                          remote-host remote-port local-host local-port)))
              ;; loop is terminated when the remote host closes the connection or on EXT:EXIT
              (LOOP (WHEN (EQ :eof (SOCKET:SOCKET-STATUS (cons socket :input))) (RETURN))
                    (PRINT (EVAL (READ socket)) socket)
                    ;; flush everything left in socket
                    (LOOP :for c = (READ-CHAR-NO-HANG socket nil nil) :while c)
                    (TERPRI socket))))
    ;; make sure server is closed
    (SOCKET:SOCKET-SERVER-CLOSE server)))

