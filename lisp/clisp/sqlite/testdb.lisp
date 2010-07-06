(in-package :sqlite)
(with-open-db (db #"data.db")
  (with-query 
    (db (id name ) 
	"select id, name from tbltest")
     (format t "~s ~s~%" id name )))
      