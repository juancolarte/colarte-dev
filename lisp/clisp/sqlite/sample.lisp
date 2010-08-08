(with-open-db (db #"foo.db")
  (with-query 
    (db (id name email) 
	"select id, name, email from student where grade=~s" 12)
     (format t "~s ~s ~s~%" id name email)))
      