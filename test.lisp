(defun parse-pattern (pat)
  (labels ((rec (i n ctrl acc)
	     (if (< i n)
		 (let* ((c (char pat i))
			(ctrl-next (and (not ctrl) (char= c #\%))))
		   (rec (i+ 1)
			n
			ctrl-next
			(if ctrl-next
			    acc
			    (cons
			     (if ctrl
				 (case c
				   (#\a 'all)
				   (#\w 'word)
				   (#\d 'digit)
				   (#\% #\%))
				 c)
			     acc))))
		 (concatenate 'vector (nreverse acc)))))
    (rec 0 (length pat) nil nil)))
