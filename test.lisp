(defmacro my-if (test then else)
  `(cond
	 (,test ,then)
	 (t ,else)))

(defmacro nth-expr (n &body body)
  (if (integerp n)
	  `(case ,n
		 ,@(let ((i -1))
				(mapcar #'(lambda (x) `(,(incf i) ,x)) body)))))

(defmacro n-of (n expr)
  (let ((grec (gensym)))
	`(labels ((,grec (i j acc)
				(if (= i j)
					(nreverse acc)
					(,grec (1+ i) j (cons ,expr acc)))))
	   (,grep 0 ,n nil))))

(defmacro retain (params &body body)
  `((lambda ,params ,@body) ,@params))
