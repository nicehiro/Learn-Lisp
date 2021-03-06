(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
	(reduce #'(lambda (v f) (funcall f v))
		rest
		:initial-value (apply fn1 args)))))

					; This method is wrong
					; Maybe fns is not a list
(defun my-compose (&rest fns)
  (if (null (cdr fns))
      #'(lambda (&rest args)
	  (apply (car fns) args))
      (let ((ret (my-compose (cdr fns))))
	#'(lambda () (apply (car fns) ret)))))

(defun disjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((disj (apply #'disjoin fns)))
	#'(lambda (&rest args)
	    (or (apply fn args) (apply disj args))))))

(defun conjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((disj (conjoin fns)))
	#'(lambda (&lambda args)
	    (and (apply fn args) (apply disj args))))))

(defun curry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args args2))))

(defun always (x) #'lambda (&rest args) x)
