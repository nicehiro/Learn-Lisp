(defmacro for (var start stop &body body)
  (let ((gstop (gensym)))
	`(do ((,var ,start (1+ ,var))
		  (,gstop ,stop))
		 ((> ,var ,gstop))
	   ,@body)))

;; obj 是否在 choices 里
(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
	`(let ((,insym ,obj))
	   (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c)) choices)))))

;; 随机选取一个参数求值
;; (random-choice (a b))
;; (case (random 2)
;;   (0 a)
;;   (1 b))
(defmacro random-choice (&rest exprs)
  `(case (random ,(length exprs))
	 ,@(let ((key -1))
			(mapcar #'(lambda (expr) `(,(incf key) ,expr))
					exprs))))

;;

(DEFMACRO with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym)))
				 syms)
	 ,@body))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
	 (if it ,then ,else)))
