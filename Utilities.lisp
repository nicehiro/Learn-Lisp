(defun single? (lst)
  (and (consp lst)
       (null (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun map-int (fn lst)
  (let ((ret nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val ret))))
    (reverse ret)))

(defun map-int-rev (fn lst)
  (if (null lst)
      nil
      (let ((ret (map-int-rev fn (cdr lst)))
	    (val (funcall fn (car lst))))
	(if val
	    (cons val ret)))))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
	     (max (funcall fn wins)))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (when (> obj max)
	      (setf wins obj
		    max score))))
	(values wins max))))
