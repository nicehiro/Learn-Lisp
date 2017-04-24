(defstruct buf
  vec
  (start -1)
  (end -1)
  (new -1)
  (used -1))

(defun bref (buf n)
  (svref (buf-vec buf)
	 (mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n)
  (setf (svref (buf-vec buf)
	       (mod n (length (buf-vec buf))))
	val))

(defun new-buf (len)
  (make-buf :vec (make-array len)))

(defun buf-insert (x buf)
  (setf (bref buf (incf (buf-end buf))) x))

(defun buf-pop (buf)
  (prog1
      (bref buf (incf (buf-start buf)))
    (setf (buf-used buf) (buf-start buf)
	  (buf-new buf) (buf-end buf))))

(defun buf-next (buf)
  (when (< (buf-used buf) (buf-new buf))
    (bref buf (incf (buf-used buf)))))

(defun buf-reset (buf)
  (setf (buf-used buf) (buf-start buf)
	(buf-new buf) (buf-end buf)))

(defun buf-clear (buf)
  (setf (buf-start buf) -1
	(buf-end buf) -1
	(buf-used buf) -1
	(buf-new buf) -1))

(defun buf-flush (buf str)
  (do ((i (1+ (buf-used buf)) (1+ i)))
      ((> i (buf-end buf)))
    (princ (bref buf i) str)))

(defun file-subst (old new file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
			 :if-exists :supersede)
      (stream-subst old new in out))))

(defun stream-subst (old new in out)
  (let* ((pos 0)
	 (len (length old))
	 (buf (new-buf len))
	 (from-buf nil))
    (do ((c (read-char in nil :eof)
	    (or (setf from-buf (buf-next buf))
		(read-char in nil :eof))))
	((eql c :eof))
      (cond ((char= c (char old pos))
	     (incf pos)
	     (cond ((= pos len)
		    (princ new out)
		    (setf pos 0)
		    (buf-clear buf))
		   ((not from-buf)
		    (buf-insert c buf))))
	    ((zerop pos)
	     (princ c out)
	     (when from-buf
	       (buf-pop buf)
	       (buf-reset buf)))
	    (t
	     (unless from-buf
	       (buf-insert c buf))
	     (princ (buf-pop buf) out)
	     (buf-reset buf)
	     (setf pos 0))))
    (buf-flush buf out)))
