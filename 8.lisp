(in-package #:cl-user)


(defun solve (input)
  (with-open-file (s input :direction :input)
    (let ((program (loop for line = (read-line s nil) while line
		      for row = (read-from-string (format nil "(~A)" line))
		      for reg = (symbol-name (pop row))
		      for op = (pop row)
		      for diff = (pop row) for ignored = (pop row)
		      for tested = (symbol-name (pop row))
		      for cmp = (pop row)
		      for cmpval = (pop row) collecting `((,(cond ((eql cmp '==) '=)
								  ((eql cmp '!=) '/=)
								  (t cmp)) ,tested ,cmpval) (,op ,reg, diff))))
	  (bank (make-hash-table :test 'equalp)))
      (values (loop for line in program with max = 0
		 for comp = (first line) for body = (second line)
		 when (funcall (first comp) (gethash (second comp) bank 0) (third comp)) do
		   (setf max (max max
				  (if (eql (first body) 'inc)
				      (incf (gethash (second body) bank 0) (third body))
				      (decf (gethash (second body) bank 0) (third body)))))
		 finally (return max))
	      (loop for value being the hash-values of bank maximizing value)))))

