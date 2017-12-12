(in-package #:cl-user)

(defun components (input)
  (with-open-file (s input :direction :input)
    (loop with records = (loop for line = (read-line s nil) while line
			    for edge = (read-from-string (format nil "(~A)" (nsubstitute #\Space #\, line))) collecting (nthcdr 2 edge))
       with edges = (make-array (length records) :initial-contents records)
       for pos from 0 below (length edges)
       when (consp (aref edges pos)) do
	 (setf (aref edges pos) (labels ((descend (array indexes)
					   (if indexes
					       (loop for e in indexes
						  for val = (aref array e) do
						    (setf (aref array e) nil)
						  summing (descend array val))
					       1)))
				  (1- (descend edges (aref edges pos)))))
       finally (return (values (aref edges 0) (loop for edge across edges when edge counting edge))))))
