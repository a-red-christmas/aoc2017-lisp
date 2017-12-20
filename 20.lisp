(in-package #:cl-user)

(defun load-points (input)
 (with-open-file (s input :direction :input)
   (loop for line = (read-line s nil)
      while line collecting (destructuring-bind (drop1 p drop2 v drop3 a)
				(read-from-string (format nil "(~A)"
							  (nsubstitute #\Space #\= (nsubstitute #\) #\> (nsubstitute #\( #\< (nsubstitute #\Space #\, line))))))
			      (list p v a)))))

(defun part1 (input)
  (let ((particles (load-points input)))
    (loop for particle in particles
       for pos from 0
       for acc = (third particle) for mag = (loop for c in acc summing (expt c 2))
       with minpos and min = most-positive-fixnum when (> min mag) do
	 (setf minpos pos
	       min (min min mag))
       finally (return minpos))))

(defun part2 (input &optional (iterations 1000))
  (let ((particles (load-points input)))
    (flet ((incvec (to with)
	     (loop for pos from 0 to 2 do
		  (incf (nth pos to) (nth pos with)))))
      (loop repeat iterations do
	   (loop for particle in particles
	      for position = (first particle)
	      for velocity = (second particle)
	      for acceleration = (third particle) do
		(incvec velocity acceleration)
		(incvec position velocity))
	   (let ((matches '()))
	     (setf particles (remove-if #'(lambda (x)
					    (member x matches :test #'equalp))
					(remove-duplicates particles :test #'(lambda (a b)
									       (when (equalp (first a) (first b))
										 (push (first a) matches))))
					:key #'first))))
      (length particles))))
