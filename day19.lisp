(in-package #:cl-user)

(defun follow (input)
  (let ((height 0)
	(width 0))
    (with-open-file (s input :direction :input)
      (loop for line = (read-line s nil)
	 while line
	 maximizing (length line) into xmax
	 counting line into ymax
	 finally (setf height ymax
		       width xmax)))
    (let ((lab (make-array (list height width) :initial-element #\x))
	  (steps 0)
	  chain)
      (flet ((blankp (y x)
	       (char= #\Space (aref lab y x))))
	(with-open-file (s input :direction :input)
	  (loop for line = (read-line s nil)
	     for y from 0
	     while line do
	       (loop for x from 0 below (length line) do
		    (setf (aref lab y x) (aref line x))))
	  (loop with y = 0 and x = (loop for i from 0 below width when (char= #\| (aref lab 0 i)) do (return i))
	     with vertical = t and increment = 1
	     while (and (<= 0 x (1- width))
			(<= 0 y (1- height)))
	     for point = (aref lab y x) do
	       (cond ((char= point #\+) (setf vertical (not vertical)
					      increment (if vertical
							    (if (blankp (1- y) x) 1 -1)
							    (if (blankp y (1- x))
								1 -1))))
		     ((char= point #\Space) (return))
		     ((alpha-char-p point) (push point chain)))
	       (incf steps)
	       (if vertical
		   (incf y increment)
		   (incf x increment)))))
      (values (coerce (reverse chain) 'string) steps))))
