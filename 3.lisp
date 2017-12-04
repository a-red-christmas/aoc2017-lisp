(in-package #:cl-user)

(defun mhd (n)
  (if (= n 1)
      0
      (let* ((num (floor (ceiling (sqrt n)) 2))
	     (offset (mod (- n (expt (1- (* num 2)) 2)) (* 2 num))))
	(+ num (abs (- offset num))))))

(defun nsum (matrix y x)
  (let ((sub (make-array '(3 3) :displaced-to matrix :displaced-index-offset (array-row-major-index matrix (1- y) (1- x)))))
    (setf (aref sub 0 0) (loop for i from 0 below (* 3 3) summing (row-major-aref sub i))))) ;assumption is, the element we set is zero (unfilled)

(defun spiral2 (n)
  (let* ((size (ceiling (expt n 0.333)))
	 (matrix (make-array (list size size) :initial-element 0))
	 (y (ceiling size 2))
	 (x (ceiling size 2)))
    (setf (aref matrix y x) 1)
    ))
