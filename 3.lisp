(in-package #:cl-user)

(defun mhd (n)
  (if (= n 1)
      0
      (let* ((num (floor (ceiling (sqrt n)) 2))
	     (offset (mod (- n (expt (1- (* num 2)) 2)) (* 2 num))))
	(+ num (abs (- offset num))))))

