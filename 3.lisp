(in-package #:cl-user)

;;; part 1
(defun mhd (n)
  (if (= n 1)
      0
      (let* ((num (floor (ceiling (sqrt n)) 2))
	     (offset (mod (- n (expt (1- (* num 2)) 2)) (* 2 num))))
	(+ num (abs (- offset num))))))

;;; part 2
(defun memoize (cache y x val)
  (unless (gethash y cache)
    (setf (gethash y cache) (make-hash-table)))
  (setf (gethash x (gethash y cache)) val))

(defun recall (cache y x)
  (if (gethash y cache)
      (gethash x (gethash y cache) 0)
      0))

(defun adjacent (cache yo xo)
  (loop for y from (1- yo) to (1+ yo) summing
       (loop for x from (1- xo) to (1+ xo) summing (recall cache y x))))

(defun spiral (n)
  (let ((y 0)
	(x 0)
	(cache (make-hash-table)))
    (memoize cache 0 0 1)
    (loop named outer with c = 1
       with l = 0 do
	 (when (= c n) (return-from outer (recall cache y x)))
	 (loop repeat (1+ l) do
	      (incf x)
	      (incf c)
	      (when (> (memoize cache y x (adjacent cache y x)) n)
		(return-from outer (recall cache y x))))
	 (loop repeat (1+ l) do
	      (incf y)
	      (incf c)
	      (when (> (memoize cache y x (adjacent cache y x)) n)
		(return-from outer (recall cache y x))))
	 (incf l)
	 (loop repeat (1+ l) do
	      (decf x)
	      (incf c)
	      (when (> (memoize cache y x (adjacent cache y x)) n)
		(return-from outer (recall cache y x))))
	 (loop repeat (1+ l) do
	      (decf y)
	      (incf c)
	      (when (> (memoize cache y x (adjacent cache y x)) n)
		(return-from outer (recall cache y x))))
	 (incf l))))
