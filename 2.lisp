(in-package #:cl-user)

(defun checksum (input)
  (with-open-file (s input :direction :input)
    (loop for line = (read-line s nil)
       for row = (read-from-string (format nil "(~A)" line))
       while line summing
	 (loop for elt in row maximizing elt into max minimizing elt into min
	    finally (return (- max min))))))

(defun checksum2 (input)
  (with-open-file (s input :direction :input)
    (loop for line = (read-line s nil)
       for row = (read-from-string (format nil "#(~A)" line))
       while line summing
	 (loop named outer
	    for i from 0 below (length row) do
	      (loop for j from 0 below (length row)
		 when (and (/= i j) (zerop (rem (aref row i) (aref row j))))
		 do (return-from outer (/ (aref row i) (aref row j))))))))
