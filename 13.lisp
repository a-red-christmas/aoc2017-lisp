(in-package #:cl-user)

(defun severity (ranges offset)
  (loop for depth from offset
     for range across ranges summing (if (and (plusp range) (zerop (rem depth (* 2 (1- range)))))
					     (* range depth)
					     0)))

(defun path (input)
  (with-open-file (s input :direction :input)
    (let* ((last 0)
	   (records (loop for line = (read-line s nil) while line
		       for edge = (read-from-string (format nil "(~A)" (remove #\: line))) collecting edge
		       do (setf last (car edge))))
	   (ranges (make-array (1+ last) :initial-element 0)))
      (loop for rec in records do
	   (setf (aref ranges (first rec)) (second rec)))
      (values (severity ranges 0)
	      (loop for offset from 0 when (zerop (severity ranges offset)) do (return offset))))))
