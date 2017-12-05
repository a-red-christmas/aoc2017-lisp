(in-package #:cl-user)

(defun jumper (input)
  (with-open-file (s input :direction :input)
    (loop with elts = (loop for jump = (read s nil) while jump collecting jump)
       with program = (make-array (length elts) :initial-contents elts)
       with pos = 0 for count from 0 do
	 (handler-case (incf pos (1- (incf (aref program pos))))
	   (error () (return count))))))

(defun jumper2 (input)
  (with-open-file (s input :direction :input)
    (loop with elts = (loop for jump = (read s nil) while jump collecting jump)
       with program = (make-array (length elts) :initial-contents elts)
       with pos = 0 for count from 0 do
	 (handler-case (incf pos (if (>= (aref program pos) 3)
				     (1+ (decf (aref program pos)))
				     (1- (incf (aref program pos)))))
	   (error () (return count))))))
