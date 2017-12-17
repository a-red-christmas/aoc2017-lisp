(in-package #:cl-user)

(defun dance (input &optional (count 1))
  (declare (optimize (speed 3) (space 0) (debug 0) (safety 0))
	   (type fixnum count))
  (with-open-file (s input :direction :input)
    (let ((choreo (read-from-string (format nil "(~A)" (nsubstitute #\Space #\, (read-line s)))))
	  (programs (format nil "ABCDEFGHIJKLMNOP")))
      (declare (type simple-string programs))
      (loop repeat count do
	   (loop for e in choreo
	      for act = (symbol-name e) do
		(case (aref act 0)
		  (#\S (let ((num (parse-integer act :start 1)))
			 (declare (type fixnum num))
			 (setf programs (format nil "~A~A" (subseq programs (- 16 num)) (subseq programs 0 (- 16 num))))))
		  (#\X (multiple-value-bind (from next) (parse-integer act :start 1 :junk-allowed t)
			 (rotatef (aref programs from) (aref programs (parse-integer act :start (1+ next))))))
		  (#\P (rotatef (aref programs (position (aref act 3) programs)) (aref programs (position (aref act 1) programs)))))))
      programs)))

;;; pt2 - the sequence had period of 100
