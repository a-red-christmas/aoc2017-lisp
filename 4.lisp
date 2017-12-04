(in-package #:cl-user)

(defun passcheck (input)
  (with-open-file (s input :direction :input)
    (loop for line = (read-line s nil)
       for row = (read-from-string (format nil "(~A)" line))
       while line summing
	 (if (= (length (remove-duplicates row)) (length row))
	     1
	     0))))

(defun anagram-p (string1 string2)
  (let ((explode1 (coerce string1 'list))
	(explode2 (coerce string2 'list)))
    (and (= (length string1) (length string2)) (null (set-exclusive-or explode1 explode2)))))

(defun passcheck-anagram (input)
  (with-open-file (s input :direction :input)
    (loop for line = (read-line s nil)
       for row = (read-from-string (format nil "(~A)" line))
       while line summing
	 (if (= (length (remove-duplicates row :test #'anagram-p :key #'symbol-name)) (length row))
	     1
	     0))))
