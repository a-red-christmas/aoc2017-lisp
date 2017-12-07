(in-package #:cl-user)

(defun build-tree (input)
    (with-open-file (s input :direction :input)
      (loop with rows = (loop for line = (read-line s nil)
			   while line for parsed = (remove #\> (remove #\- (remove #\, line)))
			   for row = (read-from-string (format nil "(~A)" line)) collecting row)
	 for row in rows for node = (car row)
	 while (member node (cddr entry)))))
