(in-package #:cl-user)

(defun components (input)
  (with-open-file (s input :direction :input)
    (loop for line = (read-from-string (read-line s nil)) while line
       collecting (cons (first line) (nthcdr 2 line))) with components = '(0)
       when (member ) )))
