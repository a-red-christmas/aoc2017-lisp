(in-package #:cl-user)

(setf *print-circle* t)

(defun spinlock (n)
  (let* ((buffer (list 0))
	 (cur (setf (cdr buffer) buffer)))
    (loop for i from 1 to 2017 do
	 (loop repeat n do (pop cur))
	 (setf (cdr cur) (cons i (cdr cur)))
	 (pop cur))
    (cadr cur)))

(defun spinlock2 (n)
  (let* ((buffer (list 0))
	 (cur (setf (cdr buffer) buffer)))
    (loop for i from 1 to 50000000 do
	 (loop repeat n do (pop cur))
	 (setf (cdr cur) (cons i (cdr cur)))
	 (pop cur))
    (cadr buffer)))
