(in-package #:cl-user)

(defun redistribute (banks)
  (let* ((max (reduce #'max banks))
	 (rest (member max banks)))
    (setf (car rest) 0)
    (loop for tail = (or (cdr rest) banks) then (or (cdr tail) banks)
       while (plusp max) do
	 (incf (car tail))
	 (decf max))
    banks))

(defun memman (input)
  (loop for count from 0
     for cache = '() then (push (copy-list banks) cache)
     for banks = input then (redistribute banks)
     until (member banks cache :test #'equal)
     finally (return count)))

(defun memman2 (input)
  (loop for count from 0
     for cache = '() then (push (cons (copy-list banks) count) cache)
     for banks = input then (redistribute banks)
     for match = (member banks cache :test #'equal :key #'car)
     until match finally (return (1+ (- count (cdr (car match)))))))
