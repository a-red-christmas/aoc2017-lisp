(in-package #:cl-user)

(load "10.lisp")

(defun defrag (input)
  (loop for i from 0 to 127
     for hash = (parse-integer (encode2 (format nil "~A-~D" input i)) :radix 16)
     summing (loop for v = hash then (ash v -1) until (zerop v) counting (not (zerop (logand v 1))))))

(defun flood (matrix y x marker)
  (if (minusp (aref matrix y x))
      (progn
	(setf (aref matrix y x) marker)
	(loop for (dy dx) in '((0 -1) (0 1) (1 0) (-1 0))
	   for ny = (+ y dy) for nx = (+ x dx)
	   when (and (<= 0 ny 127) (<= 0 nx 127)) do
	     (flood matrix ny nx marker)))
      nil))


(defun components (input)
  (let ((matrix (make-array '(128 128) :initial-element 0)))
    (loop for y from 0 to 127
       for hash = (parse-integer (encode2 (format nil "~A-~D" input y)) :radix 16) do
	 (loop for x from 127 downto 0
	    for v = hash then (ash v -1) until (zerop v) do
	      (setf (aref matrix y x) (- 0 (logand v 1)))))
    (loop for y from 0 to 127
       with count = 0 do
	 (loop for x from 0 to 127
	    when (minusp (aref matrix y x)) do
	      (flood matrix y x (incf count)))
       finally (return count))))

;;;dead end method
;; (defun connected (input)
;;   (flet ((nsegments (hash)
;; 	   (loop for v = hash then (ash v -1) until (zerop v)
;; 	      for prev = 0 then bit
;; 	      for bit = (logand v 1) counting (and (= bit 1) (zerop prev)))))
;;     (loop for i from 0 to 127
;;      for prev = 0 then hash
;;        for hash = (parse-integer (encode2 (format nil "~A-~D" input i)) :radix 16)
;; 	 do (format t "~%~b" hash)
;;        summing (print (nsegments (logand hash (lognot prev)))))))
