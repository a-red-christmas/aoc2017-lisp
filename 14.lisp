(in-package #:cl-user)

(load "3.lisp")

(defun defrag (input)
  (loop for i from 0 to 127
     for hash = (parse-integer (encode2 (format nil "~A-~D" input i)) :radix 16)
     summing (loop for v = hash then (ash v -1) until (zerop v) counting (not (zerop (logand v 1))))))

(defun connected (input)
  (flet ((nsegments (hash)
	   (loop for v = hash then (ash v -1) until (zerop v)
	      for prev = 0 then bit
	      for bit = (logand v 1) counting (and (= bit 1) (zerop prev)))))
    (loop for i from 0 to 127
     for prev = 0 then hash
       for hash = (parse-integer (encode2 (format nil "~A-~D" input i)) :radix 16)
	 do (format t "~%~b" hash)
       summing (print (nsegments (logand hash (lognot prev)))))))

