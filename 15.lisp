(in-package #:cl-user)

(defun make-generator (state factor divisor)
  (declare (optimize (space 0) (speed 3) (debug 3) (safety 1))
	   (type (unsigned-byte 32) state factor divisor))
  #'(lambda ()
      (loop for st = (setf state (rem (* state factor) 2147483647))
	 until (zerop (rem st divisor)))
      state))

(defun generators (salt1 salt2 &optional (count 5000000) (div1 4) (div2 8))
  (loop with gen-a = (make-generator salt1 16807 div1) and gen-b = (make-generator salt2 48271 div2)
       repeat count counting (zerop (logand #xffff (logxor (funcall gen-a) (funcall gen-b))))))
