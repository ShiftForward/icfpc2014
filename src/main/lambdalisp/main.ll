(include prelude.ll)

(let ((x 1))
	 (cond ((= x 2) 5)
	       ((= x 4) 4)
	       ((= x 2) 10)
	       (else 999)))