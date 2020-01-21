(defun x-y (x y)
  (if (eql y 0)
      99999999
      (/ x y)))


(defun absval (x)
  (cond ((< x 0) (- x))
	((> x 0) x)))

(defun sign (x)
  (cond ((< x 0) '-)
	((= x 0) 0)
	((> x 0) '+)))

(defun sum (n1 n2)
  "returns the sum of two nonnegative integars"
  (if (zerop n1) n2
      (sum (1- n1) (1+ n2))))

(defun switch_ (l)
  (list2 (second l) (first 1)))
