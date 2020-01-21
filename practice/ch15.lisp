(defun sum (n1 n2)
  "Returns sum of two nonnegative integars"
  (if (zerop n1) n2
      (sum (1- n1) (1+ n2))))
