(make-package :match :use '(:cl)
  )

(in-package match)

(defun variablep (x)
  "Returns T if first character of symbol's name is #\?"
  (and
   (symbolp x)
   (char= (char (symbol-name x) 0) '#\? )))


(defun match-element (e1 e2)
  "Return T if e1 and e2 are eql, if either is a #\?;
   Return a list of both elements, variable first, if either is a variable"
  (cond ((eql e1 e2) T)
	((and (match::variablep e1) (not (match::dont-care e1))) (list e1 e2))
	((and (match::variablep e2) (not (match::dont-care e2))) (list e2 e1))
	((or (match::dont-care e1) (match::dont-care e2)) T)))


(defun dont-care (x)
  (and (symbolp x) (string= (symbol-name x) "?")))
