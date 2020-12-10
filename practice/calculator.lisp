
(defpackage :calculator

  (:use :common-lisp))

(in-package calculator)

(defun combine-expr (ex n l)
  "Return expression where operator l and operand are applied to the first
   member of expression ex"
  (cons (list ex n (first l)) (rest l)))

(defun enclose-expression (expr)
  "Convert list representing arithmetic expression in normal infix notation
   into Cambridge Prefix notation"
  (check-type expr list)
  (cond
    ((< (length expr) 3) expr)
    (t (enclose-expression
        (combine-expr (second expr) (first expr) (nthcdr 2 expr))))))

(defun mult-divp (o)
  "Return T if operator o is a / or *"
  (cond
    ((string= o '/) T)
    ((string= o '*) T)))

(defun term-seperatorp (o)
  "Return T if operator o is + or -"
  (cond
    ((string= o '+) T)
    ((string= o '-) T)))

(defun enclose-term (term)
  "Take a list of expressions and return with first term collected as
   first member expressed in Cambridge Prefix notation"
  (check-type term list)
  (if (term-seperatorp (second term))
      term
      (enclose-term
       (combine-expr (second term) (first term) (nthcdr 2 term)))))

(defun enclose-factor (factor)
  "Return a list of numbers and operators with the first factor collected as
   first member in Cambridge Prefix notation"
  (check-type factor list)
  (if (mult-divp (second factor))
      factor
      (combine-expr (second factor) (first factor) (enclose-factor (nthcdr 2 factor)))))
    



