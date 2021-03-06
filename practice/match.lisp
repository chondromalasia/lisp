
(defpackage :match
  (:shadow :boundp
	   :substitute)
  (:use :common-lisp))

(in-package match)

(defun variablep (x)
  "Returns T if first character of symbol's name is #\?"
  (and
   (symbolp x)
   (char= (char (symbol-name x) 0) '#\? )))

(defun dont-care (x)
  (and (symbolp x) (string= (symbol-name x) "?")))

(defun match-element (e1 e2)
  "Return T if e1 and e2 are eql, if either is a #\?;
   Return a list of both elements, variable first, if either is a variable"
  (cond ((eql e1 e2) T)
	((and (match::variablep e1) (not (match::dont-care e1))) (list e1 e2))
	((and (match::variablep e2) (not (match::dont-care e2))) (list e2 e1))
	((or (match::dont-care e1) (match::dont-care e2)) T)))

(defun matchlelt (l1 l2)
  "Return T if corresponding members of list are same, ? marks being wild"
  (check-type l1 list)
  (check-type l2 list)
  (cond ((null l1) (null l2))
	((null l2) nil)
	((or (eql (first l1) (first l2))
	     (dont-care (first l1))
	     (dont-care (first l2)))
	 (matchlelt (rest l1) (rest l2)))
	(t nil)))

(defun boundp (v subs)
  "Return True if variable v is bound to anything in the substitution subs"
  (check-type subs list)
  (cond
    ((null (match::variablep v)) nil)
    ((null (assoc v subs)) nil)
    ((list (assoc v subs)) T)))

(defun bound-to (v subs)
  "Returns the term that variable v is bound to in substitution subs"
  (check-type subs list)
  (cond
    ((null (match::variablep v)) nil)
    ((boundp v subs) (rest (assoc v subs)))
    (T nil)))


(defun clean_pairs (pairs)
  "Check on the substitutions of list of pairs
   If a variable pair is duplicated, trim it down, if a variable
   occurs a second time with a different paining, return NIL
   Note: I don't this is how he intended to do this, so heads up for later"
  (cond
    ((null pairs) nil)
    ((equal '(T T) (first pairs)) '((T T)))
    ((not (boundp (first (first pairs)) (rest pairs)))
     (cons (first pairs) (clean_pairs (rest pairs))))
    ((equal (assoc (first (first pairs)) (rest pairs)) (first pairs))
     (clean_pairs (rest pairs)))
    (T nil)))


(defun match1 (pat lst pairs)
  "Make a list of pairs"
  (cond
    ((and (null pat) (null lst)) (cons (list T T) pairs))
    ((eql (first pat) (first lst)) (match1 (rest pat) (rest lst) pairs))
    ((variablep (first pat))
     (cons (list (first pat) (first lst))
	   (match1 (rest pat) (rest lst) pairs)))))


(defun match (pat lst)
  "Init for match1"
  (check-type pat list)
  (check-type lst list)
  (clean_pairs (match1 pat lst '())))


(defun substitute (pat subs)
  "Return list where each variable (defined by variablep) is replaced
   by term in subs"
  (check-type pat list)
  (check-type subs list)
  (cond
    ((null (first pat)) '())
    ((variablep (first pat))
     (cons
      (first (bound-to (first pat) subs)) (substitute (rest pat) subs)))
    ((not (variablep (first pat))) (cons (first pat) (substitute (rest pat) subs)))))

	
;; Work in progress, but you need to go to the website for it
;; https://cse.buffalo.edu/~shapiro/Courses/CSE202/Notes/chapter18.html
;; that's a good explanaish
(defun match2 (pat lst pairs)
  (cond
    ((symbolp pat)
     (cond
       ((eql pat lst) (cons (list T T) pairs))
       ((variablep pat) (cons (list pat lst) (cons (list T T) pairs)))
       ((not (eql pat lst)) nil)))
    (T 'butt)
    ))

(defun match2_wrapper (pat lst)
  (match2 pat lst '()))

