;(make-package :ch-17 :use '(:cl))
(defpackage :ch-17
  (:shadow :copy
	   :append
	   :reverse
	   :union
	   :identity)

  (:use :common-lisp))

; you need to do this first...
(load "~/Projects/lisp/practice/util.lisp")


(in-package ch-17)

(deftype element ()
  "Elements are objects testable by EQL,
   symbols, characters, numbers and packages."
  '(satisfies util::elementp))

(defun copy (l)
  (if (null l) '()
      (cons (first l) (copy (rest l)))))

(defun append (l1 l2)

  (if (null l1) l2
      (cons (first l1) (append (rest l1) l2))))

(defun reverse (l)
  "Returns copy of list l with order of members reversed"
  (check-type l list)
  (if (null l) '()
      (append (reverse (rest l))
	      (list (first l)))))

(defun reverse2 (l1 l2)
  "Returns a list consisting of the members of l1
   in reverse order followed by members of l2 in orig."
  (check-type l1 list)
  (check-type l2 list)
  (if (null l1) l2
      (reverse2 (rest l1)
		(cons (first l1) l2))))

(defun sub-first (new old l)
  "Return copy of the list l with the element new
   replacing the first occurence of the element old."
  (check-type new element)
  (check-type old element)
  (check-type l list)
  (cond ((null l) '())
	((eql (first l) old) (cons new (rest l)))
	(t (cons (first l)
		 (sub-first new old (rest l))))))

(defun subst* (new old l)
  "Return copy of list l with element new replacing
   all occurances of element old"
  (check-type new util::element)
  (check-type old util::element)
  (check-type l list)
  (cond ((null l) '())
	((eql (first l) old)
	 (cons new (subst* new old (rest l))))
	(t (cons (first l)
		 (subst* new old (rest l))))))

(defun makeset (b)
  "Returns a set containing just those elements of
   the input bag b"
  (cond ((null b) '())
	((member (first b) (rest b))
	 (makeset (rest b)))
	(t (cons (first b) (makeset (rest b))))))

(defun union (s1 s2)
  "Return the union of the sets s1 and s2"
  (cond ((null s1) s2)
	((member (first s1) s2)
	 (union (rest s1) s2))
	(t (cons (first s1) (union (rest s1) s2)))))

(defun identity (object)
  "Returns its argument unmodified"
  object)

(defun firstn (n l)
  "Return first n elements of list l"
  (check-type n integer)
  (check-type l list)
  (cond ((eql n 0) '())
	(t (cons (first l) (firstn (1- n) (rest l))))))

