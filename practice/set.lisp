
(defpackage :set
  (:shadow :set
	   :union
	   :first
	   :rest
	   :intersection
	   :complement
	   :subsetp
	   :equal)

  (:use :common-lisp)

  (:export :makeset
	   :set
	   :first
	   :rest
	   :insert
	   :intersection
	   :empty
	   :complement
	   :subsetp
	   :equal))

;(load "~/Projects/lisp/practice/util.lisp")

(in-package set)

(defun makeset-helper (b)
  "Returns a set containing just those elements of
   the input bag b"
  (check-type b util:bag)
  (cond ((null b) '())
	((member (common-lisp:first b) (common-lisp:rest b))
	 (makeset-helper (common-lisp:rest b)))
	(t (cons (common-lisp:first b) (makeset-helper (common-lisp:rest b))))))

(defun makeset (b)
  (check-type b util:bag)
  (cons :set (makeset-helper b)))

(defun setp (o)
  "Return T if a set, a list of objects, no two of which
   are equal, False otherwise or if not a list"
  (and
   (listp o)
   (eql (common-lisp:first o) :set)))

(deftype set ()
  "A set is a list of objects, no two of which are eql"
  '(satisfies setp))

(defun union-unlabelled-sets (s1 s2)
  "Return the union of the sets s1 and s2"
  (cond ((null s1) s2)
	((member (common-lisp:first s1) s2)
	 (union-unlabelled-sets (common-lisp:rest s1) s2))
	(t (cons (common-lisp:first s1) (union-unlabelled-sets (common-lisp:rest s1) s2)))))

(defun union (s1 s2)
  (check-type s1 set)
  (check-type s2 set)
  (cons :set (union-unlabelled-sets (rest s1) (rest s2))))

(defun first (l)
  "Return element that happens to be first in a set"
  (check-type l set)
  (common-lisp:first l))

(defun rest (l)
  "Return the set without the first element"
  (check-type l set)
  (common-lisp:rest l))

(defun insert (e s)
  "Return set s with e as an additional element, if not already in s"
  (check-type e util:element)
  (check-type s set)
  (cond ((member e (rest s)) s)
        (t (cons :set (cons e  (rest s))))))

(defun intersection-unlabelled-sets (s1 s2)
  "Returns the intersection between sets s1 and s2"
  (cond ((null s1) '())
	((member (common-lisp:first s1) s2)
	 (cons (common-lisp:first s1) (intersection-unlabelled-sets
				       (common-lisp:rest s1) s2)))
	(t (intersection-unlabelled-sets (common-lisp:rest s1) s2))))

(defun intersection (s1 s2)
  "Return the intersection between sets s1 and s2"
  (check-type s1 set)
  (check-type s2 set)
  (cons :set (intersection-unlabelled-sets (rest s1) (rest s2))))

(defun empty (s)
  "Return True if s is a set with no elements, False otherwise"
  (and (setp s)
       (null (rest s))))

(defun complement-unlabelled-sets (s1 s2)
  "Return set of elements in s1 not found in s2"
  (cond ((null s1) '())
	((member (common-lisp:first s1) s2)
	 (complement-unlabelled-sets (common-lisp:rest s1) s2))
	(t (cons (common-lisp:first s1) (complement-unlabelled-sets
					 (common-lisp:rest s1) s2)))))

(defun complement (s1 s2)
  "Return set of elements in s1 not found in s2"
  (check-type s1 set)
  (check-type s2 set)
  (complement-unlabelled-sets (rest s1) (rest s2)))

(defun subsetp-unlabelled-sets (s1 s2)
  "Return True if every element of s1 is an element of s2"
  (cond ((null s1) t)
	((member (common-lisp:first s1) s2)
	 (subsetp-unlabelled-sets (common-lisp:rest s1) s2))
	(t nil)))

(defun subsetp (s1 s2)
  "Return True if every element of s1 is an element of s2"
  (check-type s1 set)
  (check-type s2 set)
  (subsetp-unlabelled-sets (rest s1) (rest s2)))

(defun equal (s1 s2)
  "Return True if s1 and s2 contain the same elements, order
   does not matter"
  (check-type s1 set)
  (check-type s2 set)
  (and (subsetp-unlabelled-sets (rest s1) (rest s2))
       (subsetp-unlabelled-sets (rest s2) (rest s1))))

(defun xprod1 (o s)
  "Return cross product of object o with set s"
  (cond ((eql (length s) 1) (cons (list o (common-lisp:first s)) '() ))
	(t (cons(list o (common-lisp:first s))
		(xprod1 o (common-lisp:rest s))))))


(defun xprod-unlabelled-sets (s1 s2)
  "Return cross product of s1 and s2"
  (cond ((eql (length s1) 1)  (xprod1 (common-lisp:first s1) s2) )
	(t (union-unlabelled-sets (xprod1 (common-lisp:first s1) s2)
				  (xprod-unlabelled-sets (common-lisp:rest s1) s2)))))

(defun xprod (s1 s2)
  "Return cross product of s1 and s2"
  (check-type s1 set)
  (check-type s2 set)
  (cons :set (xprod-unlabelled-sets (rest s1) (rest s2))))
