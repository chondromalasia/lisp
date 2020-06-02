(defpackage :util
  (:use :common-lisp)
  (:export :elementp
	   :element
	   :bag))


(in-package util)

(defun elementp (o)
  (or
   (symbolp o)
   (characterp o)
   (numberp o)
   (packagep o)))

(deftype element ()
  "Elements are objects testable by EQL,
   symbols, characters, numbers and packages."
  '(satisfies elementp))


(defun bagp (o)
  "Not sure what the different between a list
   and a bag is, but this is a bag"
  (listp o))

(deftype bag ()
  '(satisfies bagp))



