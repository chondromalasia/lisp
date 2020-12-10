(defpackage bstree

  (:shadow :member
	   :bstree)
  (:use :common-lisp)
  (:export :bstree
	   :bstreep
	   :insert
	   :root
	   :left
	   :right
	   :member
	   :build-from-list
	   :inorder))

(in-package bstree)

(load "~/Projects/lisp/practice/util.lisp")

(defun bstreep (tree)
  "A bstree is either an element, or a three-member list,
   the first of which is an element."
  (or (typep tree 'util:element)
      (and (listp tree)
	   (= (length tree) 3)
	   (typep (first tree) 'util:element))))

(deftype bstree ()
  "A bstree is a binary search tree.
   See predicate bstreep for details."
  '(satisfies bstreep))

(defun root (tree)
  "Returns the root element of the binary search tree TREE,
   NIL if TREE is empty"
  (check-type tree bstree)
  (if (atom tree) tree
      (first tree)))

(defun left (tree)
  "Returns the left subtree of the binary search tree TREE,
   NIL if empty or has an empty left subtree"
  (check-type tree bstree)
  (if (atom tree) '()
      (second tree)))

(defun right (tree)
  "Returns the right subtree
          of the binary search tree TREE,
   NIL if TREE is empty or has an empty right subtree."
  (check-type tree bstree)
  (if (atom tree) '()
      (third tree)))

(defun insert (elt tree)
  "Returns the binary search tree TREE
   with the element ELT inserted in the proper place."
  (check-type elt util:element)
  (check-type tree bstree)
  (cond
    ((null tree) elt)
    ((eql elt (root tree)) tree)
    ((string< elt (root tree))
     (list (root tree)
	   (insert elt (left tree))
	   (right tree)))
    (t (list (root tree)
	     (left tree)
	     (insert elt (right tree))))))

(defun member (elt tree)
  "Return T if ELT is stored in the binary search tree TREE;
   NIL otherwise."
  (check-type elt util:element)
  (check-type tree bstree)
  (cond ((null tree) nil)
	((eql elt (root tree)) t)
	((string< elt (root tree))
	 (member elt (left tree)))
	(t (member elt (right tree)))))

(defun build-from-list (elist)
  "Convert a list of elemental members into a binary search tree"
  (cond ((eql (first elist) nil) '())
	(t (insert (first elist) (build-from-list (rest elist))))))

(defun inorder (tree)
  "Return binary search tree as list in alphabetical order"
  (check-type tree bstree)
  (cond ((null tree) '())
	((atom tree) (list tree))
	(t (append (inorder (left tree))
		   (cons (root tree) (inorder (right tree)))))))
