(defpackage :ch18

  (:shadow :tree-equal
           :copy-tree
           :subst)
  (:use :common-lisp))

(in-package ch18)

(defun atom-equal (a1 a2)
  "Return true if the atoms passed are equal, using the correct
   type of things"
  ;; note there's supposed to be a package p...
  (check-type a1 util:element)
  (check-type a2 util:element)

  (if (eql (type-of a1) (type-of a1))
      (typecase a1
	(symbol (eq a1 a2))
	(character (char-equal a1 a2))
	(number (= a1 a2)))))

(defun tree-equal (t1 t2)
  "Returns T if T1 and T2 are trees with:
   1. the same structure
   2. equal corresponding leaves (according to atom-equal);
  NIL otherwise"
  ;; T1 and T2 can be any objects
  (cond ((atom t1) (atom-equal t1 t2))
	((atom t2) nil)
	((tree-equal (first t1) (first t2))
	 (tree-equal (rest t1) (rest t2)))
	(t nil)))

(defun tree-sort (l)
  "Sort a list in alphabetial order using trees"
  (check-type l list)
  (bstree::inorder (bstree::build-from-list l)))

(defun max-depth-members (list)
  "Returns the maximum depth of the members
   of the argument LIST."
  (check-type list list)
  (if (null list) 0
      (max (depth (first list))
	   (max-depth-members (rest list)))))

(defun depth (tree)
  "Returns the depth of the argument TREE"
  (if (atom tree) 0
      (1+ (max-depth-members tree))))


(defun copy-tree (tree)
  "Returns a copy of the TREE,
   copying at all levels."
  (typecase tree
    (atom tree)
    (cons (cons (copy-tree (first tree))
		(copy-tree (rest tree))))))

(defun subst (new old tree)
  "Return a copy of tree where all olds are
   replaced by new"
  (cond ((equal old tree) new)
	((atom tree) tree)
	(t (cons (subst new old (first tree))
		 (subst new old (rest tree))))))
	
(defun flatten (tree)
  "Return a list of atoms that have the same
   left to right order in the original tree"
  (cond ((null tree) '())
        ((atom tree) (list tree))
	(t (append (flatten (first tree))
		   (flatten (rest tree))))))

(defun flatten2 (tree)
  (cond ((null tree) nil))
	((atom tree) (list tree))
	(t (append (flatten (first tree))
		   (flatten (rest tree))))))

;; this doesn't work
;; but (flatten2 (a () b)) => (a nil b)
(defun flatten2 (tree)
  (typecase tree
    (null '())
    (atom (list tree))
    (cons (append (flatten2 (first tree))
		  (flatten2 (rest tree))))))

(defun flatten2 (tree)
  "Return a flattened tree with null lists
   returned as nil; '(a () b) => (a nil b)"
  (cond
    ((null tree) '())
    ((atom tree) (list tree))
    ((null (car tree)) (append (list nil)
				(flatten2 (rest tree))))
    (t (append (flatten2 (first tree))
	       (flatten2 (rest tree))))))
    
	    

