(make-package :ch16 :use '(:cl))

(in-package ch16)

(load "~/Projects/lisp/practice/util.lisp")


;iunno
(shadow 'length)
(defun length (l)
  "Return number of members of list"
  (check-type l list)
  (if (null l) 0
      (1+ (length (rest l)))))

(defun member (obj l)
  "Return True if OBJ is equal to a member of list L"
  (check-type l list)
  (cond ((null l) nil)
	((eql obj (first l)) l)
	(t (member obj (rest l)))))

(defun before (e1 e2 l)
  "Returns True if element e1 occurs before the element e2
    in the list l"
  (check-type e1 (satisfies util::elementp))
  (check-type e2 (satisfies util::elementp))
  (check-type l list)
  (> (length (member e1 l))
     (length (member e2 l))))

(defun number-listp (l)
  "Return T if all members of the list L are numbers,
   NIL otherwise"
  (check-type l list)
  (cond ((null l) t)
	((numberp (first l)) (number-listp (rest l)))
	(t nil)))

(defun same-length1 (l1 l2)
  "Return T if lists l1 and l2 have the same length,
   NIL otherwise"
  (check-type l1 list)
  (check-type l2 list)
  (= (length l1) (length l2)))

(defun same-length2 (l1 l2)
  "Return T if lists l1 and l2 have the same length,
   NIL otherwise"
  (check-type l1 list)
  (check-type l2 list)
  (cond ((null l1) (null l2))
	((null l2) nil)
        (t (same-length2 (rest l1) (rest l2)))))

(shadow 'count)
(defun count (e l)
  "Return number of times element e appears in list l"
  (check-type e (satisfies util::elementp))
  (check-type l list)
  (cond ((null l) 0)
	((eql e (first l)) ( 1+ (count e (rest l))))
	(t (count e (rest l)))))
	
(defun eql-lelt (l1 l2)
  "Return T if corresponding members of l1 l2 are the same,
   nil if not"
  (check-type l1 list)
  (check-type l2 list)
  (cond ((null l1) (null l2))
	((null l2) nil)
	((eql (first l1) (first l2))
	 (eql-lelt (rest l1) (rest l2)))
	(t nil)))


(shadow 'nth)
(defun nth (n l)
  "Return the nth element of list l, nil if n exceeds size of list"
  (check-type n integer)
  (check-type l list)
  (cond ((> n 0) (nth (1- n) (rest l)))
	((< n 1) (first l))
	(t nil)))

(defun allbut (n l)
  "Return a list whole members are the members of l omitting the first n"
  (check-type n integer)
  (check-type l list)
  (cond ((eql n 0) l)
	((null (null l)) (allbut (- n 1) (rest l)))
	(t nil)))

(shadow 'assoc)
(defun assoc (e al)
  "Return the first element of al whose first member is eql to e"
  
  (check-type e (satisfies util::elementp))
  (check-type al list)

  (cond ((null al) nil)
	(eql e (first (first al))) (first al))
	(t (assoc e (rest al)))))
