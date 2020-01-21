; A mock database to store financial info


;; database initialize
(defvar *db* nil)

;; makes a list given the data, a database entry, if you will
(defun add_fundamental_data (symbol price total_assets total_liabilities)
 (list :symbol symbol :price price :total_assets total_assets :total_liabilities total_liabilities)
)

;; basically appends a record to the database
(defun add_fund_to_db (record)
  (push record *db*))

;; goes through each record and prints it
;; I'm not gonig to worry about formating now
(defun dump-db ()
  (dolist (record *db*)
    (format t "~{~a:~10t~a~%~}~%" record))
  )


;; prompts for a stock
(defun prompt_stock (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;; Prompt for values for each 
(defun prompt_for_entry ()
  (add_fundamental_data
   (prompt_stock "Ticker")
   (float (or (parse-integer (prompt_stock "Price") :junk-allowed t) 0))
   (or (parse-integer (prompt_stock "Total Assets") :junk-allowed t) 0)
   (or (parse-integer (prompt_stock "Total Liabilities") :junk-allowed t) 0)
   ))
  
;; axes if you want to add another
(defun loop_add_fundy ()
  (loop (add_fund_to_db (prompt_for_entry))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return)))
  )

;; saves database
(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out)))
  )

;; load database
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in))))
  )

(defun select-by-ticker (ticker)
  #'(lambda (entry) (equal (getf entry :symbol) ticker)))

;; a meta function that allows you to select via a function
; (select (select-by-ticker "MMM"))
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))


;; (select (where :symbol "MMM" :price 160.58))
;; note that there's a version for true false
;; let's say there was an entry with the name public
;; then when we passed it we'd have to have:
;; (public nil public-p)
;; and then at the end it would be p much th esame
(defun where (&key symbol price total_assets total_liabilities)
  #'(lambda (entry)
      (and
       (if symbol (equal (getf entry :symbol) symbol) t)
       (if price (equal (getf entry :price) price) t)
       (if total_assets (equal (getf entry :total_assets) total_assets) t)
       (if total_liabilities (equal (getf entry :total_liabilities) total_liabilities) t))))

;; (update (where :symbol "MMM") :price 69.0)
(defun update (selector-fn &key symbol price total_assets total_liabilities)
  (setf *db*
	(mapcar
	#'(lambda (row)
	    (when (funcall selector-fn row)
	      (if symbol (setf (getf row :symbol) symbol))
	      (if price (setf (getf row :price) price))
	      (if total_assets (setf (getf row :total_assets) total_assets))
	      (if total_liabilities (setf (getf row :total_liabilities) total_liabilities)))
	    row) *db* )))

;; (delete-rows (where :symbol "MMM"))
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))


;; Look, there's a more ~*efficient*~ way to do this
;; but I think it's above my paygrade for the time being
;; nonetheless: ^-f Removing Duplication and Winning Big
	      




;(save-db "~/Projects/lisp/practice/dummy_markets.db")

(load-db "~/Projects/lisp/practice/dummy_markets.db")
