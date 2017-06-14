; returns true or nil after checking if a list contains only one element 
(defun singleton-p (lst)
	(cond
		((null lst)
			NIL)
		((null (cdr lst))
			T)
	)
)

; returns the last element of a list
(defun rac (lst)
	(cond
		((singleton-p lst)
			(car lst))
		(T
			(rac (cdr lst))
		)
	)
)

; returns a list without the last element
(defun rdc (lst)
	(cond
		((singleton-p lst)
			())
		(T 
			(cons (car lst) (rdc (cdr lst)))
		)
	)
)

; returns a list with a given element appended to the end

(defun snoc (head tail)
	(append tail (cons head ()))
)

; checks a list to see if it's the same in both directions

(defun palindrome-p (lst)
	(cond
		((or (singleton-p lst) (eql lst()))
			T)
		((eql (car lst) (rac lst))
			(palindrome-p (cdr (rdc lst))))
		(T
			NIL)
	)
)

; returns an element from a list specified by the index
(defun select (index lst)
	(cond
		((eql index 0)
			(car lst))
		(T
			(select (- index 1) (cdr lst)))
	)	
)

; returns a random element from a list
(defun pick (lst)
	(select (random (length lst)) lst)
)

; returns the sum of all elements in a list
(defun sum (lst)
	(cond
		((or (singleton-p lst) (eq lst ()))
			(car lst))
		(T 
			(+ (car lst) (sum (cdr lst))))
	)
)

; returns the product of all elements in a list
(defun product (lst)
	(cond
		((or (singleton-p lst) (eq lst ()))
			(car lst))
		(T 
			(* (car lst) (product (cdr lst))))
	)
)

; returns a list containing numbers less than supplied non-negative integer
(defun iota (x)
	(cond
		((eql x 0) ())
		(T (snoc x (iota (- x 1))))
	)
)

; returns a list duplicated x times
(defun duplicate (x lo)
	(cond
		((eql x 1) 
			(lst lo))
		(T (cons lo (duplicate (- x 1)lo)))
	)
)

; returns factorial of an integer
(defun factorial (x)
	(product (iota x))
)

; returns an integer raised to a power
(defun power (base exponent)
	(cond 
		((= exponent 0) 1)
		(T (product (duplicate exponent base)))
	)
)

; returns a list of elements that are true to a given predicate
(defun filter-in (predicate lst)
    (cond
		((eql lst ())	
			()
		)
		((eql(funcall predicate(car lst))
			T)
			(cons(car lst) (filter-in predicate (cdr lst)))
		)
		(T
			(filter-in predicate (cdr lst))
		)
	)
)

; returns a list of elements that are false to a given predicate
(defun filter-out (predicate lst)
    (cond
		((eql lst ())	
			()
		)
		((eql(funcall predicate(car lst))
			NIL)
			(cons(car lst) (filter-in predicate (cdr lst)))
		)
		(T
			(filter-out predicate (cdr lst))
		)
	)
)

; returns the list with all occurrences of an object removed
(defun take-from (obj lst)
    (cond
		((eql lst ())	
			()
		)
		((not(eql obj (car lst)))
			(cons(car lst) (take-from obj (cdr lst)))
		)
		(T
			(take-from obj (cdr lst))
		)
	)
)

; returns a list with the first occurence of an object removed
(defun take-first (obj lst)
    (cond
        ((eql lst ())
        	()
    	)
    	((eq obj (car lst))
            (cdr lst))
        (t
            (cons (car lst) (take-first obj (cdr lst))))
    )
)

; takes a lst and returns a list with random permutation of the elements
(defun random-permutation (lst)
    (cond
        ((null lst)
            ())
        (T
            (setf element (pick lst))
            (setf remainder (take-first element lst))
            (cons element (random-permutation remainder)))
	)
)

(defun contains (element lst)
  (cond
    ((null lst) nil)
    ((eq (car lst) element) T)
    (T (contains element (cdr lst)))
  )
)