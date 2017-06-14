(string-trim 
  '(#\Space #\Newline #\Backspace #\Tab 
    #\Linefeed #\Page #\Return #\Rubout)
  "  A string   "
)

(defun greater (n lst)
	(cond 
		((null lst) nil)

		((> (car lst) n) 
			(cons (car lst) lst)
		)
		
		(t 
			(greater n (cdr lst))
		)
	)
)

