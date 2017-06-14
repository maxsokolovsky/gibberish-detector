; TASK 6 - READ MANY EXAMPLES AND CALCULATE AN AVERAGE DANGER VALUE

; This function works on a large set of data sources (files with text) to 
; run the previous functions and calculate the master safety value as a 
; future reference.

; The files have been fetched to a folder using a JavaScript program I wrote.

; From the results of the demo, we can get an idea of an average safety value for 
; regular English text and deliberate gibberish.

(load "task5.lisp")


(defun average (lst)
  (float (/ (reduce #'+ lst) (length lst)))
)

(defun maximum (lst)
  (reduce #'max lst)
)

(defun minimum (lst)
  (reduce #'min lst)
)

(defun task6-demo()
  (setf values ())
  (loop for f in (directory "quotes/*")
  do 
    (setf grams (n-grams (prepare f) 2)) 
  do 
    (print f)
    (setf values (cons (calculate-safety grams) values))
  )

  (terpri) (terpri)

  (format t "=====================") (terpri)
  (format t "Number of files scanned: ~a" (length values)) (terpri)

  (format t "The average safety value is: ~a" (average values)) (terpri)
  (format t "The max safety value is: ~a" (maximum values)) (terpri)
  (format t "The min safety value is: ~a" (minimum values)) (terpri)
)

(defun js-gibberish-demo()
  (setf values ())
  (loop for f in (directory "trash/*")
  do 
    (setf grams (n-grams (prepare f) 2)) 
  do 
    (print f)
    (setf values (cons (calculate-safety grams) values))
  )

  (terpri) (terpri)

  (format t "=====================") (terpri)
  (format t "Number of files scanned: ~a" (length values)) (terpri)
  (format t "The average safety value is: ~a" (average values)) (terpri)
  (format t "The max safety value is: ~a" (maximum values)) (terpri)
  (format t "The min safety value is: ~a" (minimum values)) (terpri)
)

(defun my-gibberish-demo()
  (setf values ())
  (loop for x upto 100 
  do
    (setf text (generate-word (+ 4 (random 100)))) 
    (setf grams (n-grams text 2)) 
  do 
    (print text)
    (setf values (cons (calculate-safety grams) values))
  )

  (terpri) (terpri)

  (format t "=====================") (terpri)
  (format t "Number of phrases scanned: ~a" (length values)) (terpri)
  (format t "The average safety value is: ~a" (average values)) (terpri)
  (format t "The max safety value is: ~a" (maximum values)) (terpri)
  (format t "The min safety value is: ~a" (minimum values)) (terpri)
)

