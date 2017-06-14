(load "task6.lisp")

; TASK 7 - SECOND METRIC - Index of Coincidence
; Index of Coincidence (IoC) is the chance of two randomly selected letters being the same
; It can be used as a metric to determine if a given text is close to normal
; English text (all in reation to the frequency table of English letters)
; The average value for an English phrase is 0.0685. If the IoC of a given phrase
; falls near this value (to be defined in subsequent task), then one can make an assumption 
; regarding the safety of the text

; returns the Index of Coincidence of a phrase
(defun ioc (text)
  (setf N (length text))
  (setf freqs (n-grams text 1)) ; just get the frequency of individual letters
  (setf normalized (normalize freqs))
  (setf norm-sum (sum normalized))
  (setf sigma-random (random-coincidences N))
  (terpri)
  (format t "IoC Threshold: 0.067") 
  (terpri)
  (format t "Actual value: ~a" (/ norm-sum (float sigma-random))) 
  (terpri)
  (/ norm-sum (float sigma-random)) ; force float division
)

; for each frequency f, calculate f * (f - 1)
(defun normalize (alist)
  (setf lst ())  
  (loop for x in alist do
    (setf f (cdr x)) ; get frequency
    (setf lst (cons (* f (- f 1)) lst))
  )
  lst
)

; probability-of-random-coincidences - a constant, denoted as PORC here
(defconstant porc 0.0385)

; multiplies the probability of coincidences in any random text
(defun random-coincidences (text-length)
  ; (* porc (* text-length (- text-length 1)))
  (* text-length (- text-length 1))
)

; returns the sum of all elements of a list
(defun sum (lst)
  (cond 
    ((null lst) 0)
    ((eq (length lst) 1) 
      (car lst))
    (t 
      (+ (car lst) (sum (cdr lst)))
    )
  )
)

(defun task7-demo()
  (setf test (generate-word (+ 4 (random 20))))
  (format t "Index of coincidence for : ~a is ~a" test (ioc test)) 
  (terpri)
)

