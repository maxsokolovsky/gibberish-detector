; TASK 5 - CALCULATE THE SAFETY VALUE

; The idea is to calculate a frequency value of a bigram with respect to 
; a general frequency value from an English language reference.
; For example, in a given text, the frequency of of syllable 'zx' is multiplied 
; by a relative scalar value in the reference.

; So, the syllable 'zx' occurs 2 times in the reference. 
; Its frequency is relative to frequencies of other syllables ('ca' occurs, for instance, 100 times as often)

; All values of all bigrams are added up in the end to produce a safety value of a given text;
; The reference is an abstract model of the language  that derives from a large sample body of text.
; This way, it is flexible and can be refined.

(load "task4.lisp")

(setf test (n-grams "zqoaxkwziu" 2))
(setf reference (n-grams (prepare "oneliner.txt") 2))

(defun calculate-safety (grams)
  (setf result 1)
  (loop for x in grams do
    (setf grams-key (car x))
    (setf grams-value (cdr x))

    (if (assoc grams-key reference) ; if a bigram exists in the reference
      (let ()
        (setf result (* result (* grams-value (cdr (assoc (car x) reference)))))
        ; (print (assoc grams-key reference))
      )
    )
  )
  (format t "Raw value for bigram frequency: ~a" result) 
  (terpri)
  (format t "Normalized: ~a" (expt result (/ 1 (- (length grams) 1))))
  (expt result (/ 1 (- (length grams) 1)))
) 

