;; TASK 3 - Separating tokens into n-grams

(load "task2.lisp")

;; collection of words, an alist
; (setf words ())

(defun n-grams (str n)
  (setf words ())
  (loop for x from 0 and y from n until (> y (length str)) do 
    (insert (read-from-string (subseq str x y)))
  )
  (terpri)
  words
)

(defun prepare (filename)
  (setf sample (file-string filename))
  (setf sample (strip-punctuation sample " ,.':;!@#$%^&*()_+-=[]`~?/<>\"1234567890"))
  (setf sample (string-downcase sample))
)

(defun insert (key)
  (cond
    ((eq (car (assoc key words)) key) ; check if  key already exists
      (rplacd (assoc key words) (+ 1 (cdr (assoc key words)))) ; increment the value
    )
    (t
      (push (cons key 1) words) ; add new key with value 1
    )
  )
)