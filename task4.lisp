;; TASK 4 - establishing a model of the English language, 
;; creating a database that can be queried for simple statistics and
;; generating gibberish text of variable length

(load "task3.lisp")

(defun task4-demo ()
  (prepare 'a.txt)
  (n-grams sample 2)
  ; (ascend words)
  ; (descend words)
  ; (top words 10)
  ; (bottom words 20)
  ; (generate-word 6)
  ; (generate-phrase 100)
)

(task4-demo)

(defun ascend (alist)
  (sort alist #'(lambda (a b) (> (cdr a) (cdr b))))
)

(defun descend (alist)
  (sort alist #'(lambda (a b) (< (cdr a) (cdr b))))
)

(defun top (alist n)
  (subseq (ascend alist) 0 n)
)

(defun bottom (alist n)
  (subseq (descend alist) 0 n)
)

(setf alphabet "abcdefghijklmnopqrstuvwxyz")
(setf punctuation ",.:!;? ")

(defun generate-word (n)
  (setf word "")
  (dotimes (i n)
    (setf index (random 26))
    (setf word (concatenate 'string word (subseq alphabet index (+ index 1))))
  )
  word
)

(defun generate-phrase (n)
  (setf phrase "")
  (dotimes (i n)
    (setf word-length (+ (random 12) 1))
    (setf phrase (concatenate 'string phrase (generate-word word-length)))
    (setf phrase (concatenate 'string phrase (get-punctuation)))
    ; (setf phrase (concatenate 'string phrase " "))
  )
  phrase  
)

(defun get-punctuation ()
  (setf p (- (length punctuation) 1))
  (setf index (random p))
  (cond 
    ((> (random 100) 75) 
      (concatenate 'string (subseq punctuation index (+ index 1)) " ")
    )
    (t
      " "
    )
  )
)
