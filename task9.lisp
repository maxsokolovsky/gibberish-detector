; TASK 9 - 2-CHARACTER MARKOV CHAIN

; Based on https://github.com/rrenaud/Gibberish-Detector and ported to Common Lisp



(load "lp.l")
(load "task8.lisp")

(setf allowed "abcdefghijklmnopqrstuvwxyz")
(setf threshold 0)

;; make-list does not let replace individual elements
; (setf counts (make-list (length alphabet) :initial-element (make-list (length alphabet) :initial-element 10)))

(defun make-alphabet ()
  (setf alph ())
  (loop for x from 0 and y from 1 until (> y (length allowed)) do
    (push (cons (read-from-string (substring allowed x y)) x) alph)
  )
  alph 
)

(defun 2-gram (str)
  (setf gr ())
  (loop for x from 0 and y from 2 until (> y (length str)) do
    (setf gr (snoc (read-from-string (subseq str x y)) gr))
  )
  gr
)

(defun average-transition-prob (text counts)
  (setf log-prob 0)
  (setf transition-ct 0)
  
  (loop for x in (2-gram text) do
    (setf a (subseq (string x) 0 1))
    (setf b (subseq (string x) 1 2))
    (setf a-pos (cdr (assoc (read-from-string a) pos)))
    (setf b-pos (cdr (assoc (read-from-string b) pos)))

    (setf log-prob (+ log-prob (nth a-pos (nth b-pos counts))))
    (setf transition-ct (+ 1 transition-ct))
  )
  (terpri)
  (format t "Threshold for markov chain: ~a" threshold) 
  (terpri)
  (format t "Actual value: ~a" (exp (/ log-prob transition-ct))) 
  (terpri) (terpri)
  (exp (/ log-prob transition-ct))
) 

(defun train ()
  (setf counts '((10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)
   (10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10))
  )

  ; (setf grams (2-gram "hellomyfriendswelcometomyicecreamshop"))
  (setf grams (2-gram (prepare "oneliner.txt")))
  (loop for x in grams do
    (setf a (subseq (string x) 0 1))
    (setf b (subseq (string x) 1 2))
    (setf a-pos (cdr (assoc (read-from-string a) pos)))
    (setf b-pos (cdr (assoc (read-from-string b) pos)))
    (setf (nth a-pos (nth b-pos counts)) (+ 1 (nth a-pos (nth b-pos counts))))
  )

  (loop for x from 0 upto 25 do 
    (setf s (sum (nth x counts)))
    (loop for y in '(1 2 3 4 5 6 7 8 9 10) do
      (setf (nth x (nth y counts)) (log (/ (nth x (nth y counts)) s)))
    )
  )

  (setf good-probs ())
  (setf bad-probs ())

  (loop for f in (directory "quotes/*")
  do 
    (setf good-probs (cons (average-transition-prob (prepare f) counts) good-probs))
  )

  (loop for f in (directory "trash/*")
  do 
    (setf bad-probs (cons (average-transition-prob (prepare f) counts) bad-probs))
  )

  (setf threshold (/ (+ (minimum good-probs) (maximum bad-probs)) 2))
)

(setf pos (make-alphabet))
; (train)

(defun markov (str)
  (setf str (string str))
  (if (> (average-transition-prob str counts) threshold)
    "Safe"
    "Gibberish"
  )
)




