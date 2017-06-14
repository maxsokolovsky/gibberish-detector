; TASK 10 - USING THE LEARNED THRESHOLDS, PUTTING EVERYTHING TOGETHER AND MAKING AN INTERACTIVE ENVIRONMENT

(load "task9.lisp")

(setf first 10) ; first metric - learned in task 6
(setf second 0.05) ; second metric (IoC) - learnd in task 8
(setf third 4000000) ; third metric (markov chain) - learned in task 9

(train) ; train for markov chain

(defun check (str)
  (setf str (string str))
  (setf grams (n-grams str 2))
  (if 
    (and 
      (> (calculate-safety grams) first)
      (> (ioc str) second)
      (> (average-transition-prob str counts) third)
    )
    "This text is good!"
    "This text is probably gibberish"
  )
)