; TASK 8 - MACHINE CALCULATES IoC VALUES AND GATHER STATISTICS

(load "task7.lisp")

(defun ioc-demo-gibberish ()
  (setf values ())
  (loop for f in (directory "trash/*")
  do 
    (print f)
    (setf values (cons (ioc (prepare f)) values))
  )

  (terpri) (terpri)

  (format t "=====================") (terpri)
  (format t "Number of files scanned: ~a" (length values)) (terpri)

  (format t "The average safety value is: ~a" (average values)) (terpri)
  (format t "The max safety value is: ~a" (maximum values)) (terpri)
  (format t "The min safety value is: ~a" (minimum values)) (terpri)
)

(defun ioc-demo-normal ()
  (setf values ())
  (loop for f in (directory "quotes/*")
  do 
    (print f)
    (setf values (cons (ioc (prepare f)) values))
  )

  (terpri) (terpri)

  (format t "=====================") (terpri)
  (format t "Number of files scanned: ~a" (length values)) (terpri)

  (format t "The average safety value is: ~a" (average values)) (terpri)
  (format t "The max safety value is: ~a" (maximum values)) (terpri)
  (format t "The min safety value is: ~a" (minimum values)) (terpri)
)
