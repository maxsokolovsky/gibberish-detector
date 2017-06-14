;; TASK 2 - Stripping off punctuation and putting tokens into a list

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data
    )
  )
)

(defun string-to-list (str)
  (if (not (streamp str))
  (string-to-list (make-string-input-stream str))
  (if (listen str)
    (cons (read str) (string-to-list str))
      nil
    )
  )
)

(defun strip-punctuation (str punctuation)
  (remove-if (lambda (ch) (find ch punctuation)) str)
)

(defun task2-demo (filename)
  (setf sample (file-string filename))
  (setf sample (strip-punctuation sample ",.:"))
  (setf text (string-to-list sample))
)