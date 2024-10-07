(defparameter *todos* (make-array '(0) :adjustable t :fill-pointer t))
(defstruct todo line-number weight message)

(defun create-todo (line-number weight message)
    (make-todo :line-number line-number
	       :weight weight
	       :message message))

(defun add-todo (todo todo-list)
  (vector-push-extend todo todo-list))

;;; TODO: Remove this stuff from here
;;; (add-todo (create-todo 10 1 "message a") *todos*)
;;; (add-todo (create-todo 20 2 "message b") *todos*)

(defun todo-heavier (a b)
  (> (todo-weight a) (todo-weight b)))

(defun todo-lighter (a b)
  (< (todo-weight a) (todo-weight b)))

(defun list-todos (todo-list direction)
  (if (equal direction "asc")
      (sort todo-list #'todo-lighter)
      (sort todo-list #'todo-heavier))
  )

(defun collect-lines-file ()
  (with-open-file (stream "test.txt")
    ;; TODOO: Find a way to also collect the line numbers
    (loop for line = (read-line stream nil)
         while line
	 when (search "TODO" line)
          collect line)))

(defun measure-weight (line)
  "Measures how long the TODO is by counting consecutive 'O's after 'TODO'.
   For example, 'TODO' returns 1, 'TODOOO' returns 3."
  (let ((begin (search "TODO" line)))
    (if begin ;; lets put this here just for caution
        (let ((acc 1)
              (i (+ begin 4)))
          (loop
            while (and (< i (length line)) (char= (char line i) #\O))
            do (progn
                 (incf acc)
                 (incf i)))
          acc)
        0)))  

(defun parse-todo (line)
  (let ((parts (str:split ":" line :omit-nulls t)))
    (create-todo 0 (measure-weight
		    (car parts))
		 (str:trim (str:join " " (cdr parts))))))
