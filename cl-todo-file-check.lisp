;;; cl-todo-file-check.lisp
(in-package #:cl-todo-file-check)

(defparameter *todos* (make-array '(0) :adjustable t :fill-pointer t))
(defstruct todo line-number weight message)

(defun create-todo (line-number weight message)
    (make-todo :line-number line-number
	       :weight weight
	       :message message))

(defun add-todo (todo todo-list)
  (vector-push-extend todo todo-list))

(defun todo-heavier (a b)
  (> (todo-weight a) (todo-weight b)))

(defun todo-lighter (a b)
  (< (todo-weight a) (todo-weight b)))

(defun list-todos (todo-list direction)
  (if (equal direction "asc")
      (sort todo-list #'todo-lighter)
      (sort todo-list #'todo-heavier))
  )

(defun collect-lines-file (file)
  (with-open-file (stream file)
    ;; TODOO: Find a way to also collect the line numbers
    (loop for line = (read-line stream nil)
          while line
	  ;; TODO: Find a better way to make sure they are just the ones we want
	  when (and (search "TODO" line) (not (or (search "\"TODO" line) (search "'TODO" line))))
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

(defun weight-color (weight)
  (cond
    ((> weight 3) "31")
    ((> weight 1) "33")
    ("37")))

(defun print-todo (todo)
  (print
   (format t "LINE: ~c[1;32m~D~c[0m | PRIORITY: ~c[1;~Dm~D~c[0m~%> ~x"
	   #\ESC (todo-line-number todo) #\ESC
	   #\ESC (weight-color (todo-weight todo)) (todo-weight todo) #\ESC
	   (todo-message todo))
   ))

(defun main ()
  "Really don't need the *todos* global but let's leave it for now"
  (mapcar (lambda (it)
	    (add-todo (parse-todo it) *todos*))
	    (collect-lines-file "cl-todo-file-check.lisp"))

  (loop for todo in (coerce *todos* 'list)
	do (print-todo todo))) 
			 
		      
