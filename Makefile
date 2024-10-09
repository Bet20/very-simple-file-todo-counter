all: build
build:
	sbcl --load cl-todo-file-check.asd \
		--eval '(ql:quickload :cl-todo-file-check)' \
		--eval "(sb-ext:save-lisp-and-die #p\"cl-todo-file-check\" :toplevel #'cl-todo-file-check:main :executable t)"
