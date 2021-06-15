LISP ?= sbcl

build:
	$(LISP) --load zender-sockets.asd \
	  --eval '(ql:quickload :zender-sockets)' \
	    --eval '(asdf:make :zender-sockets)' \
	    --eval '(quit)'
