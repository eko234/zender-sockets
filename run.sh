#!/bin/sh

sbcl --eval '(asdf:load-system :zender-sockets)'\
     --eval '(zender-sockets:main)'

