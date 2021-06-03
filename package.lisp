;;;; package.lisp

(defpackage #:zender-sockets
  (:use #:cl
        #:alexandria
        #:clack
        #:websocket-driver
        #:cl-json
        #:trivia
        #:flexi-streams)
  (:export #:main))
