;;;; package.lisp

(defpackage #:zender-sockets
  (:use #:cl
        #:alexandria
        #:clack
        #:websocket-driver
        #:cl-json
        ; #:dexador
        #:trivia
        #:flexi-streams)
  (:export #:main))
