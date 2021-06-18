;;;; package.lisp

(defpackage #:zender-sockets
  (:use #:cl
        #:alexandria
        #:clack
        #:websocket-driver
        #:cl-json
        #:dexador
        #:slynk
        #:trivia
        #:flexi-streams)
  (:shadowing-import-from :dexador :get :delete)
  (:export #:main))
