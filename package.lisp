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

(defpackage #:utils
  (:use #:cl
        #:cl-json
        #:dexador)
  (:shadowing-import-from :dexador
                          :get
                          :delete)
  (:export #:get-auth-data
           #:with-generic-error-handler
           #:match-with-request))
