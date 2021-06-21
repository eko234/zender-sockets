;;;; package.lisp

(defpackage #:utils
  (:use #:cl
        #:cl-json
        #:trivia
        #:alexandria
        #:dexador)
  (:shadowing-import-from :dexador
                          :get
                          :delete)
  (:export #:get-auth-data
           #:invalid-req-handler
           :with-generic-error-handler
           :match-with-request))

(defpackage #:zender-sockets
  (:use #:cl
        #:alexandria
        #:clack
        #:websocket-driver
        #:cl-json
        #:dexador
        #:slynk
        #:trivia
        #:flexi-streams
        #:utils)
  (:shadowing-import-from :dexador :get :delete)
  (:export #:main))
