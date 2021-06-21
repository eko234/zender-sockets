;;;; zender-sockets.asd

(asdf:defsystem #:zender-sockets
  :description "Describe zender-sockets here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("alexandria"
               "trivia"
               "websocket-driver"
               "dexador"
               "clack"
               "slynk"
               "cl-json"
               "flexi-streams")
  :components ((:file "package")
               (:file "zender-sockets")
               (:file "utils"))
  :build-operation "program-op"
  :build-pathname "zensoc"
  :entry-point "zender-sockets:main")
