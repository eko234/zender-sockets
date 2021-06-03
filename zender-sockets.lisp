;;;; zender-sockets.lisp

(in-package #:zender-sockets)

(defvar *connections* (make-hash-table))

;; QUERY AGAINST Zender-backend sending a 
;; json containing the key header
(defun handle-new-connection (con)
  (format t "FIRE ~%")
  (setf (gethash con *connections*)
        (format nil "user-~a" (random 100000))))

(defun broadcast-to-client (connection message)
  (let ((message (format nil "~a: ~a"
                         (gethash connection *connections*)
                         message)))
    (loop :for con :being :the :hash-key :of *connections* :do
             (websocket-driver:send con message))))

(defun handle-close-connection (connection)
  (let ((message (format nil " .... ~a has left."
                         (gethash connection *connections*))))
    (remhash connection *connections*)))

(defun run-ws-server (env)
   (let ((ws (websocket-driver:make-server env)))
    (websocket-driver:on :open ws
                         (lambda () (handle-new-connection ws)))
    (websocket-driver:on :message ws
                         (lambda (msg) (broadcast-to-client ws msg)))
    (websocket-driver:on :close ws
                         (lambda (&key code reason)
                           (declare (ignore code reason))
                           (handle-close-connection ws)))
    (lambda (responder)
      (declare (ignore responder))
      (websocket-driver:start-connection ws))))

;; handles internal requests to communicate with the sockets server
(defun run-http-server ()
  (clack:clackup
   (lambda (env)
     (declare (ignore env))
     '(200 (:content-type "text/plain") ("hello fren")))
   :port 8073))

(defun main ()
  (defvar *ws-handler* (clack:clackup #'run-ws-server :port 8086))
  (defvar *http-handler* (run-http-server))
  )
