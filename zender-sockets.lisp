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

(defun body-to-string (stream)
  (if (listen stream)
      (alexandria:read-stream-content-into-string stream)
      ""))

(defun decode-json-from-string-wrapped (string)
  (ignore-errors
   (json:decode-json-from-string string)))

(defun run-http-server (env)
  (trivia:match env
                ((plist :request-method request-method
                        :raw-body       raw-body)
                 (let ((data (decode-json-from-string-wrapped (body-to-string raw-body))))
                   (trivia:match data
                                 ((alist (:cmd . "READ"))
                                  `(200 (:content-type "application/json") ("leer")))
                                 ((alist (:cmd . "READ-ALL"))
                                  `(200 (:content-type "application/json") ("leer todo")))
                                 ((alist (:cmd . "WRITE"))
                                  `(200 (:content-type "application/json") ("escribir")))
                                 (_ '(200 (:content-type "application/json") ("invalid request kiddo"))))))
                (_ '(200 (:content-type "application/json") ("fuko")))))

(defun main ()
  (defvar *ws-handler*   (clack:clackup #'run-ws-server   :port 8086))
  (defvar *http-handler* (clack:clackup #'run-http-server :port 8073)))
