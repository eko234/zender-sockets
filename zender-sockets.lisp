;;;; zender-sockets.lisp

(in-package #:zender-sockets)

(defvar *connections* (make-hash-table :test 'equal))

(defun get-auth-data (data)
  (format T "~a ~%"(with-input-from-string
                       (s (dexador:post "http://localhost:8087/validate"
                                        :headers '(("content-type" . "application/json"))
                                        :content (cl-json:encode-json-to-string `(("secret" . ,data)))))
                     (json:decode-json s))))

(defclass client ()
  ((connection :initarg :connection
               :accessor connection)
   (mailbox :initform (list)
            :accessor mailbox)))

(defun find-client-by-conn (conn)
  (trivia:match 
   (loop for client being each hash-value in *connections*
         when (equal conn (connection client))
         collect client)
   ((list client) client)
   (_ NIL)))

(defun find-id-by-conn (conn)
  (trivia:match 
   (loop for id being each hash-key in *connections*
         for client being each hash-value in *connections*
         when (equal conn (connection client))
         collect id)
   ((list id) id)
   (_ NIL)))

(defgeneric push-to-mailbox (obj message)
  (:documentation "puts message atop of message mailbox")
  (:method (obj message)
    (declare (ignorable obj))
    (format t "data was lost: ~a ~%" message)))

(defmethod push-to-mailbox ((obj client) msg)
  (let* ((mailbox (mailbox obj)))
    (setf mailbox (cons msg mailbox))))

(defun handle-new-connection (con)
  (let ((auth-data (get-auth-data (gethash 'key (websocket-driver.ws.server::headers con)))))
    (trivia:match auth-data
                  ((alist 
                    (:id    . id)
                    (:valid .  T))
                   (trivia:match (gethash id *connections*)
                                 (NIL (setf (gethash id *connections*) 
                                            (make-instance 'client :connection con)))
                                 (client (setf (connection client)
                                               con))))
                  (_ NIL))))

(defun read-from-conn (id dumping)
  (let* ((client (gethash id *connections*))
         (mailbox (mailbox client))
         (message (car mailbox)))
    (when destroying (setf (mailbox client) (cdr mailbox)))
    (list message)))

(defun write-to-conn (id data)
  (let ((client (gethash id *connections*))
        (connection (connection client)))
    (websocket-driver:send connection data)))

(defun handle-close-connection (connection)
  (let ((message (format nil " .... ~a has left."
                         (gethash connection *connections*))))
    (remhash (find-client-by-conn connection) *connections*)))

(defun run-ws-server (env)
  (let ((ws (websocket-driver:make-server env)))
    (websocket-driver:on :open ws
                         (lambda () (handle-new-connection ws)))
    (websocket-driver:on :message ws
                         (lambda (msg) (push-to-mailbox (find-client-by-conn ws) msg)))
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
                                 ((alist (:cmd . "READ")
                                         (:id . id))
                                  `(200 (:content-type "application/json") ((read-from-conn id NIL))))
                                 ((alist (:cmd . "READ-ALL")
                                         (:id . id))
                                  `(200 (:content-type "application/json") ((read-from-conn id NIL))))
                                 ((alist (:cmd . "WRITE")
                                         (:id . id)
                                         (:data . data))
                                  `(200 (:content-type "application/json") ((write-to-conn id data))))
                                 (_ '(200 (:content-type "application/json") ("invalid request kiddo"))))))
                (_ '(200 (:content-type "application/json") ("fuko")))))

(defun main ()
  (defvar *ws-handler*   (clack:clackup #'run-ws-server   :port 8086))
  (defvar *http-handler* (clack:clackup #'run-http-server :port 8073)))
