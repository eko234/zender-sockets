;;;; zender-sockets.lisp
(in-package #:zender-sockets)

;;;CONNECTION DATABASE
(defvar *connections* (make-hash-table :test 'equal))

;;;CLIENT
(defclass client ()
  ((connection :initarg :connection
               :accessor connection)
   (mailbox :initform (list)
            :accessor mailbox)))

(defgeneric push-to-mailbox (obj message)
  (:documentation "puts message atop of message mailbox")
  (:method ((obj null) message)
    (declare (ignorable obj))
    (format t "null object, data was lost: ~a ~%" message))
  (:method (obj message)
    (declare (ignorable obj))
    (format t "non-specific object, data was lost: ~a ~%" message)))

(defmethod push-to-mailbox ((obj client) msg)
  (setf (mailbox obj) (cons msg (mailbox obj))))

(defun find-client-by-conn (conn)
  (trivia:match 
   (loop for client being each hash-value in *connections*
         when (equal conn (connection client))
         collect client)
   ((list client) client)
   (_ NIL)))

(defun find-id-by-conn (conn)
  (trivia:match 
   (loop for id     being each hash-key   in *connections*
         for client being each hash-value in *connections*
         when (equal conn (connection client))
         collect id)
   ((list id) id)
   (_ NIL)))

(defun handle-new-connection (con)
  (let ((auth-data (get-auth-data (gethash "key" (websocket-driver.ws.server::headers con)))))
    (trivia:match auth-data
                  ((alist (:result . "OK")
                          (:id . id)
                          (:valid . T))
                   (trivia:match (gethash id *connections*)
                                 (NIL (setf (gethash id *connections*) 
                                            (make-instance 'client :connection con)))
                                 (client (setf (connection client)
                                               con))))
                  (_  NIL))))

(defun read-one-from-conn (id dumping)
  (let* ((client (gethash id *connections*))
         (mailbox (mailbox client))
         (message (car mailbox)))
    (when dumping (setf (mailbox client) (cdr mailbox)))
    (cl-json:encode-json-to-string `(("RESULT" . "OK")
                                     ("DATA"   . ,message)))))

(defun read-all-from-conn (id dumping)
  (let* ((client (gethash id *connections*))
         (mailbox (mailbox client)))
    (when dumping (setf (mailbox client) (list)))
    (cl-json:encode-json-to-string `(("RESULT" . "OK")
                                     ("DATA"   . ,mailbox)))))

(defun get-status (id)
  (let* ((client (gethash id *connections*)))
    (cl-json:encode-json-to-string `(("RESULT" . "OK")
                                     ("STATUS" . ,(string (ready-state (connection client))))))))

(defun write-to-conn (id data)
  (let* ((client (gethash id *connections*))
         (connection (connection client)))
    (trivia:match (ready-state connection)
                  (:open
                   (websocket-driver:send connection  (cl-json:encode-json-to-string  data))
                   (cl-json:encode-json-to-string `(("RESULT" . "OK")))))))

(defun handle-close-connection (connection)
  (setf (connection (find-client-by-conn connection)) NIL))




;;;WS SERVER
(defun run-ws-server (env)
  (let ((ws (websocket-driver:make-server env)))
    (websocket-driver:on :open ws
                         (lambda () (handle-new-connection ws)))
    (websocket-driver:on :message ws
                         (lambda (msg) 
                           (format T "~a FIRED MESSAGE ~%" ws)
                           (push-to-mailbox (find-client-by-conn ws) msg)
                           (format T "mailbox content:  ~a ~%" (mailbox (find-client-by-conn ws)))))
    (websocket-driver:on :error ws
                         (lambda (reason)
                           (format T "~a FIRED ERROR ~%" ws)
                           (format T "this nigga eating beans ~a ~%" reason)))
    (websocket-driver:on :close ws
                         (lambda (&key code reason)
                           (declare (ignore code reason))
                           (format T "~a FIRED CLOSE ~%" ws)
                           (handle-close-connection ws)))
    (lambda (responder)
      (declare (ignore responder))
      (websocket-driver:start-connection ws))))

;;;HTTP SERVER
(defun run-http-server (env)
  (match-with-request env
   (((:cmd . "STATUS")
     (:id  . id))
    (with-generic-error-handler (get-status id)))
   (((:cmd . "READ")
     (:id . id))
    (with-generic-error-handler (read-one-from-conn id T)))
   (((:cmd . "READ-ALL")
     (:id . id))
    (with-generic-error-handler (read-all-from-conn id T)))
   (((:cmd . "WRITE")
     (:id . id)
     (:data . data))
    (with-generic-error-handler (write-to-conn id data)))))

(defun main ()
  (defvar *ws-handler*   (clack:clackup #'run-ws-server   :port 8086))
  (defvar *http-handler* (clack:clackup #'run-http-server :port 8073))
  (format T "launching this motherfuker ~%")
  (force-output)
  (uiop:format! t "slynking ma nigga")
  (bt:make-thread (lambda ()
                    (slynk:create-server :port 4006)
                    :name "slynk")))
