;;;; zender-sockets.lisp

(in-package #:zender-sockets)

;;;CONNECTION DATABASE
(defvar *connections* (make-hash-table :test 'equal))


;;;UTILS
(defun get-auth-data (data)
  (format T "data: ~a ~%" data)
  (with-input-from-string
                       (s (dexador:post "http://localhost:8087/validate"
                                        :headers '(("content-type" . "application/json"))
                                        :content (cl-json:encode-json-to-string `(("secret" . ,data)))))
                     (json:decode-json s)))


(defmacro with-generic-error-handler (exp)
  `(handler-case 
       ,exp
     (t (c) 
       (format T "~a ~%" c)
       (cl-json:encode-json-to-string `(("RESULT" . "ERR")
                                        ("STATUS" . "FUCKUP"))))))


(defun body-to-string (stream)
  (if (listen stream)
      (alexandria:read-stream-content-into-string stream)
      ""))


(defun decode-json-from-string-wrapped (string)
  (ignore-errors
    (json:decode-json-from-string string)))

;;;CLIENT
(defclass client ()
  ((connection :initarg :connection
               :accessor connection)
   (mailbox :initform (list)
            :accessor mailbox)))

(defgeneric push-to-mailbox (obj message)
  (:documentation "puts message atop of message mailbox")
  (:method (obj message)
    (declare (ignorable obj))
    (format t "data was lost: ~a ~%" message)))

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
    (format T "auth-data: ~a ~%" auth-data)
    (trivia:match auth-data
                  ((alist (:result . "OK")
                          (:id . id)
                          (:valid . T))
                   (trivia:match (gethash id *connections*)
                                 (NIL (setf (gethash id *connections*) 
                                            (make-instance 'client :connection con)))
                                 (client (setf (connection client)
                                               con)))
                  (_  NIL)))))

;; for debuging and testing purposes
; (defun handle-test-connection (con)
;   (trivia:match 
;    (gethash "id" (websocket-driver.ws.server::headers con))
;    (id
;     (trivia:match (gethash id *connections*)
;                   (NIL (setf (gethash id *connections*) 
;                              (make-instance 'client :connection con)))
;                   (client (setf (connection client)
;                                 con))))))

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
                                     ("STATUS" . ,(ready-state (connection client)))))))


(defun write-to-conn (id data)
  (let* ((client (gethash id *connections*))
         (connection (connection client)))
    (trivia:match (ready-state connection)
                  (:open
                   (websocket-driver:send connection data)
                   (cl-json:encode-json-to-string `(("RESULT" . "OK")))))))

(defun handle-close-connection (connection)
  (setf (connection (find-client-by-conn connection)) NIL))


;;;WS SERVER
(defun run-ws-server (env)
  (let ((ws (websocket-driver:make-server env)))
    (websocket-driver:on :open ws
                          (lambda () (handle-new-connection ws))
                         ;(lambda () 
                         ;  (format T "~a FIRED OPEN ~%" ws)
                         ;  (handle-test-connection ws))
                         )
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
  (trivia:match env
                ((plist :request-method request-method
                        :raw-body       raw-body)
                 (let ((data (decode-json-from-string-wrapped (body-to-string raw-body))))
                   (trivia:match data
                                 ((alist (:cmd . "STATUS")
                                         (:id . id))
                                  `(200 (:content-type "application/json") (,(with-generic-error-handler 
                                                                                 (get-status id)))))
                                 ((alist (:cmd . "READ")
                                         (:id . id))
                                  `(200 (:content-type "application/json") (,(with-generic-error-handler 
                                                                                 (read-one-from-conn id T)))))
                                 ((alist (:cmd . "READ-ALL")
                                         (:id . id))
                                  `(200 (:content-type "application/json") (,(with-generic-error-handler 
                                                                                 (read-all-from-conn id T)))))
                                 ((alist (:cmd . "WRITE")
                                         (:id . id)
                                         (:data . data))
                                  `(200 (:content-type "application/json") (,(with-generic-error-handler
                                                                               (write-to-conn id data)))))
                                 (_ 
                                  `(500 (:content-type "application/json") ("invalid request kiddo"))))))
                (_ '(200 (:content-type "application/json") ("fuko")))))


(defun main ()
  (defvar *ws-handler*   (clack:clackup #'run-ws-server   :port 8086))
  (defvar *http-handler* (clack:clackup #'run-http-server :port 8073))
  ;; let the webserver run.
  ;; warning: hardcoded "hunchentoot".
  (handler-case (bt:join-thread (find-if (lambda (th)
                                            (search "hunchentoot" (bt:thread-name th)))
                                         (bt:all-threads)))
    ;; Catch a user's C-c
    (#+sbcl sb-sys:interactive-interrupt
      #+ccl  ccl:interrupt-signal-condition
      #+clisp system::simple-interrupt-condition
      #+ecl ext:interactive-interrupt
      #+allegro excl:interrupt-signal
      () (progn
           (format *error-output* "Aborting.~&")
           (clack:stop *server*)
           (uiop:quit)))
    (error (c) (format t "Woops, an unknown error occured:~&~a~&" c))))


