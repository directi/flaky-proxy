(cl:defpackage #:flaky-proxy
  (:use #:cl))

(cl:in-package #:flaky-proxy)

(defvar *remove-request-headers*
  (list :connection :proxy-connection :content-length :user-agent :host :accept))

(defvar *remove-response-headers*
  (list :connection :proxy-connection :content-length :transfer-encoding))

(defun filter-headers (filter-list headers)
  (remove-if (lambda (header) (member header filter-list))
             headers
             :key #'car))

(defvar *id-lock* (bt:make-lock "id"))
(defvar *id* 0)

(defclass flaky-acceptor (hunchentoot:acceptor)
  ())

(defmethod hunchentoot:process-connection ((acceptor flaky-acceptor) socket)
  (bt:with-lock-held (*id-lock*)
    (incf *id*))
  (let ((*id* *id*))
    (log-message "CONN start ~A" socket)
    (unless (eql :close (call-hook *conn-received-hook* socket acceptor))
      (call-next-method))
    (log-message "CONN close ~A" socket)))

(defun proxy-handler ()
  (let* ((request-method (hunchentoot:request-method*))
         (uri (hunchentoot:request-uri*))
         (request-headers (hunchentoot:headers-in*))
         (content-length (hunchentoot:header-in* :content-length))
         (post-data (when (and content-length (plusp (parse-integer content-length)))
                      (hunchentoot:raw-post-data :force-binary t))))
    (log-message "REQ recd: ~A ~A" request-method uri)
    (call-hook *request-received-hook*)
    (setf request-headers (filter-headers *remove-request-headers* request-headers))
    (multiple-value-bind (body return-code response-headers)
        (drakma:http-request uri
                             :method request-method
                             :additional-headers request-headers
                             :content post-data
                             :redirect nil
                             :force-binary t
                             :user-agent (hunchentoot:user-agent)
                             :accept (hunchentoot:header-in* :accept))
      (log-message "RES recd: ~A" return-code)
      (call-hook *response-received-hook* body return-code response-headers)
      (setf response-headers (filter-headers *remove-response-headers* response-headers))
      (loop for (header . value) in response-headers
         do (setf (hunchentoot:header-out header) value))
      (when body
        (setf (hunchentoot:content-length*) (length body)))
      (setf (hunchentoot:return-code*) return-code)
      body)))

;;; Hooks
(defvar *conn-received-hook* nil)
(defvar *request-received-hook* nil)
(defvar *response-received-hook* nil)

(defun call-hook (hook &rest args)
  (when hook
    (apply hook args)))

;;; Hook handlers

(defun clear-hooks ()
  (setf *conn-received-hook* nil
        *request-received-hook* nil
        *response-received-hook* nil))

(defmacro with-count ((place count) (&rest args) &body body)
  ;; FIXME: lock before checking/modifying count
  (let ((count-var (gensym "COUNT-"))
        (count-lock-var (gensym "COUNT-LOCK-"))
        (new-count-var (gensym "NEW-COUNT-")))
    `(let ((,count-var ,count)
           (,count-lock-var (bt:make-lock "count-lock")))
       (setf ,place (lambda (,@args)
                      (let ((,new-count-var (bt:with-lock-held (,count-lock-var) ,count-var)))
                        (when (or (null ,new-count-var) (plusp ,new-count-var))
                          (when (integerp ,new-count-var)
                            (bt:with-lock-held (,count-lock-var)
                              (decf ,count-var)
                              (setf ,new-count-var ,count-var))
                            (when (zerop ,new-count-var)
                              (setf ,place nil)))
                          ,@body)))))))

(defun eat-connection (&key count)
  (clear-hooks)
  (with-count (*conn-received-hook* count)
      (socket acceptor)
    (log-hook-message "Eating connection")
    (close (hunchentoot::make-socket-stream socket acceptor))
    :close))

(defun eat-request (&key count)
  (clear-hooks)
  (with-count (*request-received-hook* count)
      ()
    (log-hook-message "Eating request")
    (hunchentoot:abort-request-handler)))

(defun eat-response (&key count)
  (clear-hooks)
  (with-count (*response-received-hook* count)
      (&rest args)
    (log-hook-message "Eating response")
    (hunchentoot:abort-request-handler)))

(defun hold-request (time &key count)
  (clear-hooks)
  (with-count (*request-received-hook* count)
      ()
    (log-hook-message "Holding request for ~A seconds" time)
    (sleep time)))

(defun hold-response (time &key count)
  (clear-hooks)
  (with-count (*response-received-hook* count)
      (&rest args)
    (log-hook-message "Holding response for ~A seconds" time)
    (sleep time)))


;;; Logging

(defvar *log-file* nil)
(defvar *log-file-stream* nil)

(defun iso-time (&optional (time (get-universal-time)))
  "Returns the universal time TIME as a string in full ISO format."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))

(defun log-message (fmt &rest args)
  (format *log-file-stream* "~&[~A] #~A ~A~%"
          (iso-time)
          *id*
          (apply #'format nil fmt args))
  (force-output *log-file-stream*))

(defun log-hook-message (fmt &rest args)
  (format *log-file-stream* "~&[~A] [HOOK] ~A~%" (iso-time) (apply #'format nil fmt args))
  (force-output *log-file-stream*))

;;; Setup

(defun proxy-dispatcher (request)
  (when (cl-ppcre:scan "^http" (hunchentoot:request-uri request))
    'proxy-handler))

(defun setup-hunchentoot ()
  (push 'proxy-dispatcher hunchentoot:*dispatch-table*)
  (setf hunchentoot:*handle-http-errors-p* nil
        hunchentoot:*access-log-pathname* #p"/tmp/hunchentoot-access.log"
        hunchentoot:*message-log-pathname* #p"/tmp/hunchentoot-error.log"
        hunchentoot:*log-lisp-errors-p* t
        hunchentoot:*log-lisp-backtraces-p* t))

(defvar *server* nil)

(defun init (&key (port 3200) (log-file *log-file*))
  (setup-hunchentoot)
  (setf *log-file* #p"/tmp/flaky-proxy.log")
  (setf *server* (hunchentoot:start (make-instance 'flaky-acceptor :port port)))
  (setf *log-file-stream* (open log-file :direction :output :if-exists :append :if-does-not-exist :create)))
