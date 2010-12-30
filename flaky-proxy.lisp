(cl:defpackage #:flaky-proxy
  (:use #:cl))

(cl:in-package #:flaky-proxy)

(defun proxy-dispatcher (request)
  (when (cl-ppcre:scan "^http" (hunchentoot:request-uri request))
    'proxy-handler))

(push 'proxy-dispatcher hunchentoot:*dispatch-table*)

(setf hunchentoot:*handle-http-errors-p* nil
      hunchentoot:*access-log-pathname* #p"/tmp/hunchentoot-access.log"
      hunchentoot:*message-log-pathname* #p"/tmp/hunchentoot-error.log"
      hunchentoot:*log-lisp-errors-p* t
      hunchentoot:*log-lisp-backtraces-p* t)

(defvar *remove-input-headers*
  (list :connection :proxy-connection :content-length))

(defvar *remove-output-headers*
  (list :connection :proxy-connection :content-length))

(defun filter-headers (filter-list headers)
  (remove-if (lambda (header) (member header filter-list))
             headers
             :key #'car))

(defclass flaky-acceptor (hunchentoot:acceptor)
  ())

(defmethod hunchentoot:process-connection ((acceptor flaky-acceptor) socket)
  (bt:with-lock-held (*id-lock*)
    (incf *id*))
  (let ((*id* *id*))
    (log-message "CONN start")
    (call-hook *conn-received-hook* socket)
    (call-next-method)
    (log-message "CONN close")))

(defun proxy-handler ()
  (let* ((request-method (hunchentoot:request-method*))
         (uri (hunchentoot:request-uri*))
         (input-headers (hunchentoot:headers-in*))
         (content-length (cdr (assoc :content-length input-headers)))
         (post-data (when (and content-length (plusp (parse-integer content-length)))
                      (hunchentoot:raw-post-data :force-binary t))))
    (log-message "REQ recd: ~A ~A" request-method uri)
    (call-hook *request-received-hook*)
    (setf input-headers (filter-headers *remove-output-headers* input-headers))
    (multiple-value-bind (body return-code output-headers)
        (drakma:http-request uri
                             :method request-method
                             :additional-headers input-headers
                             :content post-data
                             :redirect nil
                             :force-binary t)
      (log-message "RES recd: ~A" return-code)
      (call-hook *response-received-hook* body return-code output-headers)
      (setf output-headers (filter-headers *remove-output-headers* output-headers))
      (loop for (header . value) in output-headers
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
  (let ((count-var (gensym)))
    `(let ((,count-var ,count))
       (setf ,place (lambda (,@args)
                      (when (or (null ,count-var) (plusp ,count-var))
                        (when (integerp ,count-var)
                          (decf ,count-var)
                          (when (zerop ,count-var)
                            (setf ,place nil)))
                        ,@body))))))

(defun eat-request (&key count)
  (with-count (*request-received-hook* count)
      ()
    (hunchentoot:abort-request-handler)))

(defun eat-response (&key count)
  (with-count (*response-received-hook* count)
      (&rest args)
    (hunchentoot:abort-request-handler)))

;;; Logging

(defvar *log-file* #p"/tmp/flaky-proxy.log")
(defvar *log-file-stream* nil)

(defvar *id-lock* (bt:make-lock "id"))
(defvar *id* 0)

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

(defun init ()
  (setf *log-file-stream* (open *log-file* :direction :output :if-exists :append :if-does-not-exist :create)))
