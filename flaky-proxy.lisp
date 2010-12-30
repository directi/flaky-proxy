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

(defun proxy-handler ()
  (let* ((request-method (hunchentoot:request-method*))
         (uri (hunchentoot:request-uri*))
         (input-headers (hunchentoot:headers-in*))
         (content-length (cdr (assoc :content-length input-headers)))
         (post-data (when (and content-length (plusp (parse-integer content-length)))
                      (hunchentoot:raw-post-data :force-binary t))))
    (setf input-headers (filter-headers *remove-output-headers* input-headers))
    (multiple-value-bind (body return-code output-headers)
        (drakma:http-request uri
                             :method request-method
                             :additional-headers input-headers
                             :content post-data
                             :redirect nil
                             :force-binary t)
      (setf output-headers (filter-headers *remove-output-headers* output-headers))
      (loop for (header . value) in output-headers
         do (setf (hunchentoot:header-out header) value))
      (when body
        (setf (hunchentoot:content-length*) (length body)))
      (setf (hunchentoot:return-code*) return-code)
      body)))
