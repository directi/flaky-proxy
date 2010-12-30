(asdf:defsystem #:flaky-proxy
  :depends-on (:hunchentoot :drakma :bordeaux-threads)
  :serial t
  :components ((:file "flaky-proxy")))