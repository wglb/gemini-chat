;; test/gemini-chat-test-pkg.lisp
(defpackage #:gemini-chat-test
  (:use #:cl #:fiveam #:uiop #:com.google.flag #:jsown)
  (:import-from #:file-packer-lib #:pack-files-to-stream #:unpack-files-from-stream)
  (:import-from #:gemini-chat #:api-req #:make-api-request-payload)
  (:export #:run-tests #:gemini-chat-suite #:api-suite))
