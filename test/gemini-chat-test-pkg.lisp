;; test/gemini-chat-test-pkg.lisp
(defpackage #:gemini-chat-test
  (:use #:cl #:fiveam #:uiop)
  (:import-from #:file-packer #:pack-files-to-stream #:unpack-files-from-stream)
  (:import-from #:gemini-chat #:api-req #:make-api-request-payload #:top #:run-chat)
  (:export #:run-tests #:gemini-chat-suite))

