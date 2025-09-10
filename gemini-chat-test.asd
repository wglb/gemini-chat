;; gemini-chat-test.asd
(asdf:defsystem #:gemini-chat-test
  :description "Test suite for gemini-chat."
  :author "wgl@ciex-security.com"
  :license "MIT"
  :version "1.0.0"
  :depends-on (#:fiveam
               #:uiop
               #:file-packer
               #:gemini-chat
               #:jsown)
  :serial t
  :components ((:module "test"
                        :components ((:file "gemini-chat-test-pkg")
                                     (:file "gemini-chat-test" :depends-on ("gemini-chat-test-pkg"))
                                     (:file "api-tests" :depends-on ("gemini-chat-test-pkg")))))
  :perform (asdf:test-op (op c)
                         (symbol-call :gemini-chat-test :run-tests)))
