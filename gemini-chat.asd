;;;; gemini-chat.asd
;;;; Copyright (c) 2025-2026 Ciex-Security <wgl@ciex-security.com>
;;;; License: MIT

(asdf:defsystem #:gemini-chat
  :description "Lisp-based gemini chat"
  :author "wgl@ciex-security.com"
  :license  "GPLv3"
  :version "1.4.20"
  :serial t
  :depends-on (#:gemini-chat-lib
               #:xlog
			   #:flag-help
               #:com.google.flag)
  :defsystem-depends-on ("deploy")
  :components ((:file "gemini-chat-pkg")
               (:file "gemini-chat"))
  :entry-point "gemini-chat:top"
  :build-operation "deploy:deploy-op"
  :build-pathname "gemini-chat-yo")
