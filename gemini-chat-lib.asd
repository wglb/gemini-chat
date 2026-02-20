;;;; gemini-chat-lib.asd
;;;; Copyright (c) 2025-2026 Ciex-Security <wgl@ciex-security.com>
;;;; License: MIT

(asdf:defsystem #:gemini-chat-lib
  :description "Lisp-based gemini chat"
  :author "wgl@ciex-security.com"
  :license  "GPLv3"
  :version "1.6.5"
  :serial t
  :depends-on (#:drakma
               #:jsown
               #:cl-ppcre
               #:split-sequence
               #:uiop
			   #:file-packer-lib
               #:cl-base64
               #:xlog)
  :components ((:file "gemini-chat-lib-pkg")
               (:file "gemini-chat-lib")
			   (:file "gemini-client")
			   (:file "gemini-chat-agent")))
