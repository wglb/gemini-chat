;;;; gemini-chat.asd
;;;; Copyright (c) 2026 Ciex-Security <wgl@ciex-security.com>
;;;; License: MIT

(asdf:defsystem #:gemini-chat
  :description "Lisp-based gemini chat"
  :author "wgl@ciex-security.com"
  :license :mit ; Switched to match your preference, or keep "GPLv3" if preferred
  :version "1.4.20"
  :serial t
  :depends-on (#:gemini-chat-lib
               #:xlog
               #:flag-help
               #:com.google.flag
               #:uiop)		 ; Added uiop for robust path/arg handling
  ;; Removed :defsystem-depends-on ("deploy") to avoid "warm boot" errors
  :components ((:file "gemini-chat-pkg")
               (:file "gemini-chat"))
  :entry-point "gemini-chat:top"
  ;; Use the native ASDF/SBCL program dumper
  :build-operation "program-op"
  :build-pathname "gemini-chat")
