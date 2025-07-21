;; gemini-chat-pkg.asd
(asdf:defsystem #:gemini-chat
  :description "Lisp-based gemini chat"
  :author "wgl@ciex-security.com"
  :license  "GPLv3"
  :version "1.3.14"
  :serial t
  :depends-on (#:drakma
               #:jsown
               #:cl-ppcre
               #:split-sequence
               #:uiop
               #:xlg-lib
               #:com.google.flag)
  :components ((:file "gemini-chat-pkg")
               (:file "gemini-chat")))
