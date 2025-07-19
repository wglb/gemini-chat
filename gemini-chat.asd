;;;; gemini-chat.asd

(asdf:defsystem #:gemini-chat
  :description "Lisp-based gemini chat"
  :author "wgl@ciex-security.com"
  :license  "GPLv3"
  :version "1.3.5"
  :serial t
  :depends-on (#:drakma #:jsown #:uiop #:xlg-lib)
  :components ((:file "gemini-chat-pkg")
               (:file "gemini-chat")))
