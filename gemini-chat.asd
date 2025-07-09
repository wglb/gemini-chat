;;;; gemini-chat.asd

(asdf:defsystem #:gemini-chat
  :description "Lisp-based gemini chat"
  :author "wgl@ciex-security.com"
  :license  "GPLv3"
  :version "0.0.3"
  :serial t
  :depends-on (#:drakma #:jsown #:uiop)
  :components ((:file "gemini-chat-pkg")
               (:file "gemini-chat")))
