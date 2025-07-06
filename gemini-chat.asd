;;;; gemini-chat.asd

(asdf:defsystem #:gemini-chat
  :description "Describe gemini-chat here"
  :author "Your Name <your.name@example.com>"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:drakma #:cl-json #:uiop)
  :components ((:file "gemini-chat-pkg.lisp")
               (:file "gemini-chat")))
