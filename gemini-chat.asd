(asdf:defsystem #:gemini-chat
  :description "Lisp-based gemini chat"
  :author "wgl@ciex-security.com"
  :license  "GPLv3"
  :version "1.4.16"
  :serial t
  :depends-on (#:drakma
               #:jsown
               #:cl-ppcre
               #:split-sequence
               #:uiop
			   #:file-packer-lib
               #:xlg-lib
               #:com.google.flag)
  :defsystem-depends-on ("deploy")
  :components ((:file "gemini-chat-pkg")
               (:file "gemini-chat"))
  :entry-point "gemini-chat:top"
  :build-operation "deploy:deploy-op"
  :build-pathname "gemini-chat-yo")
