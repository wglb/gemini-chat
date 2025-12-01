(asdf:defsystem #:gemini-chat-lib
  :description "Lisp-based gemini chat"
  :author "wgl@ciex-security.com"
  :license  "GPLv3"
  :version "1.6.3"
  :serial t
  :depends-on (#:drakma
               #:jsown
               #:cl-ppcre
               #:split-sequence
               #:uiop
			   #:file-packer-lib
               #:xlg-lib)
  :components ((:file "gemini-chat-lib-pkg")
               (:file "gemini-chat-lib")))
