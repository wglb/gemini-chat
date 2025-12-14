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
			   #:cljwt        ; JWT signing (Requires Ironclad)
               #:ironclad     ; Cryptography library (for cljwt)
               #:cl-base64
			   #:jose
			   #:cl-ssh-keys
               #:xlg-lib)
  :components ((:file "gemini-chat-lib-pkg")
               (:file "gemini-chat-lib")
			   (:file "gemini-client")))
