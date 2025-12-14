;;;; gemini-chat-pkg.lisp
;;;; This file defines the GEMINI-CHAT package.
;;;; gemini-chat-pkg.lisp
;;;; This file defines the GEMINI-CHAT package.

(defpackage #:gemini-chat
  (:use #:cl
        #:jsown
        #:cl-ppcre
        #:split-sequence
		#:file-packer-lib
		#:cljwt        ; JWT signing (Requires Ironclad)
        #:ironclad     ; Cryptography library (for cljwt)
        #:cl-base64
		#:jose
		#:cl-ssh-keys
        #:xlg-lib
        #:com.google.flag
        #:uiop/driver
		#:gemini-chat-lib)
  (:import-from #:drakma 
    #:http-request 
    #:drakma-error 
    #:url-encode 
    #:parameter-error)
  (:import-from #:jsown #:to-json #:parse)
  (:import-from #:uiop #:getenv #:slurp-stream-string #:read-file-forms)
  (:export #:run-gemini-conversation
           #:slime-chat
           #:top
           #:api-req))


#+nil (defpackage #:gemini-chat
  (:use #:cl
		#:drakma
        #:jsown
        #:cl-ppcre
        #:split-sequence
		#:file-packer-lib
		#:cljwt        ; JWT signing (Requires Ironclad)
        #:ironclad     ; Cryptography library (for cljwt)
        #:cl-base64
		#:jose
		#:cl-ssh-keys
        #:xlg-lib
        #:com.google.flag
        #:uiop/driver
        #:split-sequence
		#:file-packer-lib
        #:xlg-lib
		#:gemini-chat-lib)
  (:import-from #:drakma #:http-request #:drakma-error)
  (:import-from #:jsown #:to-json #:parse)
  (:import-from #:uiop #:getenv #:slurp-stream-string #:read-file-forms)
  (:export #:run-gemini-conversation
           #:slime-chat
           #:top
           #:api-req))
