(defpackage #:gemini-chat
  (:use #:cl
        #:jsown
        #:cl-ppcre
        #:split-sequence
		#:file-packer-lib
        #:xlg-lib
        #:com.google.flag
        #:uiop/driver
		#:gemini-chat-lib)
  (:import-from #:drakma 
    #:http-request 
    #:drakma-error 
    #:url-encode) ; <--- Removed #:parameter-error, which was causing the conflict
  (:import-from #:jsown #:to-json #:parse)
  (:import-from #:uiop #:getenv #:slurp-stream-string #:read-file-forms)
  (:export #:run-gemini-conversation
           #:slime-chat
           #:top
           #:api-req))
