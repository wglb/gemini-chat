;;;; gemini-chat-pkg.lisp
;;;; This file defines the GEMINI-CHAT package.

(defpackage #:gemini-chat
  (:use #:cl #:xlg-lib #:com.google.flag #+nil #:uiop  #:uiop/driver)
  (:import-from #:drakma #:http-request #:drakma-error) 
  (:import-from #:jsown #:to-json #:parse) 
  (:import-from #:uiop #:getenv #:slurp-stream-string #:read-file-forms)
  (:export #:run-gemini-conversation
           #:make-gemini-api-request
           #:run-chat))
