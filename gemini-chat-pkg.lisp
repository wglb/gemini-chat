;;;; gemini-chat-pkg.lisp
;;;; This file defines the GEMINI-CHAT package.

(defpackage #:gemini-chat
  (:use #:cl #:xlg)
  (:import-from #:drakma #:http-request #:drakma-error) 
  (:import-from #:jsown #:to-json #:parse) 
  (:import-from #:uiop #:getenv #:slurp-stream-string)
  (:export #:run-gemini-conversation
           #:make-gemini-api-request))
