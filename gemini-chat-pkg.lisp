;;;; gemini-chat-pkg.lisp
;;;; This file defines the GEMINI-CHAT package.

(defpackage #:gemini-chat
  (:use #:cl
        #:com.google.flag
        #:uiop/driver
        #:split-sequence
        #:xlg-lib)
  (:import-from #:drakma #:http-request #:drakma-error)
  (:import-from #:jsown #:to-json #:parse)
  (:import-from #:uiop #:getenv #:slurp-stream-string #:read-file-forms)
  (:export #:run-gemini-conversation
           #:slime-chat
           #:top
           #:run-chat
           #:api-req))
