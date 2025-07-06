;;;; gemini-chat-pkg.lisp
;;;; This file defines the GEMINI-CHAT package.

(defpackage #:gemini-chat
  (:use #:cl) ; Only use CL (Common Lisp standard functions) by default for maximum control.
  (:import-from #:drakma #:http-request #:drakma-error) ; Explicitly import what you need from Drakma
  (:import-from #:cl-json #:decode-json #:encode-json-to-string) ; Explicitly import what you need from CL-JSON
  (:import-from #:uiop #:getenv) ; Explicitly import getenv from UIOP
  (:export #:run-gemini-conversation
           #:make-gemini-api-request))
