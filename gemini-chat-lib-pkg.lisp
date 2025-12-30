;;;; gemini-chat-pkg.lisp
;;;; This file defines the GEMINI-CHAT package.

(defpackage #:gemini-chat-lib
  (:use #:cl
        #:uiop/driver
        #:split-sequence
		#:file-packer-lib
        #:xlg-lib)
  (:import-from #:drakma #:http-request #:drakma-error)
  (:import-from #:jsown #:to-json #:parse)
  (:import-from #:uiop #:getenv #:slurp-stream-string #:read-file-forms)
  (:export #:run-chat-with-kw
		   #:gemini-chat-lib-init
		   #:get-key
		   #:upload-file-to-gemini
		   #:delete-gemini-file))
