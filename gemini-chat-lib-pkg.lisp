;;;; gemini-chat-pkg.lisp
;;;; Copyright (c) 2025-2026 Ciex-Security <wgl@ciex-security.com>
;;;; License: MIT

(defpackage #:gemini-chat-lib
  (:use #:cl
        #:uiop/driver
        #:split-sequence
		#:file-packer-lib
        #:xlog)
  (:import-from #:drakma #:http-request #:drakma-error)
  (:import-from #:jsown #:to-json #:parse)
  (:import-from #:uiop #:getenv #:slurp-stream-string #:read-file-forms)
  (:export #:run-chat-with-kw
		   #:gemini-chat-lib-init
		   #:get-key
		   #:gem-conv
		   #:do-api-request
		   #:upload-file-to-gemini
		   #:create-gemini-batch-job
		   #:create-vertex-batch-job
		   #:check-batch-job-status
		   #:upload-to-gcs
		   #:estimate-batch-cost
		   #:monitor-security-job
		   #:delete-gemini-file
		   #:load-job-metadata
		   #:save-job-metadata
		   #:set-static-api-key
		   #:monitor-batch-job
		   #:inspect-job-stalling
		   #:check-quota-status
		   #:check-batch-job-status
		   #:*gemini-service-account*
		   #:agentic-iterative-build
		   #:proc-ctx-files))
