;;; gemini-client.lisp
;;; A secure, open-source Common Lisp client for the Gemini API using IAM Service Accounts and Static Keys.
;;;
;;; Author: wgl@ciex-security.com (Based on user preference)
;;; Copyright: 2025 (Based on user preference)

(in-package #:gemini-chat-lib)

(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3)))

;; --- Constants and Variables ---

(defparameter *gemini-endpoint* "https://generativelanguage.googleapis.com/v1beta")

(defparameter *user-agent* "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36")

(defparameter *cookie-jar* (make-instance 'drakma:cookie-jar)) 

;; New: Stores a static API key (for simpler, but less secure, authentication)
(defparameter *static-api-key* nil
  "Stores a static Gemini API Key for direct authentication, bypassing the IAM flow.")

;; Use NIL instead of hardcoded strings to ensure the user provides them via init
(defvar *gemini-service-account* nil
  "Populated via gemini-chat-lib-init or GEMINI_SERVICE_ACCOUNT environment variable.")

(defvar *gemini-service-account-scopes* "https://www.googleapis.com/auth/generative-language")

(defvar *gemini-lib-init-p* nil
  "Flag to ensure the library has been initialized with account details.")

(defvar *use-vertex-auth* nil "Dynamic flag to toggle between API Key and Bearer Token.") ;; TODO a parameter

(defun gemini-chat-lib-init (&key static-key service-account (tag "gemini-run"))
  "Initializes the library with cloud-specific credentials and starts the logging system."
  (setf *tag* tag)
  (setf *static-api-key* (or static-key (uiop:getenv "GEMINI_API_KEY")))
  (setf *gemini-service-account* (or service-account (uiop:getenv "GEMINI_SERVICE_ACCOUNT")))
  (setf *gemini-lib-init-p* t)
  #+nil (xlogntf "gcli: Gemini-chat-lib initialized for account ~a" (or *gemini-service-account* "Static-Only"))
  t)

(defun ensure-gemini-init ()
  "Ensures the library is initialized before proceeding."
  (unless *gemini-lib-init-p*
    (error "Gemini-chat-lib has not been initialized. Call (gemini-chat-lib-init) first.")))

(defvar *active-gemini-token* nil "Storage for the current OAuth token.") ;; (setf *active-gemini-token* nil)
(defvar *token-expiry-time* 0 "Universal time when the token expires.")

(defun get-fresh-gemini-token ()
  "Fetches a fresh token using the active gcloud user (no impersonation)."
  (handler-case
      (uiop:run-program 
       (list "gcloud" "auth" "print-access-token"
             "--scopes=https://www.googleapis.com/auth/cloud-platform")
       :output '(:string :stripped t)
       :error-output :string)
    (error (c) (xlogft  "Gcloud token fetch failed: ~a" c) nil)))

;; --- Constants and Variables ---

(defparameter *project-id* nil
  "The Google Cloud Project ID for Vertex AI operations.")

;; --- Setters ---

(defun set-project-id (id)
  "Sets the project ID for the library."
  (setf *project-id* id))

;; --- Helpers ---

(defun get-project-id (&optional resource-string)
  "Returns the project ID. Extracts from a resource string if *project-id* is nil."
  (cond (*project-id* *project-id*)
        ((and resource-string (uiop:string-prefix-p "projects/" resource-string))
         ;; Extract the part between "projects/" and "/locations/"
         (let* ((start (length "projects/"))
                (end (search "/locations/" resource-string)))
           (subseq resource-string start end)))
        (t (error "Project ID not set and could not be inferred."))))

(defun set-static-api-key (key)
  (setf *static-api-key* key))

(defun get-cached-auth-token ()
  "Returns the cached token if valid, otherwise fetches a new one."
  (let ((now (get-universal-time)))
    ;; Branch 1: Token is missing or about to expire (within 60s buffer)

    (cond ((or (not *active-gemini-token*)
			   (< *token-expiry-time* (+ now 60)))
		   (xlognt "Token expired or missing. Fetching fresh token...")
		   (setf *active-gemini-token* (get-fresh-gemini-token))
		   ;; Set expiry for 55 minutes from now
		   (setf *token-expiry-time* (+ now (* 55 60)))
		   *active-gemini-token*)

		  ;; Branch 2: Token is valid
		  (t #+nil(xlognt "Token valid.")
			 *active-gemini-token*))))

(defun get-auth-info ()
  "Returns auth based on context. Vertex MUST use bearer tokens; others use static keys."
  (cond (*use-vertex-auth*
         ;; Keep this for your batch monitor and batch submission logic
         (list :type :bearer-token :value (get-cached-auth-token)))
        (*static-api-key*
         ;; Primary path for your gemini-chat interactive work
         (list :type :static-key :value *static-api-key*))
        (t
         (error "No authentication method configured. Set *static-api-key* or enable *use-vertex-auth*."))))

;; --- Core API Request Function ---


(defun do-api-request (uri-parts payload method)
  "Perform api call using either the secure Bearer token or a static API key..."
  (ensure-gemini-init)
  (let* ((auth-info (get-auth-info))
         (headers (acons "Accept" "application/json" nil))
         (retries 0)
         (max-retries 8)
         (uri (if (or (uiop:string-prefix-p "http://" uri-parts)
                      (uiop:string-prefix-p "https://" uri-parts))
                  uri-parts
                  (concatenate 'string *gemini-endpoint* "/" uri-parts))))
    (cond
      ((eq (getf auth-info :type) :static-key)
       (setf headers (acons "x-goog-api-key" (getf auth-info :value) headers)))
      
      ((eq (getf auth-info :type) :bearer-token)
       (let ((token (getf auth-info :value)))
         (setf headers (acons "Authorization" (concatenate 'string "Bearer " token) headers))))
      
      (t
       (xlognt "No authentication information available for API request.")))
    (loop
      (multiple-value-bind (bbody status-code headers uri-back http-stream must-close status-text)
          (drakma:http-request uri
                               :method method
                               :cookie-jar *cookie-jar*
                               :user-agent *user-agent*
                               :content payload
                               :content-type "application/json"
                               :additional-headers headers)
        
        (declare (ignorable uri-back http-stream must-close status-text))

        (let ((body (cond ((stringp bbody) bbody)
                          (t (map 'string #'code-char bbody))))
              (our-json nil)
              (content-type (cdr (assoc :content-type headers))))
          #+nil (xlogf  "Status: ~a (Attempt ~a)" status-code (1+ retries) )
          (cond
            ;; 429: Too Many Requests - Trigger Backoff/Retry
            ((= status-code 429)
             (when (>= retries max-retries)
               ;; FIX: Max retries reached. Return the response to allow the detailed
               ;; JSON error message to be parsed by the calling function.
               (xlogf "API Quota exceeded after ~a retries. Returning raw error response for parsing." max-retries)
               
               ;; Attempt to parse JSON before returning, just like success/other error cases
               (setf our-json (if (and content-type (search "application/json" content-type :test #'char-equal))
                                  (jsown:parse body)
                                  nil))
               ;; Exit the loop with the error response
               (return (list our-json body status-code headers uri-back http-stream must-close status-text)))

             (incf retries)
             (let ((wait-time (+ (expt 2 retries) (random 1.0))))
               (xlogf "Quota Exceeded (429). Retrying in ~a seconds (~a/~a)." (round wait-time) retries max-retries )
               (sleep wait-time)))

            ;; 200-399: Success or Redirects - Process result and exit loop
            ((< status-code 400)
             ;; Parse JSON if content type is application/json
             (if (and content-type (search "application/json" content-type :test #'char-equal))
                 (setf our-json (jsown:parse body)))

             ;; Return all relevant values and exit loop
             (return (list our-json body status-code headers uri-back http-stream must-close status-text)))

            ;; 400+: General Error - Return the error response for parsing
            (t
             ;; For all other non-2xx errors, return the response for detailed parsing
             (xlogf "API Request failed with status code ~a. Returning raw error response for parsing." status-code)
             ;; Parse JSON if content type is application/json
             (if (and content-type (search "application/json" content-type :test #'char-equal))
                 (setf our-json (jsown:parse body)))
             (return (list our-json body status-code headers uri-back http-stream must-close status-text)))))))))

(defun make-gemini-payload-alist (prompt &key file-ref-id file-mime-type)
  "Creates the Lisp alist structure for the Gemini API request body using jsown's format.
   If FILE-REF-ID is provided, it is included as a fileData part."
  (let ((text-part `(:obj ("text" . ,prompt)))
        (parts nil))
    
    ;; 1. Add file part first (optional)
    (when file-ref-id
      (push `(:obj ("fileData" . (:obj ("mimeType" . ,file-mime-type)
                                     ("fileUri" . ,(format nil "files/~a" file-ref-id)))))
            parts))
    
    ;; 2. Add the text prompt part
    (push text-part parts)
    
    `(:obj
      ("contents" . ,(list 
                      `(:obj 
                        ("parts" . ,(nreverse parts)))))))) ; NREVERSE to ensure file part is first

;;; gemini-client.lisp (Replace existing CALL-GEMINI-MODEL)

(defun call-gemini-model (model-name prompt &key file-ref-id file-mime-type)
  "Sends a request to the Gemini generateContent endpoint. FILE-REF-ID and 
   FILE-MIME-TYPE can be used to reference a previously uploaded file."
  (let* ((uri-parts (format nil "models/~a:generateContent" model-name))
         ;; Modify payload creation to pass file info
         (payload-alist (make-gemini-payload-alist prompt 
                                                  :file-ref-id file-ref-id 
                                                  :file-mime-type file-mime-type))
         (payload (jsown:to-json payload-alist))
         (result (do-api-request uri-parts payload :post))
         (json (first result)) ; The parsed JSON object (or NIL if parsing failed/no JSON)
         (body (second result)) ; The raw response body
         (status-code (third result))) ; The HTTP status code
    
    ;; Error handling: Check for API errors (400 or higher) first
    (if (>= status-code 400)
        (error "Gemini API Request Failed (Status ~a). Check your API Key or Service Account setup. Response body: ~%~a" 
               status-code 
               body)
        
        ;; Success (2xx status code)
        (if json
            (jsown:val json "candidates") ; Attempt to extract candidates
            (error "Gemini API Request Succeeded (Status ~a), but returned no parsable JSON body." status-code)))))
;;; gemini-client.lisp (Add this function)

(defun read-file-to-octets (path)
  "Manual binary read to avoid UIOP/Alexandria versioning issues."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      bytes)))

(defun extract-project-id-from-sa (sa-email)
  "Extracts 'project-id' from 'name@project-id.iam.gserviceaccount.com'."
  (let* ((domain (second (uiop:split-string sa-email :separator '(#\@))))
         (parts (uiop:split-string domain :separator '(#\.))))
    (first parts)))

(defun parse-gemini-upload-response (body status)
  (let* ((resp-str (if (stringp body) body (flexi-streams:octets-to-string body)))
         (json (jsown:parse resp-str)))
	(if (and (>= status 200) (< status 300))
        (jsown:val (if (jsown:keyp json "file") (jsown:val json "file") json) "name")
        (error "Gemini Upload Failed (~a): ~a" status resp-str))))

(defun upload-file-to-gemini (file-path mime-type)
  "Uploads a file and returns the blob-id using Drakma's native multipart handling."
  (ensure-gemini-init)
  (let* ((api-key *static-api-key*)
         ;; Use the simple upload endpoint for multipart
         (simple-url (format nil "https://generativelanguage.googleapis.com/upload/v1beta/files?key=~a" api-key))
         (display-name (format nil "up-~a" (get-universal-time)))
         (metadata (jsown:to-json 
                    `(:obj ("file" . (:obj ("displayName" . ,display-name)
                                           ("mimeType"    . ,mime-type)))))))
    
    (unless api-key (error "Upload requires *static-api-key*."))

    ;; We pass the metadata and file as a list of parameters.
    ;; Drakma will automatically generate the boundaries and correct CRLF endings.
    (multiple-value-bind (body status)
        (drakma:http-request simple-url
                             :method :post
							 ;; Use a standard list (no dot) so Drakma can read the keyword arguments
							 :parameters `(("metadata" . ,metadata)
										   ("file" ,(pathname file-path) :content-type ,mime-type))
                             
                             :preserve-uri t)
      ;; This should now return a 200/201 and the JSON containing the 'name' field
      (parse-gemini-upload-response body status))))

(defun list-gemini-files ()
  "Lists all files currently stored in the Gemini project using the Static API Key."
  (let* ((api-key (or (and (boundp '*static-api-key*) *static-api-key*)
                      (uiop:getenv "GEMINI_API_KEY")))
         (url (format nil "~a/files" *gemini-endpoint*) #+nil "https://generativelanguage.googleapis.com/v1beta/files")
         (auth-headers `(("x-goog-api-key" . ,(or api-key "")))))
    
    (unless (and api-key (not (string= api-key "")))
      (error "Listing files requires *static-api-key* or GEMINI_API_KEY environment variable."))

    (multiple-value-bind (body status)
        (drakma:http-request url 
                             :method :get 
                             :additional-headers auth-headers)
      (cond
        ((= status 200)
         (let ((parsed (jsown:parse (flexi-streams:octets-to-string body))))
           ;; The API returns an empty object if no files exist, so check for "files" key
           (if (jsown:keyp parsed "files")
               (jsown:val parsed "files")
               nil)))
        (t
         (error "Failed to list files (Status ~a): ~a" 
                status 
                (flexi-streams:octets-to-string body)))))))

(defun delete-gemini-file (file-id)
  "Deletes a file from Gemini using the Static API Key (matching the uploader)."
  (let* ((static-key *static-api-key*)
         (url (format nil "~a/~a" *gemini-endpoint* file-id))
         (auth-headers `(("x-goog-api-key" . ,(or static-key "")))))
    
    (unless static-key
      (error "Deletion requires GEMINI_API_KEY to match the upload credential."))

    (multiple-value-bind (body status)
        (drakma:http-request url 
                             :method :delete 
                             :additional-headers auth-headers)
      (cond
        ((= status 200) t)				; Success
        (t
         (let ((err-msg (if (stringp body) body (flexi-streams:octets-to-string body))))
           (error "Failed to delete Gemini file ~a (Status ~a): ~a" file-id status err-msg)))))))

(defun print-gemini-inventory ()
  "Prints a formatted list of all outstanding blobs with corrected key names."
  (let ((files (list-gemini-files)))
    (if (null files)
        (format t "No outstanding blobs found.~%")
        (progn
          (format t "~&~40A | ~25A | ~A~%" "FILE ID" "DISPLAY NAME" "CREATED")
          (format t "~V@{~A~:*~}~%" 85 "-")
          (dolist (f files)
            (format t "~40A | ~25A | ~A~%" 
                    (jsown:val f "name")
                    ;; Google uses camelCase here
                    (if (jsown:keyp f "displayName") (jsown:val f "displayName") "N/A")
                    (jsown:val f "createTime")))))))

(defun cleanup-all-gemini-blobs ()
  "Lists and deletes all files found in the project."
  (let ((files (list-gemini-files))
        (count 0))
    (dolist (file files)
      ;; "name" is lowercase in the JSON response
      (let ((name (jsown:val file "name")))
        (format t "Deleting ~a... " name)
        (delete-gemini-file name)
        (format t "done.~%")
        (incf count)))
    (format t "~&Cleanup complete. Deleted ~a files.~%" count)))

(defun salvage-failed-job (project-id display-name)
  "Downloads partial results from GCS for a specific PROJECT-ID and DISPLAY-NAME."
  (let* ((cmd (list "gcloud" "ai" "batch-prediction-jobs" "list" 
                    "--project" project-id "--format" "json"
                    "--filter" (format nil "displayName=~A" display-name)))
         (raw (uiop:run-program cmd :output :string))
         (job-list (jsown:parse raw)))
    (if (null job-list)
        (format t "~&No job found for project ~A with name ~A" project-id display-name)
        (let* ((job-data (car job-list))
               (output-config (jsown:val job-data "outputConfig"))
               (gcs-uri (jsown:val output-config "outputUriPrefix"))
               (local-path (merge-pathnames (format nil "salvage/~A/" display-name) 
                                            (user-homedir-pathname))))
          (ensure-directories-exist local-path)
          (format t "~&Salvaging partials from ~A to ~A..." gcs-uri local-path)
          ;; Using -m for parallel copy to handle multiple small jsonl parts
          (uiop:run-program (list "gsutil" "-m" "cp" "-r" (format nil "~A*" gcs-uri) 
                                  (namestring local-path))
                            :ignore-error-status t)
          local-path))))

(defun prune-stale-jobs-safely (project-id &key (dry-run t))
  "Iterates through jobs in PROJECT-ID and deletes FAILED ones only if salvaged."
  (let* ((raw (uiop:run-program 
               (list "gcloud" "ai" "batch-prediction-jobs" "list" 
                     "--project" project-id "--format" "json") :output :string))
         (jobs (jsown:parse raw))
         (stale-states '("JOB_STATE_FAILED" "JOB_STATE_CANCELLED")))
    (dolist (job jobs)
      (let* ((state (jsown:val job "state"))
             (name (jsown:val job "name"))
             (display-name (jsown:val job "displayName"))
             (salvage-path (merge-pathnames (format nil "salvage/~A/" display-name) 
                                            (user-homedir-pathname))))
        (when (member state stale-states :test #'string=)
          (if (probe-file salvage-path)
              (progn
                (format t "~&~:[DELETING~;[DRY-RUN] WOULD DELETE~]: ~A" dry-run display-name)
                (unless dry-run
                  (uiop:run-program (list "gcloud" "ai" "batch-prediction-jobs" "delete" name 
                                          "--project" project-id "--quiet"))))
              (format t "~&KEEPING: ~A (Not salvaged. Run (salvage-failed-job \"~A\" \"~A\"))" 
                      display-name project-id display-name)))))))

(defun save-job-metadata (job-result manifest-path)
  "Saves the job JSOWN object to an .sexp file named after the manifest."
  (let* ((base-name (pathname-name (pathname manifest-path)))
         (output-file (make-pathname :name base-name :type "sexp" 
                                     :defaults (pathname manifest-path))))
    (with-open-file (out output-file :direction :output :if-exists :supersede)
      ;; We store the parsed JSOWN object (the CAR of the do-api-request result)
      (format out "~S" (if (listp job-result) (car job-result) job-result)))
    (format t "~&Job metadata saved to: ~A~%" output-file)
    output-file))

(defun load-job-metadata (manifest-path)
  "Reads the job JSOWN object back from the .sexp file."
  (let* ((base-name (pathname-name (pathname manifest-path)))
         (input-file (make-pathname :name base-name :type "sxp" 
                                    :defaults (pathname manifest-path))))
    (with-open-file (in input-file :direction :input)
      (read in))))

(defun get-timestamp ()
  "Generates a simple timestamp string for logging."
  (multiple-value-bind (sec min hr day mon yr) (decode-universal-time (get-universal-time))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" yr mon day hr min sec)))

(defun monitor-batch-job (job-id &key (verbose t))
  "Monitors a Vertex AI batch job. Returns the full JSON response object (alist)
   so callers like monitor-several can access live completion statistics."
  (setf *use-vertex-auth* t)
  (let* ((job-json (if (stringp job-id) 
                       (car (do-api-request (format nil "https://us-central1-aiplatform.googleapis.com/v1/~a" job-id) nil :get))
                       job-id))
         (job-name (jsown:val job-json "name"))
         (display-name (jsown:val-safe job-json "displayName"))
         ;; Fetch the latest status from the API
         (endpoint (format nil "https://us-central1-aiplatform.googleapis.com/v1/~a" job-name))
         (status-json (car (do-api-request endpoint nil :get)))
         (state (jsown:val status-json "state"))
         (stats (jsown:val-safe status-json "completionStats"))
         (err-obj (jsown:val-safe status-json "error")))

    (when verbose
      (xlogntf "Checking on job ~A" (or display-name job-name))
      (xlogntft "Job \"~A\" State: ~A" job-name state)
      
      (when err-obj
        (xlogntft "!! ERROR DETECTED !!. Message: ~A" (jsown:val-safe err-obj "message"))
        (xlogf "Error ~s" status-json))
      
      (when stats
        (flet ((ensure-num (v)
                 (cond ((numberp v) v)
                       ((and (stringp v) (> (length v) 0)) (parse-integer v :junk-allowed t))
                       (t 0))))
          (let* ((succ (ensure-num (jsown:val-safe stats "successfulCount")))
                 (fail (ensure-num (jsown:val-safe stats "failedCount")))
                 (inc  (ensure-num (jsown:val-safe stats "incompleteCount")))
                 (total (+ succ fail inc))
                 (pct (if (> total 0) (* (/ (+ succ fail) total) 100.0) 0)))
            (xlogntf "Progress: ~2$% Success: ~A | Failed: ~A | Incomplete: ~A" 
                     pct succ fail inc)))))

    ;; Return the full JSON object (the alist starting with :OBJ) instead of a keyword
    status-json))

(defun monitor-security-job (job-response)
  "Prints progress for a Vertex batch job object, including error messages if they exist."
  (setf *use-vertex-auth* t)
  (let* ((job-name (jsown:val job-response "name"))
         (endpoint (format nil "https://us-central1-aiplatform.googleapis.com/v1/~a" job-name))
         (status-json (car (do-api-request endpoint nil :get)))
         (state (jsown:val status-json "state"))
         (stats (jsown:val-safe status-json "completionStats"))
         (err-obj (jsown:val-safe status-json "error"))) ;; Extract the error object
    #+nil (xlogntft "~%Checking on job ~a" (pathname-name fn))
    (xlogf "~&--- Job Status Update ---") 
    (xlogntf "The response~a" status-json)
    (xlogntft "Job ~s State: ~A" job-name state)
    
    ;; NEW: Check for and display error messages
    (when err-obj
      (let ((err-msg (jsown:val-safe err-obj "message")))
        (xlogntft "!! ERROR DETECTED !!")
        (xlogntft "Message: ~A" (or err-msg "No descriptive message provided."))))

    (if stats
        (flet ((ensure-number (val)
                 (cond ((numberp val) val)
                       ((stringp val) (parse-integer val :junk-allowed t))
                       (t 0))))
          (let* ((succ (ensure-number (jsown:val-safe stats "successfulCount")))
                 (fail (ensure-number (jsown:val-safe stats "failedCount")))
                 (inc  (ensure-number (jsown:val-safe stats "incompleteCount")))
                 (total (+ succ fail inc))
                 (pct   (if (> total 0) (* (/ (+ succ fail) total) 100.0) 0)))
            (xlogntft "Progress: ~2$% Success: ~A | Failed: ~A | Incomplete: ~A" pct succ fail inc)
            #+nil (xlogntft "Success: ~A | Failed: ~A | Incomplete: ~A" succ fail inc)))
        (xlogf "No completion stats available yet."))))

(defun run-security-scan-batch (file-to-scan questions-list model-name)
  "This is synchrous, grouping questions into a batch. Not the asyncronous mode."
  (let ((mime-type "text/plain")		; Assuming code is text/plain
        (file-id nil)
        (results nil))
    
    (xlog "Starting security scan. Uploading file once..." )
    ;; Step 1: Upload the large file (Requires implementation in gemini-client.lisp)
    (setf file-id (upload-file-to-gemini file-to-scan mime-type)) 
    (xlogntf "File uploaded successfully. ID: ~a" file-id)
    ;; Step 2: Loop through all 28 questions, referencing the uploaded file ID
    (loop for question in questions-list
          for i from 1
          do (xlogf "Processing question ~a of ~a..." i (length questions-list))
             (let ((response-candidates 
                     (call-gemini-model model-name 
                                        question 
                                        :file-ref-id file-id 
                                        :file-mime-type mime-type)))
               ;; Assuming response-candidates is the list of candidates
               (push (cons question response-candidates) results)))
    ;; Step 3: Delete the file after the scan (Optional but recommended cleanup)
    (delete-gemini-file file-id) ; Needs another dedicated drakma function
    (nreverse results)))

(defvar *gemini-file-cache* (make-hash-table :test 'equal)
  "Maps a local dependency file path to its Gemini file-ref-id.")

(defun check-batch-job-status (job-id &key (tag "batch-status"))
  "Polls the Gemini API for the status of a specific batch job.
   JOB-ID is the 'name' field returned by create-gemini-batch-job."
  (w/log  ((format nil "thinking-log-~s" tag) :dates :hour :show-log-file-name t :append-or-replace :append)
	(xlogf  "Polling status for Job: ~A" job-id)
    (handler-case
        (let* ((result (do-api-request job-id "" :get))
               (state (jsown:val-safe result "state")))
          (xlogf  "Current Job State: ~A" state)
          
          ;; Check for job-level errors (e.g., if the whole job failed)
          (let ((error-node (jsown:val-safe result "error")))
            (when error-node
              (xlogf  "Job reported internal error: ~S" error-node)))
          
          ;; Log progress if available (some versions of the API provide counts)
          (let ((progress (jsown:val-safe result "progressStats")))
            (when progress
              (xlogf "Progress: ~S" progress)))

          state)
      (error (c)
        (xlogf  "API communication error during status check: ~A" c)
        "UNKNOWN"))))

(defun create-vertex-batch-job (project-id gcs-bucket-name manifest-filename 
                                &key (model "gemini-2.5-pro") (tag "batch-create"))
  "Initiates a batch prediction job using the Vertex AI endpoint."
  (w/log (tag :dates :hms :show-log-file-name t :append-or-replace :append)
	(setf *use-vertex-auth* t) ;; Toggle to Bearer Token
	(let* ((region "us-central1")
           (bucket-path (if (uiop:string-prefix-p "gs://" gcs-bucket-name)
							gcs-bucket-name
							(format nil "gs://~a" gcs-bucket-name)))
           (manifest-uri (format nil "~a/~a" bucket-path manifest-filename))
           (output-uri (format nil "~a/reports/~a/" bucket-path tag))
           (endpoint (format nil "https://~a-aiplatform.googleapis.com/v1/projects/~a/locations/~a/batchPredictionJobs" ;; TODO parameter
							 region project-id region))
           (payload (jsown:new-js
                      ("displayName" (format nil "Security-Analysis-~a" tag))
                      ("model" (format nil "projects/~a/locations/~a/publishers/google/models/~a" 
                                       project-id region model))
					  ("inputConfig" (jsown:new-js 
									   ("instancesFormat" "jsonl")
									   ("gcsSource" (jsown:new-js 
													  ("uris" (list manifest-uri))))))
                      ("outputConfig" (jsown:new-js
										("predictionsFormat" "jsonl")
										("gcsDestination" (jsown:new-js 
															("outputUriPrefix" output-uri))))))))
      (xlogntf "cvbj: project ~s bucket path ~s manifest uri ~s manifest file name ~s"
			   project-id bucket-path    manifest-uri    manifest-filename)
	  (xlogntf "payload~a" (jsown:to-json payload))
      (let ((ans (do-api-request endpoint (jsown:to-json payload) :post)))
		(with-open-file (ans-fo (make-pathname :name (format nil "~a" (pathname-name manifest-filename)) :type "cl")
								:direction :output :if-exists :supersede :if-does-not-exist :create)
          (write ans :stream ans-fo))
		ans))))

(defun sxp-to-alist (data)
  "Recursively transforms jsown-style objects (with :OBJ markers) into standard alists."
  (cond ((and (listp data) (eq (car data) :OBJ))
         (mapcar (lambda (pair)
                   (cons (car pair) (sxp-to-alist (cdr pair))))
                 (cdr data)))
        ((listp data)
         (mapcar #'sxp-to-alist data))
        (t data)))

(defun get-sxp-field (field alist)
  "Convenience primitive to extract a field value from an sxp alist."
  (cdr (assoc field alist :test #'string=)))

(defun check-quota-status ()
  "Checks gcloud quotas with better error handling for empty responses."
  (let ((metrics '("aiplatform.googleapis.com/batch_prediction_jobs"
                   "aiplatform.googleapis.com/cpus"))
        (region "us-central1"))
    (w/log ("quota-audit" :extension "org" :dates t)
      (xlogntft "* Vertex AI Quota Audit (~A)" region)
      (xlogntft "| Metric | Limit | Usage | Status |")
      (xlogntft "|--------+-------+-------+--------|")
      (dolist (metric metrics)
        (let* ((cmd (list "gcloud" "compute" "project-info" "describe" 
                         "--format" (format nil "json(quotas.filter(metric=~A))" metric)))
               (raw-json (uiop:run-program cmd :output :string :ignore-error-status t))
               (data (cond ((and raw-json (not (string= (string-trim '(#\Space #\Newline #\Return) raw-json) "")))
                            (jsown:parse raw-json))
                           (t nil)))
               ;; Safely extract quotas list
               (quotas-list (cond (data (jsown:val data "quotas")) (t nil)))
               (quota (car quotas-list)))
          (cond (quota
                 (let ((limit (jsown:val quota "limit"))
                       (usage (jsown:val quota "usage")))
                   (xlogntft "| ~A | ~A | ~A | ~A |" 
                             metric limit usage (cond ((>= usage limit) "EXHAUSTED") (t "OK")))))
                (t (xlogntft "| ~A | N/A | N/A | NO DATA |" metric))))))))


(defun inspect-job-stalling (job-id)
  "Directly queries gcloud for the raw state of a stalled job."
  (let* ((cmd (list "gcloud" "ai" "batch-prediction-jobs" "describe" job-id 
                   "--region=us-central1" "--format=json"))
         (raw (uiop:run-program cmd :output :string :ignore-error-status t))
         (data (cond ((not (string= (string-trim '(#\Space #\Newline) raw) "")) 
                      (sxp-to-alist (jsown:parse raw)))
                     (t nil))))
    (cond (data
           (format t "--- Job Inspection: ~A ---~%" job-id)
           (format t "State: ~A~%" (get-sxp-field "state" data))
           (let ((err (get-sxp-field "error" data))
                 (partials (get-sxp-field "partialFailures" data)))
             (cond (err (format t "Main Error: ~A~%" (get-sxp-field "message" err))))
             (cond (partials 
                    (format t "Partial Failures Detected:~%")
                    (dolist (pf partials)
                      (format t "  - ~A~%" (get-sxp-field "message" pf)))))
             (cond ((not (or err partials))
                    (format t "No explicit errors found. Job is likely throttled or provisioning.~%")))))
          (t (format t "Error: Could not retrieve data for Job ID ~A. Check gcloud auth.~%" job-id)))))

(defun upload-to-gcs (local-path gcs-bucket-name &key (tag "gcs-upload"))
  "Uploads a local file to GCS using the gsutil command line tool."
  (w/log ((format nil "~a-thinking" tag) :dates :hour :show-log-file-name :both :append-or-replace :append)
	(let ((destination (format nil "gs://~a/~a" 
							   (cl-ppcre:regex-replace "^gs://" gcs-bucket-name "") 
							   (file-namestring local-path))))
      (xlogf  "Uploading ~A to ~A" local-path destination)
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program (list "gsutil" "cp" (namestring local-path) destination)
							:ignore-error-status t
							:output :string
							:error-output :string)
		(declare (ignorable output))
		(if (zerop exit-code)
			(progn
			  (xlognt  "Upload successful.")
			  t)
			(progn
			  (xlogf "Upload failed with exit code ~D~%Error: ~A" exit-code error-output)
			  nil))))))

;; list every object in the bucket:
;; gsutil ls -r gs://wglb-security-analysis-2025/**

#+nil
(defun get-or-upload-dependencies (dep-path)
  (let ((existing-id (gethash dep-path *gemini-file-cache*)))
    if existing-id
	))

