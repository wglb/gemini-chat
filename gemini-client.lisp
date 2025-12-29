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

(defvar *gemini-service-account* nil
  "The GCP Service Account email for impersonation. 
Users should set this or the GEMINI_SERVICE_ACCOUNT environment variable.")

(defvar *gemini-service-account-scopes* nil)

(defun get-fresh-gemini-token ()
  "Fetches a fresh 60-minute token via gcloud impersonation with the correct scopes."
  (let ((sa-email (or *gemini-service-account*
                      (uiop:getenv "GEMINI_SERVICE_ACCOUNT"))))
    (cond
      ((not sa-email)
       (xlg :thinking-log "Auth Error: No Service Account configured.")
       nil)
      (t
       (handler-case
		   (uiop:run-program 
			(list "gcloud" "auth" "print-access-token" 
				  (format nil "--impersonate-service-account=~a" sa-email)
				  ;; Add the specific Generative Language scope here
				  (format nil "--scopes=~a" *gemini-service-account-scopes*)
				  #+nil "--scopes=https://www.googleapis.com/auth/cloud-platform,https://www.googleapis.com/auth/generative-language")
			:output '(:string :stripped t)
			:error-output nil)
         (error (c)
           (xlg :thinking-log "Gcloud execution failed: ~a" c)
           nil))))))

(defun get-auth-info ()
  "Determines the active authentication method and returns the necessary info."
  (let ((token nil))
    (cond
      ;; Priority 1: Use the static API key if set.
      (*static-api-key*
       (xlg :thinking-log "Using static API key.")
       (list :type :static-key :value *static-api-key*)) ;; TODO this needs to be a config value

      ;; Priority 2: Use gcloud impersonation.
      ;; This now correctly calls the helper function defined above.
      ((setf token (get-fresh-gemini-token))
       (xlg :thinking-log "Using impersonated IAM Bearer token.")
       (list :type :bearer-token :value token))

      ;; Fallback: No authentication info.
      (t
       (xlgt :thinking-log "Warning: No authentication method configured (gcloud failed or not set).")
	   (error "Warning: No authentication method configured (gcloud failed or not set).")
       nil))))

;; --- Core API Request Function ---

(defun do-api-request (uri-parts payload method)
  "Perform api call using either the secure Bearer token or a static API key,
   with built-in exponential backoff for rate limiting (429 errors)."
  (let* ((auth-info (get-auth-info))
         (headers (acons "Accept" "application/json" nil))
         (retries 0)
         (max-retries 5)
         (uri (concatenate 'string *gemini-endpoint* "/" uri-parts)))

    ;; Authentication setup (remains outside the retry loop)
    (cond
      ((eq (getf auth-info :type) :static-key)
       (setf headers (acons "x-goog-api-key" (getf auth-info :value) headers)))
      
      ((eq (getf auth-info :type) :bearer-token)
       (let ((token (getf auth-info :value)))
         (setf headers (acons "Authorization" (concatenate 'string "Bearer " token) headers))))
      
      (t
       (xlg :thinking-log "No authentication information available for API request.")))

    ;; START OF EXPONENTIAL BACKOFF LOOP
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
          
          (xlg :thinking-log (format nil "Status: ~a (Attempt ~a)" status-code (1+ retries)))

          (cond
            ;; 429: Too Many Requests - Trigger Backoff/Retry
            ((= status-code 429)
             (when (>= retries max-retries)
               ;; FIX: Max retries reached. Return the response to allow the detailed
               ;; JSON error message to be parsed by the calling function.
               (xlg :thinking-log "API Quota exceeded after ~a retries. Returning raw error response for parsing." max-retries)
               
               ;; Attempt to parse JSON before returning, just like success/other error cases
               (setf our-json (if (and content-type (search "application/json" content-type :test #'char-equal))
                                  (jsown:parse body)
                                  nil))
               ;; Exit the loop with the error response
               (return (list our-json body status-code headers uri-back http-stream must-close status-text)))

             (incf retries)
             (let ((wait-time (+ (expt 2 retries) (random 1.0))))
               (xlg :thinking-log (format nil "Quota Exceeded (429). Retrying in ~a seconds (~a/~a)." 
                                          (round wait-time) retries max-retries))
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
             (xlg :thinking-log (format nil "API Request failed with status code ~a. Returning raw error response for parsing." status-code))
             ;; Parse JSON if content type is application/json
             (if (and content-type (search "application/json" content-type :test #'char-equal))
                 (setf our-json (jsown:parse body)))
             (return (list our-json body status-code headers uri-back http-stream must-close status-text)))))))))

;; --- Example Usage (Gemini/AI endpoint) ---

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

(defun octets-to-string (octets)
  "Decodes a byte vector into a UTF-8 string for the Gemini API."
  (babel:octets-to-string octets :encoding :utf-8))

(defun read-file-to-octets (path)
  "Manual binary read to avoid UIOP/Alexandria versioning issues."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      bytes)))


#+nil
(defun extract-project-id-from-sa (sa-email)
  "Extracts 'project-id' from 'name@project-id.iam.gserviceaccount.com'."
  (let* ((domain (second (uiop:split-string sa-email :separator '(#\@))))
         (parts (uiop:split-string domain :separator '(#\.))))
    (first parts)))


(defun extract-project-id-from-sa (sa-email)
  "Extracts 'project-id' from 'name@project-id.iam.gserviceaccount.com'."
  (let* ((domain (second (uiop:split-string sa-email :separator '(#\@))))
         (parts (uiop:split-string domain :separator '(#\.))))
    (first parts)))


(defun upload-file-to-gemini (file-path mime-type)
  "Uploads a file to Gemini using a Static API Key to bypass Service Account 400 errors."
  (let* ((static-key (uiop:getenv "GEMINI_API_KEY")) 
         (base-url "https://generativelanguage.googleapis.com/upload/v1beta/files")
         (display-name (format nil "up-~a" (get-universal-time)))
         (file-bytes (read-file-to-octets file-path))
         (auth-headers `(("x-goog-api-key" . ,(or static-key "")))))
    
    (unless static-key
      (error "Upload requires GEMINI_API_KEY environment variable."))

    (let* ((init-url (format nil "~a?uploadType=resumable" base-url))
           (metadata (jsown:to-json `(:obj ("file" . (:obj ("display_name" . ,display-name)))))))
      
      (multiple-value-bind (body status resp-headers)
          (drakma:http-request init-url :method :post 
                               :content metadata 
                               :content-type "application/json"
                               :additional-headers auth-headers)
        (cond
          ;; Success Path: 200 OK and we have the upload destination
          ((and (= status 200)
                (or (cdr (assoc :location resp-headers))
                    (cdr (assoc :x-goog-upload-url resp-headers))))
           (let ((upload-url (or (cdr (assoc :location resp-headers))
                                 (cdr (assoc :x-goog-upload-url resp-headers)))))
             (multiple-value-bind (final-body final-status)
                 (drakma:http-request upload-url :method :post
                                      :content file-bytes
                                      :additional-headers (append auth-headers
                                                                  `(("Content-Type" . ,mime-type))))
               (parse-gemini-upload-response final-body final-status))))

          ;; Error Path: Either 400/403 or 200 with missing headers
          (t
           (let ((err-msg (if (stringp body) body (flexi-streams:octets-to-string body))))
             (error "Upload failed (Status ~a): ~a" status err-msg))))))))



(defun parse-gemini-upload-response (body status)
  (let* ((resp-str (if (stringp body) body (flexi-streams:octets-to-string body)))
         (json (jsown:parse resp-str)))
    (if (and (>= status 200) (< status 300))
        (jsown:val (if (jsown:keyp json "file") (jsown:val json "file") json) "name")
        (error "Gemini Upload Failed (~a): ~a" status resp-str))))

(defun upload-multipart-simple (url metadata file-path mime-type headers)
  (flexi-streams:with-input-from-sequence 
      (m-stream (flexi-streams:string-to-octets metadata :external-format :utf-8))
    (multiple-value-bind (body status)
        (drakma:http-request url
                             :method :post
                             :parameters `(("metadata" ,m-stream :content-type "application/json")
                                           ("file" ,(pathname file-path) :content-type ,mime-type))
                             :additional-headers headers
                             :form-data t)
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

;;; gemini-chat-lib.lisp (Conceptual new function for batch scanning)

(defun run-security-scan-batch (file-to-scan questions-list model-name)
  (let ((mime-type "text/plain")		; Assuming code is text/plain
        (file-id nil)
        (results nil))
    
    (xlg :thinking-log "Starting security scan. Uploading file once...")
    ;; Step 1: Upload the large file (Requires implementation in gemini-client.lisp)
    (setf file-id (upload-file-to-gemini file-to-scan mime-type)) 
    (xlg :thinking-log "File uploaded successfully. ID: ~a" file-id)
    ;; Step 2: Loop through all 28 questions, referencing the uploaded file ID
    (loop for question in questions-list
          for i from 1
          do (xlg :thinking-log "Processing question ~a of ~a..." i (length questions-list))
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

#+nil
(defun get-or-upload-dependencies (dep-path)
  (let ((existing-id (gethash dep-path *gemini-file-cache*)))
    if existing-id
	))
