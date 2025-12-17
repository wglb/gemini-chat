;;; gemini-client.lisp
;;; A secure, open-source Common Lisp client for the Gemini API using IAM Service Accounts and Static Keys.
;;;
;;; Author: wgl@ciex-security.com (Based on user preference)
;;; Copyright: 2025 (Based on user preference)

(in-package #:gemini-chat-lib)
(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3)))

;; --- Constants and Variables ---

(defparameter *gemini-endpoint* "https://generativelanguage.googleapis.com/v1beta")
(defparameter *auth-endpoint* "https://www.googleapis.com/oauth2/v4/token")
(defparameter *user-agent* "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36")
(defparameter *cookie-jar* (make-instance 'drakma:cookie-jar)) 

;; Stores the Service Account Private Key info loaded from the JSON file
(defvar *iam-credentials* nil)

;; Stores the dynamic access token and its expiration time (IAM flow)
(defvar *access-token-info* '(:token nil :expires-at 0))

;; New: Stores a static API key (for simpler, but less secure, authentication)
(defparameter *static-api-key* nil
  "Stores a static Gemini API Key for direct authentication, bypassing the IAM flow.")

;; --- Core Authentication Functions (IAM/OAuth 2.0 Flow) ---

(defun initialize-iam-credentials (service-account-json-path)
  "Loads the Google Service Account JSON key file into *iam-credentials* using jsown:parse."
  (setf *iam-credentials* (with-open-file (s service-account-json-path)
                            (jsown:parse s)))
  (xlg :thinking-log "Loaded IAM credentials from ~a." service-account-json-path)
  (values))

(defun generate-jwt-assertion ()
  "Creates and signs a JSON Web Token (JWT) assertion using the JOSE library."
  (flet ((alist-to-plist (alist)
           (let ((plist nil))
             (dolist (pair alist (nreverse plist))
               (push (cdr pair) plist)
               (push (intern (string-upcase (car pair)) :keyword) plist)))))
    
    (let* ((private-key-pem (jsown:val *iam-credentials* "private_key"))
           (client-email (jsown:val *iam-credentials* "client_email"))
           (scope "https://www.googleapis.com/auth/cloud-platform")
           (now (get-universal-time))
           (private-key (ssh-keys:parse-private-key private-key-pem)) 
           
           (claims-alist `(("iss" . ,client-email)
                           ("scope" . ,scope)
                           ("aud" . ,*auth-endpoint*)
                           ("exp" . ,(+ now 3600)) ; Expires in 1 hour
                           ("iat" . ,now)))
           (claims-plist (alist-to-plist claims-alist)))
           
      (xlg :thinking-log "Signing JWT assertion for Service Account: ~a" client-email)
      ;; Use the standard 3-argument JWS/JWT encoding function.
      (jose:encode :rs256 private-key claims-plist))))

(defun get-fresh-bearer-token ()
  "Executes the JWT assertion grant flow to obtain a fresh Bearer token."
  (let* ((assertion (generate-jwt-assertion))
         (payload (format nil "grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer&assertion=~a"
                          assertion))
         (response (drakma:http-request *auth-endpoint*
                                        :method :post
                                        :content payload
                                        :content-type "application/x-www-form-urlencoded")))
    (let* ((token-data (jsown:parse response))
           (access-token (jsown:val token-data "access_token"))
           (expires-in (jsown:val token-data "expires_in")))
      (setf (getf *access-token-info* :token) access-token)
      ;; Set expiration time a bit early (10 minutes buffer) to ensure refresh
      (setf (getf *access-token-info* :expires-at) (- (+ (get-universal-time) expires-in) 600))
      (xlg :thinking-log "Successfully retrieved new access token, expires in ~a seconds." expires-in)
      access-token)))

(defun get-bearer-token ()
  "Returns the cached Bearer token, refreshing it if it's expired or nil."
  (let ((token (getf *access-token-info* :token))
        (expires-at (getf *access-token-info* :expires-at)))
    (if (or (null token) (< expires-at (get-universal-time)))
        (get-fresh-bearer-token)
        token)))

(defun get-auth-info ()
  "Determines the active authentication method and returns the necessary info."
  (cond
    ;; Priority 1: Use the static API key if set.
    (*static-api-key*
     (xlg :thinking-log "Using static API key.")
     (list :type :static-key :value *static-api-key*))
    
    ;; Priority 2: Use the IAM Bearer token if credentials are loaded.
    (*iam-credentials*
     (let ((token (get-bearer-token))) 
       (xlg :thinking-log "Using IAM Bearer token.")
       (list :type :bearer-token :value token)))

    ;; Fallback: No authentication info.
    (t
     (xlg :thinking-log "Warning: No authentication method configured.")
     nil)))


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

(defun upload-file-to-gemini (file-path mime-type)
  "Uploads a file to the Gemini File API using multipart/form-data.
Returns the file 'name' (the identifier string for subsequent API calls)."
  (let* ((auth (get-auth-info))
         (url "https://generativelanguage.googleapis.com/upload/v1beta/files")
         (metadata (jsown:to-json 
                    `(:obj ("file" . (:obj ("display_name" . ,(file-namestring file-path))
                                          ("mime_type" . ,mime-type))))))
         (parameters `(("metadata" . ,metadata)
                       ("file" . ,(pathname file-path))))
         (headers (case (getf auth :type)
                    (:static-key `(("x-goog-api-key" . ,(getf auth :value))))
                    (:bearer-token `(("Authorization" . ,(format nil "Bearer ~a" (getf auth :value)))))
                    (otherwise (error "No valid authentication found for file upload.")))))
    
    (multiple-value-bind (body status)
        (drakma:http-request url
                             :method :post
                             :parameters parameters
                             :additional-headers headers
                             :external-format-out :utf-8)
      (let* ((resp-str (if (stringp body) body (octets-to-string body)))
             (json (jsown:parse resp-str)))
        ;; Fixed typo: using 'status' instead of 'status-code'
        (if (and (>= status 200) (< status 300))
            (let ((file-id (jsown:val (jsown:val json "file") "name")))
              (xlg :thinking-log "File ~a uploaded. ID: ~a" (file-namestring file-path) file-id)
              file-id)
            (error "Gemini Upload Failed (Status ~a): ~a" status resp-str))))))

(defun delete-gemini-file (file-id)
  "Deletes a file from the Gemini File API.
'file-id' should be the full name string returned by the upload (e.g., 'files/abc-123')."
  (let* ((auth (get-auth-info))
         ;; The file-id already contains the "files/" prefix from the API response.
         (url (format nil "https://generativelanguage.googleapis.com/v1beta/~a" file-id))
         (headers (case (getf auth :type)
                    (:static-key `(("x-goog-api-key" . ,(getf auth :value))))
                    (:bearer-token `(("Authorization" . ,(format nil "Bearer ~a" (getf auth :value)))))
                    (otherwise (error "No valid authentication found for file deletion.")))))
    
    (multiple-value-bind (body status)
        (drakma:http-request url
                             :method :delete
                             :additional-headers headers)
      (if (and (>= status 200) (< status 300))
          (progn
            (xlg :thinking-log "Successfully deleted Gemini file: ~a" file-id)
            t)
          (error "Failed to delete Gemini file ~a (Status ~a): ~a" 
                 file-id status (if (stringp body) body (octets-to-string body)))))))


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

(defun get-or-upload-dependencies (dep-path)
  (let ((existing-id (gethash dep-path *gemini-file-cache*)))
    (if existing-id
        existing-id ;; Reuse the one already on the server
        (let ((new-id (upload-file-to-gemini dep-path "text/plain")))
          (setf (gethash dep-path *gemini-file-cache*) new-id)
          new-id))))



;;; gemini-client.lisp (Replace existing MAKE-GEMINI-PAYLOAD-ALIST)

										; NREVERSE to ensure file part is first

;;; gemini-client.lisp (Replace existing CALL-GEMINI-MODEL)


