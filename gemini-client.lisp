;;; gemini-client.lisp
;;; A secure, open-source Common Lisp client for the Gemini API using IAM Service Accounts and Static Keys.
;;;
;;; Author: wgl@ciex-security.com (Based on user preference)
;;; Copyright: 2025 (Based on user preference)

(in-package #:gemini-chat-lib)

;; https://gemini.google.com/app/2986662dc6d3bc35

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
(defvar *static-api-key* nil
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

;;; gemini-client.lisp, near source: 21 (Replaces the entire DO-API-REQUEST function)

(defun do-api-request (uri-parts payload method)
  "Perform api call using either the secure Bearer token or a static API key,
   with built-in exponential backoff for rate limiting (429 errors)."
  ;; DECLARE must be the FIRST form in the function body
  (let* ((auth-info (get-auth-info))
         (headers (acons "Accept" "application/json" nil))
         (retries 0)
         (max-retries 5) ; Set a maximum number of retries ;; TODO This needs to be a -kw option
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
        
        ;; DECLARE for the M-V-B variables must be the FIRST form inside the BIND
        (declare (ignorable uri-back http-stream must-close status-text))

        (let ((body (cond ((stringp bbody) bbody)
                          (t (map 'string #'code-char bbody))))
              (our-json nil)
              (content-type (cdr (assoc :content-type headers))))
          
          (xlgt :thinking-log "Status: ~a (Attempt ~a)" status-code (1+ retries))
		  (xlg :thinking-log "Body ~s" body)

          (cond
            ;; 429: Too Many Requests - Trigger Backoff/Retry
            ((= status-code 429)
			 
             (when (>= retries max-retries)
               (error "API Quota exceeded after ~a retries. Giving up. Response body: ~%~a" 
                      max-retries body))

             (incf retries)
             ;; Wait time = 2^retries seconds + random jitter
             (let ((wait-time (+ (expt 2 retries) (random 1.0))))
               (xlgt :error-log "Quota Exceeded (429). Retrying in ~a seconds (~a/~a)." 
                     (round wait-time) retries max-retries)
			   (xlg :thinking-log "Quota Exceeded (429). Retrying in ~a seconds (~a/~a)." 
                    (round wait-time) retries max-retries)
			   
               (sleep wait-time)))

            ;; 200-399: Success or Redirects - Process result and exit loop
            ((< status-code 400)
             ;; Parse JSON if content type is application/json
             (if (and content-type (search "application/json" content-type :test #'char-equal))
                 (setf our-json (jsown:parse body)))

             ;; Return all relevant values from the multiple-value-bind and exit loop
             (return (list our-json body status-code headers uri-back http-stream must-close status-text)))

            ;; 400+: General Error - Throw error and exit loop
            (t
             (error "API Request failed with status code ~a. Response body: ~%~a" status-code body))))))))

#+nil
(defun do-api-request (uri-parts payload method)
  "Perform api call using either the secure Bearer token or a static API key,
   with built-in exponential backoff for rate limiting (429 errors)."
  (declare (ignorable user-pwd-base))
  (let* ((auth-info (get-auth-info))
         (headers (acons "Accept" "application/json" nil))
         (retries 0)
         (max-retries 5)			 ; Set a maximum number of retries
         (uri (concatenate 'string *gemini-endpoint* "/" uri-parts))) ; Define URI outside the loop

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
        
        (let ((body (cond ((stringp bbody) bbody)
                          (t (map 'string #'code-char bbody))))
              (our-json nil)
              (content-type (cdr (assoc :content-type headers))))
          
          (xlgt :thinking-log "Status: ~a (Attempt ~a)" status-code (1+ retries))

          (cond
            ;; 429: Too Many Requests - Trigger Backoff/Retry
            ((= status-code 429)
             (when (>= retries max-retries)
               (error "API Quota exceeded after ~a retries. Giving up. Response body: ~%~a" 
                      max-retries body))

             (incf retries)
             ;; Wait time = 2^retries seconds + random jitter (to avoid thundering herd)
             (let ((wait-time (+ (expt 2 retries) (random 1.0))))
               (xlogntft "Quota Exceeded (429). Retrying in ~a seconds (~a/~a)." 
                         (round wait-time) retries max-retries)
               (sleep wait-time)))

            ;; 200-399: Success or Redirects - Process result and exit loop
            ((< status-code 400)
             (declare (ignore uri-back http-stream must-close status-text))
             
             ;; Parse JSON if content type is application/json
             (if (and content-type (search "application/json" content-type :test #'char-equal))
                 (setf our-json (jsown:parse body)))

             ;; Return all relevant values from the multiple-value-bind
             (return (list our-json body status-code headers uri-back http-stream must-close status-text)))

            ;; 400+: General Error - Throw error
            (t
             (error "API Request failed with status code ~a. Response body: ~%~a" status-code body))))))))

;; --- Example Usage (Gemini/AI endpoint) ---

(defun make-gemini-payload-alist (prompt)
  "Creates the Lisp alist structure for the Gemini API request body using jsown's format."
  `(:obj
    ("contents" . ,(list 
                    `(:obj 
                      ("parts" . ,(list 
								   `(:obj ("text" . ,prompt)))))))))

(defun call-gemini-model (model-name prompt)
  "Sends a request to the Gemini generateContent endpoint (e.g., gemini-3-pro-preview)."
  (let* ((uri-parts (format nil "models/~a:generateContent" model-name))
         (payload-alist (make-gemini-payload-alist prompt))
         (payload (jsown:to-json payload-alist))
         (result (do-api-request uri-parts payload :post)) ; <-- MODIFIED
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
            ;; This is the error path you hit, now fixed by header search
            (error "Gemini API Request Succeeded (Status ~a), but returned no parsable JSON body." status-code)))))
