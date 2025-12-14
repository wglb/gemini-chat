;;; gemini-client.lisp
;;; A secure, open-source Common Lisp client for the Gemini API using IAM Service Accounts.
;;;
;;; Author: wgl@ciex-security.com (Based on user preference)
;;; Copyright: 2025 (Based on user preference)

(in-package #:gemini-chat-lib) ; Added based on your latest instruction

;; https://gemini.google.com/app/2986662dc6d3bc35

(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3)))

;; --- Constants and Variables ---

(defparameter *gemini-endpoint* "https://generativelanguage.googleapis.com/v1beta")
(defparameter *auth-endpoint* "https://www.googleapis.com/oauth2/v4/token")
(defparameter *user-agent* "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36")
(defparameter *cookie-jar* (make-instance 'drakma:cookie-jar)) 

;; Stores the Service Account Private Key info loaded from the JSON file
(defvar *iam-credentials* nil)

;; Stores the dynamic access token and its expiration time
(defvar *access-token-info* '(:token nil :expires-at 0))

;; --- Core Authentication Functions (IAM/OAuth 2.0 Flow) ---

(defun initialize-iam-credentials (service-account-json-path)
  "Loads the Google Service Account JSON key file into *iam-credentials* using jsown:parse."
  (setf *iam-credentials* (with-open-file (s service-account-json-path)
                            (jsown:parse s)))
  ;; FIX: Corrected logging to use XLG :THINKING-LOG with format string
  (xlg :thinking-log "Loaded IAM credentials from ~a." service-account-json-path)
  (values))

(defun generate-jwt-assertion ()
  "Creates and signs a JSON Web Token (JWT) assertion using the JOSE library."
  ;; Helper function to convert alists (used for JSON structures) to plists (used by JOSE positional arguments)
  (flet ((alist-to-plist (alist)
           (let ((plist nil))
             (dolist (pair alist (nreverse plist))
               ;; Converts the key string to a Lisp keyword symbol
               (push (cdr pair) plist)
               (push (intern (string-upcase (car pair)) :keyword) plist)))))
    
    (let* ((private-key-pem (jsown:val *iam-credentials* "private_key"))
           (client-email (jsown:val *iam-credentials* "client_email"))
           (scope "https://www.googleapis.com/auth/cloud-platform")
           (now (get-universal-time))
           
           (private-key (ssh-keys:parse-private-key private-key-pem)) 
           
           ;; Define claims payload as alist
           (claims-alist `(("iss" . ,client-email)
                           ("scope" . ,scope)
                           ("aud" . ,*auth-endpoint*)
                           ("exp" . ,(+ now 3600)) ; Expires in 1 hour
                           ("iat" . ,now)))
           
           ;; Convert to plist for JOSE:ENCODE positional call
           (claims-plist (alist-to-plist claims-alist)))
           
      ;; FIX: Corrected logging to use XLG :THINKING-LOG with format string
      (xlg :thinking-log "Signing JWT assertion for Service Account: ~a" client-email)

      ;; FIX: Use the standard 3-argument JWS/JWT encoding function.
      ;; This removes the invalid keyword argument :PROTECTED.
      (jose:encode :rs256 private-key claims-plist))))

(defun get-fresh-access-token ()
  "Executes the JWT assertion grant flow to obtain a fresh Bearer token."
  (let* ((assertion (generate-jwt-assertion))
         ;; FIX: The assertion is already URL-safe. Removed erroneous DRAKMA:URL-ENCODE call.
         (payload (format nil "grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer&assertion=~a"
                          assertion))
         ;; Drakma POSTs the assertion to the token endpoint
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
      ;; FIX: Corrected logging to use XLG :THINKING-LOG with format string
      (xlg :thinking-log "Successfully retrieved new access token, expires in ~a seconds." expires-in)
      access-token)))

(defun get-current-access-token ()
  "Returns the cached token, refreshing it if it's expired or nil."
  (let ((token (getf *access-token-info* :token))
        (expires-at (getf *access-token-info* :expires-at)))
    (if (or (null token) (< expires-at (get-universal-time)))
        (get-fresh-access-token)
        token)))

;; --- Core API Request Function ---

(defun do-api-request (uri-parts &key (payload nil) (method :get) (user-pwd-base nil))
  "Perform api call using the secure Bearer token. This is the new 'do-api' logic."
  (declare (ignorable user-pwd-base)) ; Ignores unused Basic Auth parameter from original
  (let ((token (get-current-access-token)))
    ;; FIX: Corrected logging to use XLG :THINKING-LOG with format string
    (xlg :thinking-log "do-api: uri-parts ~a method ~a" uri-parts method)
    (multiple-value-bind (bbody status-code headers uri-back http-stream must-close status-text)
        (drakma:http-request
         (concatenate 'string *gemini-endpoint* "/" uri-parts)
         :method method
         :cookie-jar *cookie-jar*
         :user-agent *user-agent*
         :content payload
         :content-type "application/json"
         ;; CRITICAL: Uses Authorization: Bearer <token> instead of Basic <b64>
         :additional-headers (acons "Authorization" (concatenate 'string "Bearer " token)
                                    (acons "Accept" "application/json" nil)))
      (declare (ignorable bbody status-code headers uri-back http-stream must-close status-text))
      (let ((body (cond ((stringp bbody) bbody)
                        (t (map 'string #'code-char bbody))))
            (our-json nil))
        (if (string= (cdr (assoc :content-type headers)) "application/json")
            (setf our-json (jsown:parse body))) ; Uses jsown:parse

        ;; FIX: Corrected logging to use XLGT :THINKING-LOG with format string
        (xlgt :thinking-log "Status: ~a" status-code)
        (list our-json body status-code headers uri-back http-stream must-close status-text)))))

;; --- Example Usage (Gemini/AI endpoint) ---

(defun make-gemini-payload-alist (prompt)
  "Creates the Lisp alist structure for the Gemini API request body using jsown's format. This is a helper function to prevent compiler parsing errors."
  `(:obj
    ("contents" . ,(list ; The list for the "contents" array
                    `(:obj ; The first object in the contents array
                      ("parts" . ,(list ; The list for the "parts" array
                                   `(:obj ("text" . ,prompt)))))))))

(defun call-gemini-model (model-name prompt)
  "Sends a request to the Gemini generateContent endpoint (e.g., gemini-3-pro-preview)."
  (let* ((uri-parts (format nil "models/~a:generateContent" model-name))
         (payload-alist (make-gemini-payload-alist prompt))
         (payload (jsown:to-json payload-alist))
         (result (do-api-request uri-parts :payload payload :method :post)))
    (jsown:val (first result) "candidates")))
