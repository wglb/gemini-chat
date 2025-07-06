;;;; gemini-chat.lisp
;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This Lisp program interacts with the Google Gemini API,
;;;; supporting multi-turn conversations and model selection.
;;;;
;;;; Prerequisites:
;;;; 1. A Common Lisp implementation (e.g., SBCL, CCL).
;;;; 2. Quicklisp installed.
;;;; 3. The 'drakma' and 'cl-json' libraries installed via Quicklisp.
;;;; 4. Your Gemini API key set as an environment variable named
;;;;    'GEMINI_API_KEY' (or '_GEMINI_API_KEY_').

;; This file assumes its package is defined in gemini-chat-pkg.lisp
;; and that it's loaded as part of an ASDF system.

(in-package #:gemini-chat) ; Ensure we are in the correct package after loading

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

(defun get-gemini-api-key ()
  "Retrieves the Gemini API key from the GEMINI_API_KEY environment variable.
   Signals an error if the environment variable is not set.
   It first tries GEMINI_API_KEY, then falls back to _GEMINI_API_KEY_."
  (let ((key (or (getenv "GEMINI_API_KEY") ; Changed from uiop:getenv
                 (getenv "_GEMINI_API_KEY_")))) ; Changed from uiop:getenv
    (unless key
      (error "Error: Neither GEMINI_API_KEY nor _GEMINI_API_KEY_ environment variables are set.
              Please set one of them before running this program."))
    key))

(defun create-message-part (text)
  "Creates a Lisp alist representing a 'part' in the Gemini API JSON structure."
  (list (cons "text" text)))

(defun create-message-content (parts)
  "Creates a Lisp alist representing 'content' in the Gemini API JSON structure."
  (list (cons "parts" parts)))

(defun create-message-turn (role text)
  "Creates a Lisp alist representing a single 'turn' (message) in the Gemini API JSON structure."
  (list (cons "role" role)
        (cons "parts" (list (create-message-part text)))))

(defun make-gemini-api-request (messages &key (model "gemini-2.5-pro"))
  "Constructs and sends an HTTP POST request to the Gemini API.
   'messages' should be a list of Lisp alists, each representing a conversation turn.
   'model' specifies the Gemini model to use (e.g., \"gemini-2.5-pro\", \"gemini-1.5-flash\").
   Returns the response stream if successful."
  (let* ((api-key (get-gemini-api-key))
         ;; Dynamically construct the API URL with the chosen model
         (api-url (format nil "https://generativelanguage.googleapis.com/v1beta/models/~a:generateContent?key=~a"
                          model api-key))
         ;; Directly create the full JSON payload as a Lisp alist
         (json-payload-alist (list (cons "contents" messages)))
         ;; Use cl-json:encode-json-to-string to convert the alist to a JSON string
         (json-payload-string (encode-json-to-string json-payload-alist)) ; Changed from cl-json:encode-json-to-string
         ;; Set the Content-Type header
         (headers '(("Content-Type" . "application/json"))))
    ;; Debug prints to verify the payload before sending
    (format t "~&Making API request to: ~a~%" api-url)
    (format t "JSON string being sent: ~a~%" json-payload-string)

    (handler-case
        (http-request api-url ; Changed from drakma:http-request
                             :method :post
                             :content-type "application/json"
                             :content json-payload-string
                             :additional-headers headers
                             :want-stream t
                             :force-ssl t)
      (drakma-error (c) ; Changed from drakma:drakma-error
        (error "HTTP Request Failed: ~a" c))
      (error (c)
        (error "An unexpected error occurred during the HTTP request: ~a" c)))))

(defun parse-gemini-api-response (response-stream)
  "Parses the JSON response from the Gemini API response stream.
   Returns the parsed JSON as a Lisp object."
  (handler-case
      (decode-json response-stream) ; Changed from cl-json:decode-json
    (error (c)
      (error "Failed to parse JSON response: ~a" c))))

(defun extract-gemini-text (parsed-json)
  "Extracts the generated text from the parsed Gemini API JSON response.
   Returns the text string or NIL if not found.
   Note: cl-json decodes JSON keys as Lisp keywords by default unless configured otherwise."
  (let* ((candidates (cdr (assoc :candidates parsed-json)))
         (first-candidate (car candidates)))
    (when first-candidate
      (let* ((content (cdr (assoc :content first-candidate)))
             (parts (cdr (assoc :parts content)))
             (first-part (car parts)))
        (when first-part
          (cdr (assoc :text first-part)))))))

(defun run-gemini-conversation (initial-prompt &key (model "gemini-2.5-pro"))
  "Starts and manages a multi-turn conversation with the Gemini API.
   Takes an initial prompt, then allows for follow-up questions.
   Returns the complete conversation history."
  (let ((conversation-history nil))

    ;; First turn
    (format t "~&User: ~a~%" initial-prompt)
    (let* ((user-turn (create-message-turn "user" initial-prompt))
           (response-stream (make-gemini-api-request (list user-turn) :model model))
           (parsed-json (parse-gemini-api-response response-stream))
           (model-response-text (extract-gemini-text parsed-json))
           (model-turn (create-message-turn "model" (or model-response-text "Error: No response"))))

      (if model-response-text
          (progn
            (format t "~&Gemini: ~a~%" model-response-text)
            (setf conversation-history (append conversation-history (list user-turn model-turn))))
          (progn
            (format t "~&Error on initial turn: No text generated or unexpected response structure.~%")
            (format t "Parsed JSON: ~s~%" parsed-json)
            (return-from run-gemini-conversation nil)))

      ;; Loop for follow-up turns
      (loop
        (format t "~&~%Enter your next prompt (or type 'quit' to end):~%")
        (let ((next-prompt (read-line)))
          (when (string-equal next-prompt "quit")
            (format t "~&~%Ending conversation.~%")
            (return))

          (let* ((new-user-turn (create-message-turn "user" next-prompt))
                 (updated-history (append conversation-history (list new-user-turn)))
                 (response-stream (make-gemini-api-request updated-history :model model))
                 (parsed-json (parse-gemini-api-response response-stream))
                 (model-response-text (extract-gemini-text parsed-json))
                 (new-model-turn (create-message-turn "model" (or model-response-text "Error: No response"))))

            (if model-response-text
                (progn
                  (format t "~&Gemini: ~a~%" model-response-text)
                  (setf conversation-history (append updated-history (list new-model-turn))))
                (progn
                  (format t "~&Error on follow-up turn: No text generated or unexpected response structure.~%")
                  (format t "Parsed JSON: ~s~%" parsed-json)
                  (return)))))))
    conversation-history))

;; Example usage for multi-turn conversation:
;; To run this, ensure your Lisp environment is set up and then call:
;; (in-package #:gemini-chat)
;; (run-gemini-conversation "Hello, Gemini! What can you do?" :model "gemini-2.5-pro")
;; (run-gemini-conversation "What's the best recipe for spaghetti carbonara?" :model "gemini-1.5-flash")
;; (run-gemini-conversation "Tell me a short story.") ; Will use the default "gemini-2.5-pro"
