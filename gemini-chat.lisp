;;;; gemini-chat.lisp
;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This Lisp program tests the validity of your Google Gemini API key
;;;; by making a simple request to the Gemini API, now supporting multi-turn conversations.
;;;;
;;;; Prerequisites:
;;;; 1. A Common Lisp implementation (e.g., SBCL, CCL).
;;;; 2. Quicklisp installed (the de facto Common Lisp package manager).
;;;;    If you don't have Quicklisp, you can install it by following
;;;;    instructions on its website: https://www.quicklisp.org/beta/
;;;; 3. The 'drakma' and 'cl-json' libraries installed via Quicklisp.
;;;;    (ql:quickload :drakma)
;;;;    (ql:quickload :cl-json)
;;;; 4. Your Gemini API key set as an environment variable named
;;;;    'GEMINI_API_KEY' (or '_GEMINI_API_KEY_' if you prefer to keep your current setup).

;; Load necessary libraries using Quicklisp.
;; If you run this file directly, ensure Quicklisp is loaded first in your Lisp session.
;; For example, in SBCL: (load "~/quicklisp/setup.lisp")

(in-package #:gemini-chat)

(ql:quickload :drakma)
(ql:quickload :cl-json)
(ql:quickload :uiop) ; For uiop:getenv

(defun get-gemini-api-key ()
  "Retrieves the Gemini API key from the GEMINI_API_KEY environment variable.
   Signals an error if the environment variable is not set.
   It first tries GEMINI_API_KEY, then falls back to _GEMINI_API_KEY_."
  (let ((key (or (uiop:getenv "GEMINI_API_KEY")
                 (uiop:getenv "_GEMINI_API_KEY_")))) ; Try standard name first, then fallback
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

(defun message-to-json-string (message-alist)
  "Converts a single message alist (role and parts) into its JSON string representation.
   Uses cl-json:encode-json-to-string for proper string escaping."
  (let* ((role (cdr (assoc "role" message-alist :test #'string=)))
         (parts-list (cdr (assoc "parts" message-alist :test #'string=)))
         (text (cdr (assoc "text" (car parts-list) :test #'string=)))) ; Assuming one part with text
    (format nil "{\"role\":\"~a\",\"parts\":[{\"text\":~a}]}"
            role
            (cl-json:encode-json-to-string text)))) ; Encode text to handle quotes/special chars

(defun make-gemini-api-request (messages)
  "Constructs and sends an HTTP POST request to the Gemini API.
   'messages' should be a list of Lisp alists, each representing a conversation turn.
   Returns the response stream if successful."
  (let* ((api-key (get-gemini-api-key))
         ;; Use gemini-1.5-flash as a common model for testing
         (api-url (format nil "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent?key=~a" api-key))
         ;; Manually construct the JSON string for the entire payload
         (messages-json-strings (mapcar #'message-to-json-string messages))
         (contents-array-string (format nil "[~{~a~^,~}]" messages-json-strings)) ; Join message strings with commas
         (json-payload-string (format nil "{\"contents\":~a}" contents-array-string)) ; Wrap in "contents" object
         ;; Set the Content-Type header
         (headers '(("Content-Type" . "application/json"))))
    ;; Debug prints to verify the payload before sending
    (format t "~&Making API request to: ~a~%" api-url)
    (format t "JSON string being sent: ~a~%" json-payload-string)

    ;; The body of the let* starts here.
    (handler-case
        (drakma:http-request api-url
                             :method :post
                             :content-type "application/json"
                             :content json-payload-string
                             :additional-headers headers
                             :want-stream t   ; Request a stream to read the response
                             :force-ssl t)    ; Ensure HTTPS is used
      (drakma:drakma-error (c)
        ;; Catch Drakma-specific HTTP errors
        (error "HTTP Request Failed: ~a" c))
      (error (c)
        ;; Catch any other general errors during the request
        (error "An unexpected error occurred during the HTTP request: ~a" c)))))

(defun parse-gemini-api-response (response-stream)
  "Parses the JSON response from the Gemini API response stream.
   Returns the parsed JSON as a Lisp object."
  (handler-case
      (cl-json:decode-json response-stream)
    (error (c)
      ;; Catch JSON parsing errors
      (error "Failed to parse JSON response: ~a" c))))

(defun extract-gemini-text (parsed-json)
  "Extracts the generated text from the parsed Gemini API JSON response.
   Returns the text string or NIL if not found."
  (let* ((candidates (cdr (assoc :candidates parsed-json)))
         (first-candidate (car candidates)))
    (when first-candidate
      (let* ((content (cdr (assoc :content first-candidate)))
             (parts (cdr (assoc :parts content)))
             (first-part (car parts)))
        (when first-part
          (cdr (assoc :text first-part)))))))

(defun run-gemini-conversation (initial-prompt)
  "Starts and manages a multi-turn conversation with the Gemini API.
   Takes an initial prompt, then allows for follow-up questions.
   Returns the complete conversation history."
  (let ((conversation-history nil)) ; Initialize empty history

    ;; First turn
    (format t "~&User: ~a~%" initial-prompt)
    (let* ((user-turn (create-message-turn "user" initial-prompt))
           (response-stream (make-gemini-api-request (list user-turn)))
           (parsed-json (parse-gemini-api-response response-stream))
           (model-response-text (extract-gemini-text parsed-json))
           (model-turn (create-message-turn "model" model-response-text)))

      (if model-response-text
          (progn
            (format t "~&Gemini: ~a~%" model-response-text)
            (setf conversation-history (append conversation-history (list user-turn model-turn))))
          (progn
            (format t "~&Error on initial turn: No text generated or unexpected response structure.~%")
            (format t "Parsed JSON: ~s~%" parsed-json)
            (return-from run-gemini-conversation nil))) ; Exit if initial turn fails

      ;; Loop for follow-up turns
      (loop
        (format t "~&~%Enter your next prompt (or type 'quit' to end):~%")
        (let ((next-prompt (read-line)))
          (when (string-equal next-prompt "quit")
            (format t "~&~%Ending conversation.~%")
            (return))

          (let* ((new-user-turn (create-message-turn "user" next-prompt))
                 ;; Append new user turn to history before sending
                 (updated-history (append conversation-history (list new-user-turn)))
                 (response-stream (make-gemini-api-request updated-history))
                 (parsed-json (parse-gemini-api-response response-stream))
                 (model-response-text (extract-gemini-text parsed-json))
                 (new-model-turn (create-message-turn "model" model-response-text)))

            (if new-model-turn
                (progn
                  (format t "~&Gemini: ~a~%" new-model-turn)
                  ;; Append new model turn to history for next iteration
                  (setf conversation-history (append updated-history (list new-model-turn))))
                (progn
                  (format t "~&Error on follow-up turn: No text generated or unexpected response structure.~%")
                  (format t "Parsed JSON: ~s~%" parsed-json)
                  ;; Don't add to history if it failed
                  (return)))))))
    conversation-history)) ; Return the full history at the end

;; Example usage for multi-turn conversation:
;; To run this, ensure your Lisp environment is set up and then call:
;; (run-gemini-conversation "Hello, Gemini! What can you do?")

