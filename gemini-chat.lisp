;;;; gemini-chat.lisp
;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:gemini-chat)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

(defun get-gemini-api-key ()
  "Retrieves the Gemini API key from the GEMINI_API_KEY environment variable.
   Signals an error if the environment variable is not set.
   It first tries GEMINI_API_KEY, then falls back to _GEMINI_API_KEY_."
  (let ((key (or (getenv "GEMINI_API_KEY")
                 (getenv "_GEMINI_API_KEY_"))))
    (unless key
      (error "Error: Neither GEMINI_API_KEY nor _GEMINI_API_KEY_ environment variables are set.
              Please set one of them before running this program."))
    key))

;; --- JSOWN-specific data structure creation ---
;; We will now explicitly use JSOWN's :OBJ format with STRING keys for JSON objects.
;; This ensures correct casing and object structure.

;;;; gemini-chat.lisp
;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:gemini-chat)

;; ... (get-gemini-api-key remains the same) ...

;; --- JSOWN-specific data structure creation ---
;; We will now explicitly use JSOWN's :OBJ format with STRING keys for JSON objects.
;; This ensures correct casing and object structure.

(defun create-message-part (text)
  "Creates a Lisp :OBJ representing a 'part' in the Gemini API JSON structure."
  (jsown:new-js ("text" text))) ; Correct: {"text": "..."}

;; REVISED: create-message-content is not directly used by create-message-turn
;; We'll inline its logic directly into create-message-turn's parts list.
;; This function can be removed if not used elsewhere, or kept for clarity.
(defun create-message-content (parts)
  "Creates a Lisp :OBJ representing 'content' in the Gemini API JSON structure."
  (jsown:new-js ("parts" parts))) ; Correct: {"parts": [...]}

;; REVISED: This is the critical function
(defun create-message-turn (role text)
  "Creates a Lisp :OBJ representing a single 'turn' (message) in the Gemini API JSON structure.
   This will be encoded by JSOWN as a JSON object."
  (jsown:new-js
    ("role" role)            ; <--- CORRECTED: "role" is a direct string value
    ("parts" (list (create-message-part text))))) ; <--- "parts" is a key, its value is a list (JSON array) of parts

;; ... (make-gemini-api-request remains the same) ...
;; ... (parse-gemini-api-response remains the same) ...
;; ... (extract-gemini-text remains the same) ...
;; ... (run-gemini-conversation remains the same) ...


(defun make-gemini-api-request (messages &key (model "gemini-2.5-pro"))
  "Constructs and sends an HTTP POST request to the Gemini API.
   'messages' should be a list of Lisp :OBJ structures, each representing a conversation turn.
   'model' specifies the Gemini model to use (e.g., \"gemini-2.5-pro\", \"gemini-1.5-flash\").
   Returns the response stream if successful."
  (let* ((api-key (get-gemini-api-key))
         (api-url (format nil "https://generativelanguage.googleapis.com/v1beta/models/~a:generateContent?key=~a"
                          model api-key))
         ;; --- JSOWN ENCODING (Revised) ---
         ;; The top-level payload needs to be a JSON object with a "contents" key.
         ;; We must explicitly construct this using jsown:new-js to guarantee :OBJ format
         ;; and string keys for correct casing.
         (json-payload-lisp-object (jsown:new-js ("contents" messages))) ; <--- Use jsown:new-js for top-level object

         (json-payload-string (jsown:to-json json-payload-lisp-object)) ; <--- Use JSOWN:TO-JSON
         ;; --- END JSOWN ENCODING ---

         (headers '(("Content-Type" . "application/json"))))
    (xlg :thinking-log "~&Making API request to: ~a~%" api-url)
    (xlg :thinking-log "JSON string being sent: ~a~%" json-payload-string)

    (time (handler-case
        (http-request api-url
                             :method :post
                             :content-type "application/json"
                             :content json-payload-string
                             :additional-headers headers
                             :want-stream t
                             :force-ssl t)
      (drakma-error (c)
        (error "HTTP Request Failed: ~a" c))
      (error (c)
        (error "An unexpected error occurred during the HTTP request: ~a" c))))))

(defun parse-gemini-api-response (response-stream)
  "Parses the JSON response from the Gemini API response stream using jsown.
   Returns the parsed JSON as a Lisp object (jsown's internal :OBJ format)."
  (handler-case
      (let ((json-string (uiop:slurp-stream-string response-stream)))
        (xlg :thinking-log "~&Raw JSON string received: ~a~%" json-string)
        (jsown:parse json-string))
    (error (c)
      (error "Failed to parse JSON response: ~a" c))))

(defun extract-gemini-text (parsed-json)
  "Extracts the generated text from the parsed Gemini API JSON response using jsown accessors.
   Returns the text string or NIL if not found.
   JSOWN represents JSON objects as (:OBJ key1 val1 key2 val2 ...)."
  ;; Add a check for an 'error' key at the top level
  (if (jsown:keyp parsed-json "error") ; <--- Check for "error" key
      (progn
        (xlg :thinking-log "~&API returned an error: ~a~%" (jsown:val parsed-json "error"))
        (xlg :answer-log "~&API returned an error: ~a~%" (jsown:val parsed-json "error"))
        nil) ; Return NIL if there's an error in the response
      (let* ((candidates (jsown:val parsed-json "candidates"))
             (first-candidate (car candidates)))
        (when first-candidate
          (let* ((content (jsown:val first-candidate "content"))
                 (parts (jsown:val content "parts"))
                 (first-part (car parts)))
            (when first-part
              (jsown:val first-part "text")))))))

(defun run-gemini-conversation (initial-prompt &key (model "gemini-2.5-pro"))
  "Starts and manages a multi-turn conversation with the Gemini API.
   Takes an initial prompt, then allows for follow-up questions.
   Returns the complete conversation history."
  (with-open-log-files ((:answer-log "answer" :ymd)
                        (:thinking-log "thinking :ymd"))
    (let ((conversation-history nil))

    ;; First turn
    (xlg :thinking-log "~&User: ~a" initial-prompt)
    (let* ((user-turn (create-message-turn "user" initial-prompt))
           (response-stream (make-gemini-api-request (list user-turn) :model model))
           (parsed-json (parse-gemini-api-response response-stream))
           (model-response-text (extract-gemini-text parsed-json)) ; This now handles error responses gracefully
           (model-turn (create-message-turn "model" (or model-response-text "Error: No response"))))

      (if model-response-text
          (progn
            (xlg :answer-log  "~&Gemini: ~a" model-response-text)
            (xlg :thinking-log "~s" (list user-turn model-turn))
            (setf conversation-history (append conversation-history (list user-turn model-turn))))
          (progn
            (xlg :answer-log "~&Error on initial turn: ~a~%" (or (jsown:val (jsown:val parsed-json "error") "message") "No text generated or unexpected response structure."))
            (xlg :thinking-log "Parsed JSON: ~s~%" parsed-json)
            (return-from run-gemini-conversation nil)))

      ;; Loop for follow-up turns
      (loop
        (format t "~&~%Enter your next prompt (or type 'quit' to end):~%")
        (let ((next-prompt (read-line)))
          (when (string-equal next-prompt "quit")
            (format t "~&~%Ending conversation.~%")
            (xlg :answer-log "~&~%Ending conversation.")
            (return))

          (let* ((new-user-turn (create-message-turn "user" next-prompt))
                 (updated-history (append conversation-history (list new-user-turn)))
                 (response-stream (make-gemini-api-request updated-history :model model))
                 (parsed-json (parse-gemini-api-response response-stream))
                 (model-response-text (extract-gemini-text parsed-json))
                 (new-model-turn (create-message-turn "model" (or model-response-text "Error: No response"))))

            (if model-response-text
                (progn
                  (xlg :answer-log "~&Gemini: ~a~%" model-response-text)
                  (xlg :thinking-log "~s" (list user-turn model-turn))
                  (setf conversation-history (append updated-history (list new-model-turn))))
                (progn
                  (xlg :answer-log "~&Error on follow-up turn: ~a" (or (jsown:val (jsown:val parsed-json "error") "message") "No text generated or unexpected response structure."))
                  (xlg :thinking-log "Parsed JSON: ~s" parsed-json)
                  (return)))))))
    conversation-history)))

(defun gemini-top ()
  (format nil "first ask is ~s" (format nil "~{~a ~}" (rest sb-ext:*posix-argv*)))
  (run-gemini-conversation (format nil "~{~a ~}" (rest sb-ext:*posix-argv*)) ))

(defun save-core ()
  (format t "building being  ~a~%" (slot-value (asdf:find-system 'gemini-chat) 'asdf:version))
  (sb-ext:save-lisp-and-die "gemini-chat" 
                            :toplevel #'gemini-top
							:save-runtime-options t
							:compression 22
                            :executable t))
