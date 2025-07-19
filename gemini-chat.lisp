;;;; gemini-chat.lisp
;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:gemini-chat)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

;; A default tag to use if none is provided on the command line.
(defparameter *default-conversation-tag* "chat")

;; New special variable for the runtime output file stream
(defparameter *runtime-output-stream* nil
  "Stream for saving conversation answers during a conversation.")

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

(defun create-message-part (text)
  "Creates a Lisp :OBJ representing a 'part' in the Gemini API JSON structure."
  (jsown:new-js ("text" text))) ; Correct: {"text": "..."}

(defun create-message-content (parts)
  "Creates a Lisp :OBJ representing 'content' in the Gemini API JSON structure."
  (jsown:new-js ("parts" parts))) ; Correct: {"parts": [...]}

(defun create-message-turn (role text)
  "Creates a Lisp :OBJ representing a single 'turn' (message) in the Gemini API JSON structure.
   This will be encoded by JSOWN as a JSON object."
  (jsown:new-js
    ("role" role)
    ("parts" (list (create-message-part text)))))

(defun make-gemini-api-request (messages &key (model "gemini-2.5-pro"))
  "Constructs and sends an HTTP POST request to the Gemini API.
   'messages' should be a list of Lisp :OBJ structures, each representing a conversation turn.
   'model' specifies the Gemini model to use (e.g., \"gemini-2.5-pro\", \"gemini-1.5-flash\").
   Returns the response stream if successful."
  (let* ((api-key (get-gemini-api-key))
         (api-url (format nil "https://generativelanguage.googleapis.com/v1beta/models/~a:generateContent?key=~a"
                          model api-key))
         ;; The top-level payload needs to be a JSON object with a "contents" key.
         ;; We must explicitly construct this using jsown:new-js to guarantee :OBJ format
         ;; and string keys for correct casing.
         (json-payload-lisp-object (jsown:new-js ("contents" messages)))

         (json-payload-string (jsown:to-json json-payload-lisp-object))

         (headers '(("Content-Type" . "application/json"))))
    (xlg :thinking-log "~&Making API request to: ~a" api-url)
    (xlg :thinking-log "JSON string being sent: ~a" json-payload-string)
    (handler-case
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
        (error "An unexpected error occurred during the HTTP request: ~a" c)))))

(defun parse-gemini-api-response (response-stream)
  "Parses the JSON response from the Gemini API response stream using jsown.
   Returns the parsed JSON as a Lisp object (jsown's internal :OBJ format)."
  (handler-case
      (let* ((json-string (uiop:slurp-stream-string response-stream))
             (pjs (jsown:parse json-string)))
        (xlg :thinking-log "~&Raw JSON string received: ~a" pjs)
        pjs)
    (error (c)
      (error "Failed to parse JSON response: ~a" c))))


(defun extract-gemini-text (parsed-json)
  "Extracts the generated text from the parsed Gemini API JSON response using jsown accessors.
   Returns the text string or NIL if not found.
   JSOWN represents JSON objects as (:OBJ key1 val1 key2 val2 ...)."
  ;; Add a check for an 'error' key at the top level
  (cond (parsed-json
         (if (jsown:keyp parsed-json "error")
             (progn
               (xlg :thinking-log "~&API returned an error: ~a" (jsown:val parsed-json "error"))
               (xlgt :answer-log "~&API returned an error: ~a" (jsown:val parsed-json "error"))
               ;; Error message will be handled by handle-gemini-turn-response if it goes to answer log
               nil)
             (let* ((candidates (jsown:val parsed-json "candidates"))
                    (first-candidate (car candidates)))
               (when first-candidate
                 (let* ((content (jsown:val first-candidate "content"))
                        (parts (jsown:val content "parts"))
                        (first-part (car parts)))
                   (when first-part
                     (jsown:val first-part "text")))))))
        (t (xlgt :answer-log "No parsed json available: ~a" parsed-json)
           ;; Error message will be handled by handle-gemini-turn-response if it goes to answer log
           "No parsed json available. Why?")))

(defun read-file-content (filepath)
  "Reads the entire content of a file into a string.
   Returns NIL if the file cannot be read."
  (handler-case
      (uiop:read-file-string filepath)
    (file-error (c)
      (xlg :thinking-log "~&Error reading file ~a: ~a" filepath c)
      (xlgt :answer-log "~&Error reading file ~a: ~a" filepath c)
      nil)
    (error (c)
      (xlg :thinking-log "~&An unexpected error occurred while reading file ~a: ~a" filepath c)
      (xlgt :answer-log "~&An unexpected error occurred while reading file ~a: ~a" filepath c)
      nil)))

(defun process-context-files (context-files)
  "Reads content from a list of context files and concatenates them.
   Returns a single string with all file contents, or NIL if no files or errors."
  (when context-files
    (let ((all-content (make-string-output-stream)))
      (format all-content "--- Context Files --~%~%")
      (dolist (file context-files)
        (let ((content (read-file-content file)))
          (if content
              (format all-content "File: ~a~%```~a```~%~%" file content)
              (format t "~&Warning: Could not read context file '~a'. Skipping.~%" file))))
      (format all-content "--- End Context Files --~%~%")
      (get-output-stream-string all-content))))

(defun gemini-chat-loop (conversation-history model tag)
  "Manages the interactive follow-up turns of a Gemini conversation.
   Takes the current conversation history, model, and tag as input.
   Returns the final conversation history."
  (loop
    (xlgt :answer-log "~&~%Enter your next prompt (or type 'quit' to end, ':save <filename>' to save output):")
    (let* ((next-prompt (read-line))
           (first-char (and (> (length next-prompt) 0) (char next-prompt 0))))

      (cond
        ((string-equal next-prompt "quit")
         (format t "~&~%Ending conversation.~%")
         (xlgt :answer-log "~&~%Ending conversation, dude.")
         (when *runtime-output-stream*
           (format *runtime-output-stream* "~&~%Ending conversation.~%")
           (close *runtime-output-stream*)
           (setf *runtime-output-stream* nil))
         (flush-all-log-streams)
         (return conversation-history))

        ;; Handle :save command
        ((and first-char (char= first-char #\:)
              (string-starts-with-p next-prompt ":save "))
         (let* ((parts (split-sequence:split-sequence #\Space next-prompt :remove-empty-subseqs t))
                (filepath (second parts))) ; ":save" is first part, filename is second
           (if filepath
               (handler-case
                   (progn
                     (when *runtime-output-stream* ; Close existing stream if any
                       (format t "~&Closing previous runtime output file.~%")
                       (close *runtime-output-stream*))
                     (setf *runtime-output-stream* (open filepath :direction :output :if-exists :supersede :if-does-not-exist :create))
                     (format t "~&Saving conversation answers to: ~a~%" filepath)
                     (xlgt :answer-log "~&Saving conversation answers to: ~a~%" filepath)
                     (format *runtime-output-stream* "~&--- Runtime Conversation Answers Log Started (~a) ---~%~%" tag))
                 (file-error (c)
                   (format t "~&Error opening file for saving: ~a~%" c)
                   (xlgt :answer-log "~&Error opening file for saving: ~a~%" c)
                   (setf *runtime-output-stream* nil)))
               (progn
                 (format t "~&Usage: :save <filename>. No filename provided.~%")
                 (xlgt :answer-log "~&Usage: :save <filename>. No filename provided.~%"))))
         (continue)) ; Continue to next loop iteration after handling :save

        (t
         (let ((final-user-input next-prompt)) ; This will hold the text to send to Gemini
           ;; Check for file input (starts with '/')
           (if (and (> (length next-prompt) 0) (char= (char next-prompt 0) #\/))
               (let* ((file-path (subseq next-prompt 1))
                      (file-content (read-file-content file-path)))
                 (if file-content
                     (progn
                       (xlgt "~&File '~a' loaded. Enter an additional prompt for Gemini (optional):~%" file-path)
                       (let ((additional-prompt (read-line)))
                         (setf final-user-input (format nil "File content from ~a:~%```~a```~%~%My prompt: ~a"
                                                         file-path file-content additional-prompt))))
                     (progn
                       (format t "~&Error: Could not read file '~a'. Please try again.~%" file-path)
                       (flush-all-log-streams)
                       (continue)))) ; Skip to next loop iteration if file read failed
             ;; Else (not a file input), final-user-input is already set to next-prompt
             )

           (xlg :thinking-log "~&User: ~a" final-user-input)
           (xlgt :answer-log "User: ~a" final-user-input)

           (let* ((new-user-turn (create-message-turn "user" final-user-input))
                  (updated-history (append conversation-history (list new-user-turn)))
                  (response-stream (make-gemini-api-request updated-history :model model))
                  (parsed-json (parse-gemini-api-response response-stream))
                  (model-response-text (extract-gemini-text parsed-json))
                  (new-model-turn (create-message-turn "model" (or model-response-text "Error: No response"))))

             (multiple-value-bind (new-total-history success)
                 (handle-gemini-turn-response model-response-text parsed-json new-user-turn new-model-turn updated-history :turn-type "follow-up turn")
               (if success
                   (setf conversation-history new-total-history)
                   (return conversation-history))))))))))

(defun gemini-conversation (initial-prompt &key (model "gemini-2.5-pro") (tag *default-conversation-tag*))
  "Starts and manages a multi-turn conversation with the Gemini API.
   Takes an initial prompt, then allows for follow-up questions.
   Returns the complete conversation history."
  (with-open-log-files ((:answer-log (format nil "~a-the-answer.log" tag) :ymd)
                        (:thinking-log (format nil "~a-thinking.log" tag) :ymd))
    (let* ((ver (slot-value (asdf:find-system 'gemini-chat) 'asdf:version))
           (vermsg (format nil "begin, gemini-chat version ~a----------------------------------------" ver)))
      (xlg :answer-log vermsg)
      (xlg :thinking-log vermsg)
      )

    (let ((conversation-history nil))

      ;; First turn
      (xlg :thinking-log "~&User: ~a" initial-prompt)
      (xlgt :answer-log "User: ~a" initial-prompt)
      (let* ((user-turn (create-message-turn "user" initial-prompt))
             (response-stream (make-gemini-api-request (list user-turn) :model model))
             (parsed-json (parse-gemini-api-response response-stream))
             (model-response-text (extract-gemini-text parsed-json))
             (model-turn (create-message-turn "model" (or model-response-text "Error: No response"))))

        (multiple-value-bind (new-history success)
            (handle-gemini-turn-response model-response-text parsed-json user-turn model-turn conversation-history :turn-type "initial turn")
          (if success
              (setf conversation-history new-history)
              (return-from gemini-conversation nil))))

      ;; If the initial turn was successful, proceed to the interactive loop
      (when conversation-history
        ;; Pass the tag to the chat loop so it can derive output file names
        (setf conversation-history (gemini-chat-loop conversation-history model tag)))
      conversation-history)))

(defun handle-gemini-turn-response (model-response-text parsed-json user-turn model-turn conversation-history &key (turn-type "turn"))
  "Handles the processing of a Gemini API response for a single turn,
   logging the output and updating conversation history.
   Returns (values updated-conversation-history t) on success,
   or (values nil nil) on error, signaling the need for the caller to stop."
  (if model-response-text
      (progn
        (xlgt :answer-log "~&Gemini: ~a" model-response-text)
        (when *runtime-output-stream*
          (format *runtime-output-stream* "~&Gemini: ~a~%" model-response-text)
          (finish-output *runtime-output-stream*)) ; Ensure content is written immediately
        ;; Log the specific user and model turns involved in this step
        (xlg :thinking-log "Turns processed: ~s" (list user-turn model-turn))
        (flush-all-log-streams)
        (values (append conversation-history (list user-turn model-turn)) t))
      (progn
        (xlgt :answer-log "~&Error on ~a: ~a" turn-type
              (or (jsown:val (jsown:val parsed-json "error") "message") "No text generated or unexpected response structure."))
        (when *runtime-output-stream* ; Log API errors that prevent text generation, as they are part of the answer stream
          (format *runtime-output-stream* "~&Error on ~a: ~a~%" turn-type
                  (or (jsown:val (jsown:val parsed-json "error") "message") "No text generated or unexpected response structure."))
          (finish-output *runtime-output-stream*))
        (xlg :thinking-log "Parsed JSON for ~a: ~s" turn-type parsed-json)
        (flush-all-log-streams)
        (values nil nil))))

;; Utility to check if a string starts with a prefix
(defun string-starts-with-p (string prefix)
  (and (>= (length string) (length prefix))
       (string= string prefix :end1 (length prefix))))

;; Updated gemini-top function for robust command-line parsing and log tag derivation
(defun gemini-top ()
  (let* ((all-args sb-ext:*posix-argv*) ; Corrected: Accessing the variable directly
         (cmd-args (rest all-args)) ; Arguments after the executable name (e.g., "gemini-chat")
         (tag *default-conversation-tag*) ; Start with default tag
         (context-files nil)             ; New: List to store context file paths
         (remaining-args nil)            ; These are the arguments that form the prompt or file path
         (potential-file-path nil)       ; File path if detected as first *remaining-arg*
         (initial-prompt-parts nil))     ; Parts of the text prompt

    ;; Parse command-line arguments for tag, context files, and initial prompt/file
    (loop while cmd-args do
      (let ((arg (pop cmd-args)))
        (cond
          ((string= arg "-c")
           (unless cmd-args
             (error "Error: -c option requires a file path."))
           (push (pop cmd-args) context-files))
          ((string= arg "--context")
           (unless cmd-args
             (error "Error: --context option requires a file path."))
           (push (pop cmd-args) context-files))
          (t
           ;; Once we hit a non-option arg, the rest are part of the prompt or a file
           (push arg remaining-args)
           (setf remaining-args (nconc (nreverse remaining-args) cmd-args)) ; Add remaining args
           (setf cmd-args nil))))) ; Clear cmd-args to exit loop

    (setf context-files (nreverse context-files)) ; Restore original order

    ;; 1. Determine the conversation tag and the initial set of "remaining arguments"
    ;;    If the first non-option cmd-arg is NOT a file, it's considered the tag.
    ;;    Otherwise, all non-option cmd-args are "remaining-args" and the tag remains default for now.
    (if (and remaining-args
             (not (uiop:file-exists-p (first remaining-args))))
        (progn
          (setf tag (first remaining-args))
          (setf remaining-args (rest remaining-args)))
        ;; If no explicit tag and the first arg is a file, tag will be derived from it later.
        ;; If no explicit tag and no file, tag remains default "chat".
        )

    (format t "Conversation tag is: [~a]~%" tag)

    ;; Process context files
    (let ((context-content (process-context-files context-files)))
      (when context-content
        (format t "~&Loaded content from context files.~%")
        (xlgt :answer-log "~&Loaded content from context files.~%")
        (setf initial-prompt-parts (cons context-content initial-prompt-parts)))

      ;; 2. Process the 'remaining-args' to determine the actual initial prompt string for Gemini
      (cond
        ;; Case A: No arguments left after tag (e.g., "gemini-chat" or "gemini-chat mytag")
        ((null remaining-args)
         (format t "~&Please enter your initial question or file path (e.g., /path/to/my/file.txt):~%")
         (let* ((user-input (read-line))
                (parsed-input (if (and (> (length user-input) 0) (char= (char user-input 0) #\/))
                                  (subseq user-input 1)
                                  user-input)))
           (if (uiop:file-exists-p parsed-input)
               ;; User entered a file path interactively
               (let ((file-content (read-file-content parsed-input)))
                 ;; Derive tag from file if still default
                 (when (string-equal tag *default-conversation-tag*)
                   (setf tag (pathname-name (parse-namestring parsed-input))))
                 (if file-content
                     (progn
                       (format t "~&File '~a' loaded. Now, enter your instructions/question for Gemini about this file:~%" parsed-input)
                       (let* ((user-instructions (read-line))
                              (initial-prompt (format nil "File content from ~a:~%```~a```~%~%My instructions: ~a~%~{~a~}"
                                                      parsed-input file-content user-instructions initial-prompt-parts)))
                         (gemini-conversation initial-prompt :tag tag)))
                     (progn
                       (format t "~&Error: Could not read file '~a'. Please enter your initial question for Gemini directly:~%" parsed-input)
                       (gemini-conversation (format nil "~a~%~{~a~}" (read-line) initial-prompt-parts) :tag tag))))
               ;; User entered a direct prompt interactively
               (gemini-conversation (format nil "~a~%~{~a~}" user-input initial-prompt-parts) :tag tag))))

        ;; Case B: First of 'remaining-args' is an existing file (e.g., "gemini-chat /path/file.txt analyze this"
        ;;         OR "gemini-chat mytag /path/file.txt analyze this")
        ((uiop:file-exists-p (first remaining-args))
         (setf potential-file-path (first remaining-args))
         (setf initial-prompt-parts (append initial-prompt-parts (rest remaining-args))) ; The rest are user instructions

         ;; If tag is still default (meaning first actual command-line arg was a file, no explicit tag)
         ;; then derive tag from the filename.
         (when (string-equal tag *default-conversation-tag*)
             (setf tag (pathname-name (parse-namestring potential-file-path))))

         (let ((file-content (read-file-content potential-file-path)))
           (if file-content
               (let ((user-instructions (string-trim '(#\Space #\Newline #\Tab) (format nil "~{~a ~}" initial-prompt-parts))))
                 (gemini-conversation
                  (format nil "File content from ~a:~%```~a```~%~%My instructions: ~a"
                          potential-file-path file-content user-instructions)
                  :tag tag))
               (progn
                 (format t "~&Error: Could not read file '~a'. Proceeding with prompt only.~%" potential-file-path)
                 (gemini-conversation (string-trim '(#\Space #\Newline #\Tab) (format nil "~{~a ~}" initial-prompt-parts)) :tag tag)))))

        ;; Case C: No file path detected, treat all 'remaining-args' as the direct prompt
        (t
         (setf initial-prompt-parts (append initial-prompt-parts remaining-args))
         (gemini-conversation (string-trim '(#\Space #\Newline #\Tab) (format nil "~{~a ~}" initial-prompt-parts)) :tag tag))))))


(defun save-core ()
  (format t "Building gemini-chat version ~a~%" (slot-value (asdf:find-system 'gemini-chat) 'asdf:version))
  (sb-ext:save-lisp-and-die "gemini-chat"
                            :toplevel #'gemini-top
                            :save-runtime-options t
                            :compression 22
                            :executable t))
