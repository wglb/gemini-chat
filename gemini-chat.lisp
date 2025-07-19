;;;; gemini-chat.lisp
;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:gemini-chat)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

;; A default tag to use if none is provided on the command line.
(defparameter *d-tag* "chat")

;; New special variable for the runtime output file stream
(defparameter *run-out-s* nil
  "Stream for saving conversation answers during a conversation.")

(defun get-key ()
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

(defun msg-part (text)
  "Creates a Lisp :OBJ representing a 'part' in the Gemini API JSON structure."
  (jsown:new-js ("text" text)))

(defun msg-content (parts)
  "Creates a Lisp :OBJ representing 'content' in the Gemini API JSON structure."
  (jsown:new-js ("parts" parts)))

(defun msg-turn (role text)
  "Creates a Lisp :OBJ representing a single 'turn' (message) in the Gemini API JSON structure.
   This will be encoded by JSOWN as a JSON object."
  (jsown:new-js
    ("role" role)
    ("parts" (list (msg-part text)))))

(defun api-req (msgs &key (model "gemini-2.5-pro"))
  "Constructs and sends an HTTP POST request to the Gemini API.
   'msgs' should be a list of Lisp :OBJ structures, each representing a conversation turn.
   'model' specifies the Gemini model to use (e.g., \"gemini-2.5-pro\", \"gemini-1.5-flash\").
   Returns the response stream if successful."
  (let* ((api-key (get-key))
         (api-url (format nil "https://generativelanguage.googleapis.com/v1beta/models/~a:generateContent?key=~a"
                          model api-key))
         (json-payload-lisp-object (jsown:new-js ("contents" msgs)))
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

(defun parse-api-resp (resp-stream)
  "Parses the JSON response from the Gemini API response stream using jsown.
   Returns the parsed JSON as a Lisp object (jsown's internal :OBJ format)."
  (handler-case
      (let* ((json-string (uiop:slurp-stream-string resp-stream))
             (pjs (jsown:parse json-string)))
        (xlg :thinking-log "~&Raw JSON string received: ~a" pjs)
        pjs)
    (error (c)
      (error "Failed to parse JSON response: ~a" c))))


(defun extract-txt (parsed-json)
  "Extracts the generated text from the parsed Gemini API JSON response using jsown accessors.
   Returns the text string or NIL if not found."
  (cond (parsed-json
         (if (jsown:keyp parsed-json "error")
             (progn
               (xlg :thinking-log "~&API returned an error: ~a" (jsown:val parsed-json "error"))
               (xlgt :answer-log "~&API returned an error: ~a" (jsown:val parsed-json "error"))
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
           "No parsed json available. Why?")))

(defun read-file (fpath)
  "Reads the entire content of a file into a string.
   Returns NIL if the file cannot be read."
  (handler-case
      (uiop:read-file-string fpath)
    (file-error (c)
      (xlg :thinking-log "~&Error reading file ~a: ~a" fpath c)
      (xlgt :answer-log "~&Error reading file ~a: ~a" fpath c)
      nil)
    (error (c)
      (xlg :thinking-log "~&An unexpected error occurred while reading file ~a: ~a" fpath c)
      (xlgt :answer-log "~&An unexpected error occurred while reading file ~a: ~a" fpath c)
      nil)))

(defun proc-ctx-files (ctx-files)
  "Reads content from a list of context files and concatenates them.
   Returns a single string with all file contents, or NIL if no files or errors."
  (when ctx-files
    (let ((all-content (make-string-output-stream)))
      (format all-content "--- Context Files --~%~%")
      (dolist (file ctx-files)
        (let ((content (read-file file)))
          (if content
              (format all-content "File: ~a~%```~a```~%~%" file content)
              (format t "~&Warning: Could not read context file '~a'. Skipping.~%" file))))
      (format all-content "--- End Context Files --~%~%")
      (get-output-stream-string all-content))))

(defun str-starts-with-p (str prefix)
  "Checks if STR starts with PREFIX."
  (and (>= (length str) (length prefix))
       (string= str prefix :end1 (length prefix))))

(defun save-cmd (cmd-str tag)
  "Implements the ':save <filename>' command. Opens a new runtime output stream
   or closes the existing one."
  (let* ((parts (split-sequence:split-sequence #\Space cmd-str :remove-empty-subseqs t))
         (fpath (second parts)))
    (if fpath
        (handler-case
            (progn
              (when *run-out-s* ; Close existing stream if any
                (format t "~&Closing previous runtime output file.~%")
                (close *run-out-s*))
              (setf *run-out-s* (open fpath :direction :output :if-exists :supersede :if-does-not-exist :create))
              (format t "~&Saving conversation answers to: ~a~%" fpath)
              (xlgt :answer-log "~&Saving conversation answers to: ~a~%" fpath)
              (format *run-out-s* "~&--- Runtime Conversation Answers Log Started (~a) ---~%~%" tag))
          (file-error (c)
            (format t "~&Error opening file for saving: ~a~%" c)
            (xlgt :answer-log "~&Error opening file for saving: ~a~%" c)
            (setf *run-out-s* nil)))
        (progn
          (format t "~&Usage: :save <filename>. No filename provided.~%")
          (xlgt :answer-log "~&Usage: :save <filename>. No filename provided.~%")))))

(defun proc-usr-prompt-file (raw-prompt)
  "Processes a raw user prompt, checking for leading '/' for file paths.
   If a file path is found and readable, reads content and prompts for more instructions.
   Returns the final prompt string to send to Gemini, or (values NIL :file-error) on failure."
  (if (and (> (length raw-prompt) 0) (char= (char raw-prompt 0) #\/))
      (let* ((fpath (subseq raw-prompt 1))
             (fcontent (read-file fpath)))
        (if fcontent
            (progn
              (xlgt "~&File '~a' loaded. Enter an additional prompt for Gemini (optional):~%" fpath)
              (let ((add-prompt (read-line)))
                (format nil "File content from ~a:~%```~a```~%~%My prompt: ~a"
                        fpath fcontent add-prompt)))
            (progn
              (format t "~&Error: Could not read file '~a'. Please try again.~%" fpath)
              (flush-all-log-streams)
              (values nil :file-error)))) ; Signal an error to the caller
      raw-prompt)) ; Not a file, return as is

(defun gem-interact (u-prompt conv-hist model)
  "Sends a user prompt to Gemini, gets a response, and updates history.
   Returns (values new-conversation-history success-p)."
  (xlg :thinking-log "~&User: ~a" u-prompt)
  (xlgt :answer-log "User: ~a" u-prompt)

  (let* ((new-u-turn (msg-turn "user" u-prompt))
         (upd-hist (append conv-hist (list new-u-turn)))
         (resp-stream (api-req upd-hist :model model))
         (parsed-json (parse-api-resp resp-stream))
         (model-resp-txt (extract-txt parsed-json))
         (new-m-turn (msg-turn "model" (or model-resp-txt "Error: No response"))))

    (gem-turn-resp model-resp-txt parsed-json new-u-turn new-m-turn upd-hist :turn-type "follow-up turn")))

(defun gem-turn-resp (model-resp-txt parsed-json u-turn m-turn conv-hist &key (turn-type "turn"))
  "Processes a Gemini API response for a single turn,
   logging the output and updating conversation history.
   Returns (values updated-conversation-history t) on success,
   or (values nil nil) on error, signaling the need for the caller to stop."
  (if model-resp-txt
      (progn
        (xlgt :answer-log "~&Gemini: ~a" model-resp-txt)
        (when *run-out-s*
          (format *run-out-s* "~&Gemini: ~a~%" model-resp-txt)
          (finish-output *run-out-s*)) ; Ensure content is written immediately
        ;; Log the specific user and model turns involved in this step
        (xlg :thinking-log "Turns processed: ~s" (list u-turn m-turn))
        (flush-all-log-streams)
        (values (append conv-hist (list u-turn m-turn)) t))
      (progn
        (xlgt :answer-log "~&Error on ~a: ~a" turn-type
              (or (jsown:val (jsown:val parsed-json "error") "message") "No text generated or unexpected response structure."))
        (when *run-out-s* ; Log API errors that prevent text generation, as they are part of the answer stream
          (format *run-out-s* "~&Error on ~a: ~a~%" turn-type
                  (or (jsown:val (jsown:val parsed-json "error") "message") "No text generated or unexpected response structure."))
          (finish-output *run-out-s*))
        (xlg :thinking-log "Parsed JSON for ~a: ~s" turn-type parsed-json)
        (flush-all-log-streams)
        (values nil nil))))

;; Helper functions for gemini-chat-loop refactoring
(defun quit-cmd ()
  "Performs cleanup when the 'quit' command is issued."
  (format t "~&~%Ending conversation.~%")
  (xlgt :answer-log "~&~%Ending conversation, dude.")
  (when *run-out-s*
    (format *run-out-s* "~&~%Ending conversation.~%")
    (close *run-out-s*)
    (setf *run-out-s* nil))
  (flush-all-log-streams))

(defun read-usr-cmd (raw-in)
  "Parses raw user input and determines the command type and associated data.
   Returns (values command-keyword data) or (values :error nil) if a file read fails."
  (cond
    ((string-equal raw-in "quit")
     (values :quit nil))
    ((str-starts-with-p raw-in ":save ")
     (values :save raw-in))
    (t
     (multiple-value-bind (proc-prompt err-type)
         (proc-usr-prompt-file raw-in)
       (if err-type
           (values :error nil)
           (values :prompt proc-prompt))))))

(defun proc-send-prompt (proc-prompt conv-hist model)
  "Processes a valid user prompt (which may include file content) and interacts with Gemini.
   Returns (values new-history success-p)."
  (gem-interact proc-prompt conv-hist model))


(defun chat-loop (conv-hist model tag)
  "Manages the interactive follow-up turns of a Gemini conversation.
   Takes the current conversation history, model, and tag as input.
   Returns the final conversation history."
  (loop
    (xlgt :answer-log "~&~%Enter your next prompt (or type 'quit' to end, ':save <filename>' to save output):")
    (let ((raw-usr-in (read-line)))

      (multiple-value-bind (cmd-type cmd-data)
          (read-usr-cmd raw-usr-in)
        (ecase cmd-type
          (:quit
           (quit-cmd)
           (return conv-hist)) ; Exit loop and return history
          (:save
           (save-cmd cmd-data tag) ; cmd-data is the full ":save <filename>" string
           (continue)) ; Continue to next loop iteration
          (:prompt
           (multiple-value-bind (new-total-hist success)
               (proc-send-prompt cmd-data conv-hist model)
             (if success
                 (setf conv-hist new-total-hist)
                 (return conv-hist)))) ; Return history on error
          (:error
           (format t "~&Skipping turn due to input error.~%")
           (continue)))))))


(defun gem-conv (init-prompt &key (model "gemini-2.5-pro") (tag *d-tag*))
  "Starts and manages a multi-turn conversation with the Gemini API.
   Takes an initial prompt, then allows for follow-up questions.
   Returns the complete conversation history."
  (with-open-log-files ((:answer-log (format nil "~a-the-answer.log" tag) :ymd)
                        (:thinking-log (format nil "~a-thinking.log" tag) :ymd))
    (let* ((ver (slot-value (asdf:find-system 'gemini-chat) 'asdf:version))
           (vermsg (format nil "begin, gemini-chat version ~a----------------------------------------" ver)))
      (xlg :answer-log vermsg)
      (xlg :thinking-log vermsg))

    (let ((conv-hist nil))

      ;; First turn (initial prompt)
      (multiple-value-bind (new-hist success)
          (gem-interact init-prompt conv-hist model)
        (if success
            (setf conv-hist new-hist)
            (return-from gem-conv nil)))

      ;; If the initial turn was successful, proceed to the interactive loop
      (when conv-hist
        (setf conv-hist (chat-loop conv-hist model tag)))
      conv-hist)))


;; --- New functions for refactored top ---

(defun cli-args (args)
  "Processes command-line arguments.
   Returns (values explicit-tag context-files prompt-arg-list).
   'explicit-tag' is the tag provided directly on the command line (e.g., 'my-chat-tag').
   'context-files' is a list of file paths specified with -c or --context.
   'prompt-arg-list' is a list of strings forming the raw initial prompt,
   which might include a file path as its first element if provided directly."
  (let ((expl-tag nil)
        (ctx-files nil)
        (prompt-arg-l nil)
        (rem-args args))
    (loop while rem-args do
      (let ((arg (pop rem-args)))
        (cond
          ((or (string= arg "-c") (string= arg "--context"))
           (unless rem-args
             (error "Error: ~a option requires a file path." arg))
           (push (pop rem-args) ctx-files))
          (t
           (if (and (not expl-tag)
                    (not (uiop:file-exists-p arg))
                    (not (str-starts-with-p arg "/")))
               (setf expl-tag arg)
               (push arg prompt-arg-l))
           (setf prompt-arg-l (nconc (nreverse prompt-arg-l) rem-args))
           (setf rem-args nil)))))
    (values expl-tag (nreverse ctx-files) (nreverse prompt-arg-l))))


(defun initial-prompt (prompt-comps ctx-content tag-from-cli)
  "Assembles the final initial prompt string for Gemini and determines the conversation tag.
   'prompt-comps' are raw parts from CLI.
   'ctx-content' is the combined text from context files.
   'tag-from-cli' is the tag explicitly provided via CLI, or *d-tag*.
   Returns (values final-prompt-string new-tag) or (values nil nil) on error."
  (let ((final-prompt nil)
        (deriv-tag tag-from-cli))

    (cond
      ;; Case 1: No command-line prompt components, prompt the user interactively
      ((null prompt-comps)
       (format t "~&Please enter your initial question or file path (e.g., /path/to/my/file.txt):~%")
       (let* ((usr-in (read-line)))
         (multiple-value-bind (proc-in err-type)
             (proc-usr-prompt-file usr-in)
           (if err-type
               (return-from initial-prompt (values nil nil))
               (progn
                 (when (and (string-equal deriv-tag *d-tag*)
                            (char= (char usr-in 0) #\/))
                   (let ((pname (parse-namestring (subseq usr-in 1))))
                     (when (pathname-name pname)
                       (setf deriv-tag (pathname-name pname)))))
                 (setf final-prompt (format nil "~a~%~a" proc-in (or ctx-content ""))))))))

      ;; Case 2: First component is an existing file path (from CLI)
      ((uiop:file-exists-p (first prompt-comps))
       (let* ((fpath (first prompt-comps))
              (fcontent (read-file fpath)))
         (if fcontent
             (progn
               (when (string-equal deriv-tag *d-tag*)
                   (setf deriv-tag (pathname-name (parse-namestring fpath))))
               (let ((usr-inst (string-trim '(#\Space #\Newline #\Tab) (format nil "~{~a ~}" (rest prompt-comps)))))
                 (setf final-prompt
                       (format nil "~a~%File content from ~a:~%```~a```~%~%My instructions: ~a"
                               (or ctx-content "") fpath fcontent usr-inst))))
             (progn
               (format t "~&Error: Could not read file '~a'. Proceeding with prompt only.~%" fpath)
               (setf final-prompt (string-trim '(#\Space #\Newline #\Tab) (format nil "~a~%~{~a ~}" (or ctx-content "") (rest prompt-comps))))))))

      ;; Case 3: All components are direct prompt text (from CLI)
      (t
       (setf final-prompt (string-trim '(#\Space #\Newline #\Tab) (format nil "~a~%~{~a ~}" (or ctx-content "") prompt-comps)))))

    (values final-prompt deriv-tag)))


(defun start-chat (init-prompt tag &key (model "gemini-2.5-pro"))
  "Initiates the Gemini conversation with the assembled initial prompt and tag."
  (format t "Conversation tag is: [~a]~%" tag)
  (gem-conv init-prompt :model model :tag tag))


(defun top ()
  "Main entry point for the gemini-chat application.
   Handles command-line argument parsing, initial prompt assembly, and starting the chat session."
  (let* ((all-args sb-ext:*posix-argv*)
         (cmd-args (rest all-args)))

    (multiple-value-bind (expl-tag ctx-files prompt-comps)
        (cli-args cmd-args)

      (let* ((actual-tag (or expl-tag *d-tag*))
             (ctx-content (proc-ctx-files ctx-files)))

        (multiple-value-bind (f-prompt d-tag)
            (initial-prompt prompt-comps ctx-content actual-tag)
          (unless f-prompt
            (format t "~&Initial prompt generation failed. Exiting.~%")
            (return-from top nil))

          (start-chat f-prompt (or d-tag actual-tag)))))))

(defun save-core ()
  "Saves the current Lisp image as an executable."
  (format t "Building gemini-chat version ~a~%" (slot-value (asdf:find-system 'gemini-chat) 'asdf:version))
  (sb-ext:save-lisp-and-die "gemini-chat"
                            :toplevel #'top
                            :save-runtime-options t
                            ;; :compression 22
                            :executable t))
