;;;; gemini-chat.lisp
;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:gemini-chat)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

;; A default tag to use if none is provided on the command line.
(defparameter *d-tag* "chat")

;; New special variable for the runtime output file stream
(defparameter *run-out-s* nil
  "Stream for saving conversation answers during a conversation.")

;; Function to retrieve the current version
(defun get-version ()
  (slot-value (asdf:find-system 'gemini-chat) 'asdf:version))

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

;; --- Define Flags using com.google.flag ---

(define-flag *context* ; <-- CHANGED: Flag name now uses asterisk convention
  :help "Path to a context file. Can be specified multiple times. Example: --context file1.txt,file2.txt"
  :type list
  :parser string-identity-parser
  :selector "context"
  :default-value nil)

(define-flag *save* ; <-- CHANGED: Flag name now uses asterisk convention
  :help "File to save Gemini's responses to. Responses will be appended. Example: --save conversation.log"
  :type string
  :selector "save"
  :default-value "")

(define-flag *tag* ; <-- CHANGED: Flag name now uses asterisk convention
  :help "A unique tag for the conversation logs (default: chat). Example: --tag my-session"
  :type string
  :selector "tag"
  :default-value "chat")

(define-flag *input-file* ; <-- CHANGED: Flag name now uses asterisk convention
  :help "Path to a primary input file whose content will be sent to Gemini with your prompt. Example: --input-file my-code.lisp"
  :type string
  :selector "input_file"
  :default-value "")

(define-flag *help-is*
  :help "Show this help message and exit." ; Corrected help string for --help
  :type boolean
  :selector "help"
  :default-value nil)

(defun s/z (str)
  (zerop (length str)))

(defun s/nz (str)
  (plusp (length str)))

(defun print-help ()
  "Prints the command-line help message and example usage."
  (format t "~&gemini-chat version ~a~%~%" (get-version)) ; Added version to help
  (format t "Usage: ./gemini-chat [options] [tag] [initial_prompt | /path/to/file.txt]~%~%")
  (format t "Options:~%")
  (format t "  -h, --help               Show this help message and exit.~%")
  (format t "  -c, --context <file1,file2,...> Specify a comma-separated list of files to be included as initial context.~%~%") ; <-- UPDATED THIS LINE
  (format t "  -s, --save <file>        File to save Gemini's responses to (appends).~%") ; Added more explicit help for -s
  (format t "  -t, --tag <tag>          A unique tag for conversation logs (default: 'chat').~%") ; Added help for -t
  (format t "  -f, --input-file <file>  Path to a primary input file whose content will be sent with your prompt.~%~%") ; Added help for -f
  (format t "Interactive Commands (during chat loop):~%")
  (format t "  :save <filename>         Start or change saving model responses to the specified file.~%")
  (format t "  quit                     End the conversation.~%~%")
  (format t "Initial Prompt Options:~%")
  (format t "  If no initial prompt or file is given, the program will prompt you interactively.~%")
  (format t "  If the first argument is a path starting with '/', the file content will be loaded as the initial input.~%")
  (format t "  Otherwise, all subsequent arguments are treated as the initial prompt text.~%~%")

  (format t "---~%~%")
  (format t "## Example Flow:~%~%")
  (format t "To use the `gemini-chat` program with an input file, a context file, a defined output file, and chat input that references the input file, you'd use a command like this:~%~%")
  (format t "```bash~%")
  (format t "./gemini-chat -c your_context_file.txt,:~~/another_context.md :save my_output.txt /path/to/your_input_file.txt \"Please summarize the content of the attached file and then answer my questions.\"~%")
  (format t "```~%~%")
  (format t "Let's break down the components of that command:~%~%")
  (format t "* **`./gemini-chat`**: This is how you'd typically execute the compiled program.~%")
  (format t "* **`-c your_context_file.txt,:~~/another_context.md`** (or `--context your_context_file.txt,:~~/another_context.md`):~%")
  (format t "    * `:-c` or `--context` is the option to specify a **context file(s)**.~%")
  (format t "    * `your_context_file.txt,:~~/another_context.md` is a comma-separated list of paths to files whose content you want to provide as additional context to the Gemini model before it processes your main prompt. This is useful for providing background information, specific guidelines, or data that isn't directly part of your immediate query but should influence the model's response.~%")
  (format t "* **`:save my_output.txt`**:~%")
  (format t "    * `::save` is a special command *within* `gemini-chat` that tells it to direct the model's responses to a file.~%")
  (format t "    * `my_output.txt` is the name of the file where the conversation's output will be saved. The program will open this file and append Gemini's responses to it.~%")
  (format t "* **`/path/to/your_input_file.txt`**:~%")
  (format t "    * When the first non-option argument on the command line starts with a `/` (indicating a file path), `gemini-chat` will read this file's content. This becomes the primary 'input file' for the current turn.~%")
  (format t "    * The `gemini-chat` program will then prompt you for an **additional prompt** that will accompany the file content.~%")
  (format t "* **`\"Please summarize the content of the attached file and then answer my questions.\"`**:~%")
  (format t "    * This is the **chat input** you'd type after `gemini-chat` prompts you, following the reading of `/path/to/your_input_file.txt`.~%")
  (format t "    * This is where you provide instructions or questions *related to the content of the input file*.~%~%")
  (format t "---~%~%")
  (format t "### Example Flow:~%~%")
  (format t "1.  You run the command:~%    ```bash~%")
  (format t "    ./gemini-chat -c my_project_docs.txt :save session_log.txt /home/bill/data/quarterly_report.csv~%")
  (format t "    ```~%")
  (format t "2.  `gemini-chat` processes `my_project_docs.txt` and `another_doc.txt` as context.~%")
  (format t "3.  It sets up `session_log.txt` to save the output.~%")
  (format t "4.  It reads `/home/bill/data/quarterly_report.csv`.~%")
  (format t "5.  You then see a prompt like:~%    ```~%")
  (format t "    File '/home/bill/data/quarterly_report.csv' loaded. Enter an additional prompt for Gemini (optional):~%")
  (format t "    ```~%")
  (format t "6.  You would type:~%    ```~%")
  (format t "    Based on the report, what were the key revenue drivers and what challenges are highlighted?~%")
  (format t "    ```~%")
  (format t "7.  `gemini-chat` combines the context from `my_project_docs.txt` and `another_doc.txt`, the content of `quarterly_report.csv`, and your \"key revenue drivers\" prompt, sends it to Gemini, and logs the response to `session_log.txt` (and displays it to you).~%")
  (format t "---~%~%")
  (finish-output))

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
          (format t "~&Usage: :save <filename>. No filename provided fpath ~s.~%" fpath)
          (setf *run-out-s* nil)))))

(defun proc-usr-prompt-file (fpath)
  "Reads content from a specified file path.
   Returns the file content string, or (values NIL :file-error) on failure."
  (let ((fcontent (read-file fpath)))
    (if fcontent
        fcontent
        (progn
          (format t "~&Error: Could not read file '~a'. Please try again.~%" fpath)
          (flush-all-log-streams)
          (values nil :file-error)))))

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

;; Helper functions for chat-loop refactoring
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
   Returns (values command-keyword data)."
  (cond
    ((string-equal raw-in "quit")
     (values :quit nil))
    ((str-starts-with-p raw-in ":save ")
     (values :save raw-in))
    (t
     (values :prompt raw-in)))) ; Now returns direct prompt or file path to be processed by initial-prompt

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
               (proc-send-prompt cmd-data conv-hist model) ; cmd-data is already the prompt string
             (if success
                 (setf conv-hist new-total-hist)
                 (return conv-hist)))) ; Return history on error
          (:error ; This case should not be reached with the new proc-usr-prompt-file
           (format t "~&Skipping turn due to input error.~%")
           (continue)))))))


(defun gem-conv (init-prompt &key (model "gemini-2.5-pro") (tag *d-tag*))
  "Starts and manages a multi-turn conversation with the Gemini API.
   Takes an initial prompt, then allows for follow-up questions.
   Returns the complete conversation history."
  (with-open-log-files ((:answer-log (format nil "~a-the-answer.log" tag) :ymd)
                        (:thinking-log (format nil "~a-thinking.log" tag) :ymd))
    (let* ((ver (get-version))
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
;; --- Define Flags using com.google.flag ---
;; com.google.flag handles --help automatically.
;; The `:help` string for each flag will appear in the generated help message.


;; --- Define Flags using com.google.flag ---

;; Add this helper function somewhere in your gemini-chat.lisp,
;; ideally near other utility functions or at the top level.
(defun string-identity-parser (s)
  "A parser function for com.google.flag that simply returns the string itself
   and a success boolean T. Used for list flags where each element is a string."
  (let ((full-l nil))
    (mapc #'(lambda (k)
              (format t "sip: k is ~s~%" k)
              (push k full-l))
          (split-sequence:split-sequence #\, s))
    (values (reverse full-l) t)))

;; --- Functions for refactored top ---

(defun initial-prompt (ctx-content)
  "Assembles the final initial prompt string for Gemini based on parsed flags and positional arguments.
   Returns (values final-prompt-string new-tag) or (values nil nil) on error."
  (let* ((initial-prompt-text (command-line)) ; <-- CHANGED from (arguments)
         (final-prompt nil)
         (file-content nil)
         (prompt-from-cli (string-trim '(#\Space #\Newline #\Tab) (format nil "~{~a ~}" initial-prompt-text))))

    (when (s/nz *input-file*)
      (multiple-value-setq (file-content)
        (proc-usr-prompt-file *input-file*))
      (unless file-content
        (return-from initial-prompt (values nil nil)))) ; Error reading file

    (cond
      ;; Case 1: No initial prompt via CLI (neither -f nor non-option args), prompt interactively
      ((and (s/nz *input-file*) (s/nz prompt-from-cli))
       (format t "~&Please enter your initial question (or type 'quit' to end):~%")
       (let ((usr-in (read-line)))
         (when (string-equal usr-in "quit")
           (return-from initial-prompt (values nil nil)))
         (setf final-prompt (format nil "~a~%~a" (or ctx-content "") usr-in))))

      ;; Case 2: Input file provided, combine its content with the prompt
      ((s/nz *input-file*)
       (setf final-prompt
             (format nil "~a~%File content from ~a:~%```~a```~%~%My prompt: ~a"
                     (or ctx-content "") *input-file* file-content prompt-from-cli)))

      ;; Case 3: Only direct prompt text provided
      (t
       (setf final-prompt (format nil "~a~%~a" (or ctx-content "") prompt-from-cli))))

    ;; Tag is now explicit from *tag* (the special variable)
    (values final-prompt *tag*))) ; <-- CHANGED from (flag-value 'tag) ; <-- CHANGED from (flag-value 'tag)

(defun start-chat (init-prompt tag &key (model "gemini-2.5-pro"))
  "Initiates the Gemini conversation with the assembled initial prompt and tag."
  (format t "Conversation tag is: [~a]~%" tag)
  (gem-conv init-prompt :model model :tag tag))

(defun run-chat (&rest raw-args)
  "Main entry point for the gemini-chat application.
   Handles argument parsing, initial prompt assembly, and starting the chat session."
  (let* ((ver (get-version))
         (cmd-args
           (if (and (consp raw-args) (listp (car raw-args)))
               (car raw-args)
               raw-args)))

    (format t "~&gemini-chat version ~a~%" ver)

    (handler-case
        (parse-command-line  cmd-args)
      (error (c)
        (format t "~&Error parsing arguments: ~a~%" c)
        (format t "~&Run with `--help` for usage information.~%")
        (uiop:quit 1)))

    ;; Access flag values directly from their special variables
    (cond (*help-is*
           (print-help))
          (t (let* ((actual-tag *tag*)     ; <-- CHANGED from (flag-value 'tag)
                    (ctx-content (proc-ctx-files *context*)))
               ;; Handle initial save command if -s was provided
               (when (s/nz *save*)
                 (save-cmd (format nil ":save ~a" *save*) actual-tag))

               (multiple-value-bind (f-prompt d-tag)
                   (initial-prompt ctx-content) ; Initial prompt now gets context content directly
                 (unless f-prompt
                   (format t "~&Initial prompt generation failed or user quit. Exiting.~%")
                   (return-from run-chat nil))

                 ;; Assuming 'start-chat' is the orchestrator for 'gem-conv'
                 ;; If start-chat is not defined, you might want to call gem-conv directly here.
                 ;; For now, I'll keep the call to start-chat as is, but know it might need adjustment.
                 (start-chat f-prompt (or d-tag actual-tag))))))))

(defun top ()
  "Toplevel function for the compiled gemini-chat executable.
   It retrieves arguments from sb-ext:*posix-argv* and passes them to run-chat."
  ;; com.google.flag:parse-command-line without :argv defaults to sb-ext:*posix-argv*
  ;; However, run-chat expects a list of strings, so pass (rest sb-ext:*posix-argv*)
  (format t "Top: we have command line args of ~s~%" sb-ext:*posix-argv*)
  (run-chat (rest sb-ext:*posix-argv*)))

(defun save-core ()
  "Saves the current Lisp image as an executable."
  (format t "Building gemini-chat version ~a~%" (get-version))
  (sb-ext:save-lisp-and-die "gemini-chat"
                            :toplevel #'top
                            :save-runtime-options t
                            ;:compression 22
                            :executable t))
