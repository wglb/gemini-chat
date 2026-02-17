;;;; gemini-chat.lisp
;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:gemini-chat-lib)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

;; A default tag to use if none is provided on the command line.
(defparameter *tag* "chat")

;; New special variable for the runtime output file stream
(defparameter *run-out-s* nil
  "Stream for saving conversation answers during a conversation.")

(defun get-version ()
  (slot-value (asdf:find-system 'gemini-chat-lib) 'asdf:version))

(defun get-key (keyname)
  "Retrieves the Gemini API key from ~/.gemini/keys.lsp. If not found,
   we use the GEMINI_API_KEY environment variable.
   Signals an error if the environment variable is not set."
  (let* ((fn "~/.gemini/keys.lsp")
         (keys (if (probe-file fn)
                   (uiop:read-file-form fn)
                   nil)))
    (let* ((keyv (cond ((eq (type-of keyname) :keyword)
                        keyname)
                       (t (intern (string-upcase keyname) :keyword))))
           (key (cond (keys
                       (second (assoc keyv keys)))
                      (t (format t "getting key from environment ~%")
                         (getenv "GEMINI_API_KEY")))))
      (unless key
        (error "Error: The GEMINI_API_KEY environment variable is not set.
              Please set this before running this program."))
      key)))

(defvar *last-token-refresh* 0)

(defvar *cached-token* nil) ;; (setf *cached-token* (gemini-chat-lib::get-fresh-bearer-token))

(defun get-fresh-bearer-token ()
  "Fetches a fresh OAuth2 token using gcloud. Caches it for 50 minutes."
  (w/log ((format nil "auth" ) :dates :hour :show-log-file-name nil :append-or-replace :append))
  (let ((now (get-universal-time)))
    (if (and *cached-token* (< (- now *last-token-refresh*) 3000))
        *cached-token*
        (let ((new-token (string-trim '(#\Space #\Newline #\Return)
                                      (uiop:run-program "gcloud auth print-access-token" 
														:output :string))))
          (setf *last-token-refresh* now)
          (setf *cached-token* new-token)
          (xlogf  "Token refreshed at ~A" now)
          new-token))))

(defun s-s (str delim &key (rem-empty nil))
  "Encapsulates calls to split-sequence. Splits a string by a single character delimiter.
   :rem-empty T will remove empty strings from the result list."
  (split-sequence:split-sequence delim str :remove-empty-subseqs rem-empty))

(defun s/z (str)
  (zerop (length str)))

(defun s/nz (str)
  (plusp (length str)))

;; --- Primitives for JSOWN-specific data structure creation ---

(defun make-text-part (text)
  "Creates a Lisp :OBJ representing a 'part' in the Gemini API JSON structure."
  (jsown:new-js ("text" text)))

(defun make-content-object (parts)
  "Creates a Lisp :OBJ representing 'content' in the Gemini API JSON structure."
  (jsown:new-js ("parts" parts)))

(defun make-message-turn (role content-parts)
  "Creates a Lisp :OBJ representing a single 'turn' (message) in the Gemini API JSON structure.
   This will be encoded by JSOWN as a JSON object."
  (jsown:new-js
    ("role" role)
    ("parts" content-parts)))

(defun make-api-request-payload (msgs)
  "Constructs the full JSON payload object for the Gemini API."
  (jsown:new-js ("contents" msgs)))

(defun api-req (msgs &key (model "gemini-2.5-pro"))
  "Constructs and sends an HTTP POST request to the Gemini API, 
   delegating to DO-API-REQUEST for backoff and auth. Returns the parsed JSON object."
  (w/log ((format nil "~a-thinking" "api") :dates :hour :show-log-file-name nil :append-or-replace :append))
  (let* (;; The API key is handled by the client library *static-api-key*
         (uri-parts (format nil "models/~a:generateContent" model))
         (json-payload-lisp-object (make-api-request-payload msgs))
         (json-payload-string (jsown:to-json json-payload-lisp-object)))
    (xlogf "~&Making API request to: ~a" uri-parts )
    (xlogf "JSON string being sent: ~a" json-payload-string)
    (handler-case
        (let ((result-list (do-api-request uri-parts json-payload-string :post)))
          (first result-list))	  ; <-- Returns the parsed JSON object
      (error (c)
        (error "An unexpected error occurred during the API request: ~a" c)))))

(defun log-token-data (token-log-data)
  (with-open-file (to (format nil "~a_token.tkn" (dates-ymd :ymd)) :direction :output :if-exists :append :if-does-not-exist :create)
	(format to "~s" token-log-data)))

(defmacro log-answer (tag control-string &rest args)
  `(w/log ((format nil "~a-answer" (or ,tag "process")) 
                        :dates :hour 
                        :show-log-file-name nil
                        :append-or-replace :append)
     (xlogf ,control-string ,@args)))

(defmacro log-error (tag control-string &rest args)
  "Log formatted error data to a tag-specific error log."
  `(with-open-log-file ((format nil "~a-error" (or ,tag "")) 
                        :dates :hour 
                        :show-log-file-name nil 
                        :append-or-replace :append)
     (xlogf ,control-string ,@args)))

(defun extract-txt (parsed-json echo-to-console)
  "Extracts the generated text from the parsed Gemini API JSON response using jsown accessors.
Returns the text string or NIL if not found."

  ;; --- START Token Count Extraction ---
  (let ((usage-metadata (jsown:val-safe parsed-json "usageMetadata")))
    (when usage-metadata
      (let ((prompt-tokens (jsown:val-safe usage-metadata "promptTokenCount"))
            (candidate-tokens (jsown:val-safe usage-metadata "candidatesTokenCount"))
            (total-tokens (jsown:val-safe usage-metadata "totalTokenCount"))
            (thoughts-tokens (jsown:val-safe usage-metadata "thoughtsTokenCount"))
            (details (jsown:val-safe usage-metadata "promptTokensDetails"))
            (model-version (jsown:val-safe parsed-json "modelVersion")))
        (let ((token-log-data (list
                               (cons :prompt-token-count (or prompt-tokens "N/A"))
                               (cons :candidates-token-count (or candidate-tokens "N/A"))
                               (cons :total-token-count (or total-tokens "N/A"))
                               (cons :thoughts-token-count (or thoughts-tokens "N/A"))
                               (cons :prompt-tokens-details (or details "N/A"))
                               (cons :modelversion (or model-version "N/A")))))
          (xlogf "--- Token Usage (Last Response) ---~%      ~S" token-log-data)
		  (log-token-data token-log-data)
          (xlognt  "-----------------------------------")))))
  ;; --- END Token Count Extraction ---

  (cond ((jsown:keyp parsed-json "error")
         ;; Extract detailed error info
         (let* ((the-err (jsown:val parsed-json "error"))
                (code (jsown:val-safe the-err "code"))
                (message (jsown:val-safe the-err "message"))
                (status (jsown:val-safe the-err "status")))

           (xlogntf   "~&API Error [~a] ~a: ~a" code status message)
		   (xlogf  "~&API Error [~a] ~a: ~a" code status message )
           (log-answer "extract"  "~&API Error [~a] ~a: ~a" code status message )
           nil))
        
        ((jsown:keyp parsed-json "candidates")
         (let* ((candidates (jsown:val parsed-json "candidates"))
                (first-candidate (car candidates)))

           ;; --- Robust Key Checking ---
           ;; Use nested IF statements and jsown:val-safe to check for the presence of the nested keys 
           ;; (content, parts, text) before attempting to access them.
           (if first-candidate
               (let ((content (jsown:val-safe first-candidate "content")))
                 (if content
                     (let ((parts (jsown:val-safe content "parts")))
                       (if parts
                           (let* ((first-part (car parts))
                                  (text (if first-part (jsown:val-safe first-part "text"))))
                             
                             (if text
                                 (progn
                                   (log-answer "extract" "~&Raw answer: ~a" text)
                                   (setf text (string-trim '(#\Space #\Tab #\Newline) text))
                                   (when echo-to-console
									 (format t "~%~a~%" text)) ;; HMMM this is needed if we are interactive. TODO
                                   (if *run-out-s*
									   (progn
										 (format *run-out-s* "~%~a~%" text)
										 (finish-output *run-out-s*)))
                                   text)
                                 ;; Failure: Missing 'text' key
                                 (progn
                                   (xlogntf "~&Warning: Content found, but 'text' key missing in parts. JSON: ~S" parsed-json)
								   (log-error nil "~&Warning: Content found, but 'text' key missing in parts. JSON: ~S" parsed-json)
                                   nil)))
                           ;; Failure: Missing 'parts' key
                           (progn
                             (xlogntf "~&Warning: 'content' found, but 'parts' key missing. JSON: ~S" parsed-json)
                             (xlogntf "~&Warning: 'content' found, but 'parts' key missing. JSON: ~S" parsed-json)
                             nil)))
                     ;; Failure: Missing 'content' key
                     (progn
                       (xlogntf "~&Warning: Candidate found, but 'content' key missing. JSON: ~S" parsed-json)
                       (log-error nil  "~&Warning: Candidate found, but 'content' key missing. JSON: ~S" parsed-json)
                       nil)))
               ;; Failure: Empty 'candidates' list
               (progn
                 (xlogntf "~&Warning: 'candidates' key found, but list is empty. JSON: ~S" parsed-json)
                 (log-error nil "~&Warning: 'candidates' key found, but list is empty. JSON: ~S" parsed-json)
                 nil))))

        (t 
         ;; Handles the case where 'error' and 'candidates' keys are both missing (e.g., empty object or unexpected format)
         (xlogntf  "~&API Response missing 'candidates' and 'error' keys. JSON: ~S" parsed-json)
         (log-error nil "~&API Response missing 'candidates' and 'error' keys. JSON: ~S" parsed-json)
         nil)))

;; --- Primitives for Prompt Assembly ---

(defun assemble-context-prompt (ctx-content)
  "Assembles the context section of the prompt."
  (when (s/nz ctx-content)
    (format nil "--- Context Files --~%~a~%--- End Context Files --~%" ctx-content)))

(defun assemble-input-files-prompt (input-files exit-on-error)
  (declare (ignorable exit-on-error))
  (let ((text-segments nil)
        (blob-ids nil)
        (all-files-read-ok t))
    (dolist (file-path input-files)
      (cond 
        ((and (stringp file-path) (alexandria:starts-with-subseq "files/" file-path))
         (push file-path blob-ids))
        
        (t 
         (let ((native-file-path (uiop:ensure-pathname file-path))
               (file-content nil))
           ;; 1. Try UTF-8 first (Standard)
           (handler-case
               (setf file-content (uiop:read-file-string native-file-path :external-format :utf-8))
             (error () 
               ;; 2. Detect UTF-16LE via octets 377 376 (255 254)
               (handler-case
                   (progn
                     (xlogntf  "Attempting recovery for ~A using :utf-16le" native-file-path)
					 (log-error nil "Attempting recovery for ~A using :utf-16le" native-file-path)
                     ;; In SBCL, :utf-16le is the explicit format for #(255 254) files
                     (setf file-content (uiop:read-file-string native-file-path :external-format :utf-16le)))
                 (error (c)
				   (xlogntf "Final read error on ~A: ~A" native-file-path c)
                   (setf all-files-read-ok nil)))))

           (when file-content
             (push (format nil "===BEGIN_FILE: [~a]===~%~a~%===END_FILE: [~a]===" 
                           native-file-path file-content native-file-path) 
                   text-segments))))))
    (values (format nil "~{~a~%~%~}" (nreverse text-segments))
            all-files-read-ok
            (nreverse blob-ids))))

(defun assemble-user-prompt (prompt)
  "Formats the user's initial prompt."
  (when (s/nz prompt)
    (format nil "My prompt: ~a" prompt)))

(defun build-full-prompt (context input-files prompt-text exit-on-error)
  "Combines context, files, and task prompt. Passes blob-ids through."
  (multiple-value-bind (files-string success-p blob-ids)
      (assemble-input-files-prompt input-files exit-on-error)
	(unless success-p
	  (break "why no success??"))
    (let ((full-text 
            (if context
                (format nil "CONTEXT:~%~a~%~%FILES:~%~a~%~%TASK:~%~a" 
                        context files-string prompt-text)
                (format nil "FILES:~%~a~%~%TASK:~%~a" 
                        files-string prompt-text))))
      (values full-text success-p blob-ids))))


(defun make-file-part (file-id)
  (let ((full-uri (if (alexandria:starts-with-subseq "https://" file-id)
                      file-id
                      (format nil "https://generativelanguage.googleapis.com/v1beta/~a" file-id))))
    `(:obj ("file_data" . (:obj ("mime_type" . "text/plain")
                                ("file_uri" . ,full-uri))))))

(defun gem-conv (initial-prompt save single-shot exit-on-error &key (model "gemini-2.0-flash") (blob-ids nil) (echo-to-console nil))
  "Handles a turn using DO instead of LOOP. Returns the last model answer string."
  (declare (ignorable exit-on-error))
  (w/log ((format nil "~a-thinking" "conv") :dates :hour :show-log-file-name nil :append-or-replace :append)
    (let* ((parts (append (list (make-text-part initial-prompt))
                          (mapcar #'make-file-part blob-ids)))
           (conversation-history (list (make-message-turn "user" parts))))
      ;; do (variable-definitions) (termination-condition return-value) body
      (do ((last-answer nil)
           (user-input nil)
           (done-p nil))
          (done-p last-answer)
        (let* ((parsed-json (api-req conversation-history :model model)))
          (when save
            (save-cmd (format nil ":save ~a" save)))
          (let ((answer (extract-txt parsed-json echo-to-console)))
            (setf last-answer answer)
            (when (jsown:keyp parsed-json "candidates")
              (push (make-message-turn "model" (list (make-text-part answer))) 
                    conversation-history))
            
            ;; Check termination for single-shot or quit response
            (if (or single-shot (string= answer "quit") (string= answer ":quit"))
                (setf done-p t)
                ;; Interactive branch
                (progn
                  (xlogntft "~&Single shot is ~s>> " single-shot)
                  (finish-output)
                  (setf user-input (read-line))
                  (let ((command (if (s/nz user-input)
                                     (string-trim '(#\Space #\Tab)
                                                  (car (s-s user-input #\Space :rem-empty nil)))
                                     "")))
                    (cond
                      ((or (string= user-input "quit") (string= user-input ":quit"))
                       (setf done-p t))
                      ((string= command ":input") 
                       (input-cmd user-input))
                      ((string= command ":save") 
                       (save-cmd user-input))
                      (t
                       (push (make-message-turn "user" (list (make-text-part user-input))) 
                             conversation-history))))))))))))

(defun save-cmd (out-to-user &key (if-exists :supersede))
  "Handles the :save command, opening a new file for responses."
  (let* ((args (s-s out-to-user #\Space :rem-empty t))
         (file-path (second args)))
    (when (s/z file-path)
      (format t "~&Please specify a file to save to, e.g., :save my-session.log~%")
      (return-from save-cmd))
	(when *run-out-s*
      (format t "~&gchat: save-cmd: Closing previous save file: ~a~%" (file-namestring (pathname *run-out-s*)))
      (finish-output *run-out-s*) ;; Flush before closing
      (close *run-out-s*)
      (setf *run-out-s* nil))
    (handler-case
        (let ((actual-path (uiop:ensure-pathname (if (uiop:absolute-pathname-p file-path)
                                                     file-path
                                                     (uiop:merge-pathnames* file-path (uiop:getcwd))))))
          (xlogntf  "~&Opening save file: ~a" actual-path)
          (setf *run-out-s* (open actual-path :direction :output :if-does-not-exist :create :if-exists if-exists))
          (xlogntf "~&gchat: save-cmd: Now saving responses to: ~a" actual-path)
          (finish-output *run-out-s*)) 
      (error (c)
        (xlogntf  "~s~&gchat: save-cmd: Failed to open file for saving" c)
        (setf *run-out-s* nil)))))

(defun input-cmd (user-input)
  "Handles the :input command. Currently just logs the intention."
  (let* ((args (s-s user-input #\Space :rem-empty t))
         (files (second args)))
    (when (s/z files)
      (format t "~&Please specify input files, e.g., :input file1.txt,file2.lisp~%")
      (return-from input-cmd))
    (format t "~&[Note: Input files functionality is part of the initial prompt only in this version. Files will be included in the next request, but not in interactive mode as of yet]~%")))

(defun get-default-context-file ()
  "Returns the path to the default context file if it exists."
  (let ((default-path (uiop:merge-pathnames* "gemini-chat-context.md" (uiop:getcwd))))
    (if (uiop:file-exists-p default-path)
        default-path
        nil)))

(defun proc-ctx-files (file-list)
  "Processes a list of context files and returns a single string of their combined content."
  (when file-list
    (let ((result (make-string-output-stream)))
      (dolist (file file-list)
        (handler-case
            (format result "~&File: ~a~%```~%~a~%```~%~%" (file-namestring file) (uiop:read-file-string file))
          (error (e)
			(w/log ((format nil "~a-thinking" "ctx") :dates :hour :show-log-file-name nil :append-or-replace :append)
			  (xlogf  "~&Failed to read context file: ~a" e)
              (format t "~&Failed to read context file: ~a~%" e)))))
      (get-output-stream-string result))))

(defun write-batch-jsonl-line (custom-id prompt-text model output-path stream)
  "Writes the 'custom_id' and 'request' wrapper Vertex AI expects for structured batch results."
  (declare (ignore model output-path))
  (let* ((json-line (jsown:to-json
                     `(:obj 
                        ("custom_id" . ,custom-id) ;; metadata foreign key
                        ("request" . (:obj 
                                       ("contents" . ((:obj ("role" . "user")
                                                            ("parts" . ((:obj ("text" . ,prompt-text)))))))
                                       ("generationConfig" . (:obj ("temperature" . 0.0)))))))))
    (format stream "~A~%" json-line)
    (finish-output stream)))


(defun run-chat-with-kw (&key 
                           (gemini-model "gemini-2.5-pro")
                           (context "context.txt")
                           (save nil)
                           (tag "kw")
                           (exit-on-error t)
                           (input-files nil)
                           (batch-mode nil)
                           (batch-stream nil)
						   (custom-id nil)
                           (remaining-args nil))
  "Orchestrates prompt assembly. Batch-mode redirects to batch-stream."
  (w/log ((format nil "~a-thinking" tag) :dates :hour :show-log-file-name nil :append-or-replace :append)
	(let ((prompt-text (car remaining-args)))
      ;; build-full-prompt signature: (context input-files prompt-text exit-on-error)
      (multiple-value-bind (assembled-prompt success-p blob-ids)
          (build-full-prompt context input-files prompt-text exit-on-error)
		(cond (batch-mode
			   (cond ((not (and batch-stream (open-stream-p batch-stream)))
					  (error "Batch mode requested but :batch-stream is NIL or closed."))

					 (success-p
					  (write-batch-jsonl-line custom-id assembled-prompt gemini-model save batch-stream)
					  (values t t))

					 (exit-on-error
					  (error "Failed to assemble prompt for ~A" (or save "unknown file")))

					 (t 
					  (values nil nil))))
			  
			  (success-p ;; gem-conv signature: (initial-prompt save single-shot exit-on-error &key model blob-ids)
			   (let ((response (gem-conv assembled-prompt save t exit-on-error 
										 :model gemini-model 
										 :blob-ids blob-ids)))
				 (values response t)))

			  (exit-on-error
			   (error "Failed to assemble prompt for live mode."))

			  (t 
			   (values nil nil)))))))

(defun gemini-fix-file (pathname instruction)
  "Loads a file and asks Gemini to fix it. Corrected for gemini-chat-lib signature."
  (let* ((original-code (uiop:read-file-string pathname))
         (full-prompt (format nil "SYSTEM: Expert Lisp dev. Return ONLY code. NO PROSE.~%USER: ~a~%~%CODE:~%~a" 
                              instruction original-code)))
    ;; multiple-value-bind response from run-chat-with-kw
    ;; Note: It returns (values response-text success-p)
    (multiple-value-bind (response success-p)
        (run-chat-with-kw :remaining-args (list full-prompt)
                          :save (format nil "~a.fixed" (file-namestring pathname)))
      (if (and success-p response)
          (progn 
            (xlogntft "Agentic fix successful for ~a. Result in .fixed file." (file-namestring pathname))
            response) ;; Use the variable to satisfy the style-warning
          (xlogntft "Agentic fix failed or returned empty response.")))))

(defun gemini-fix-region-to-file (code-text instruction &key (filename "agent-fix.lisp"))
  "Automates the transformation of a specific code block."
  (let ((full-prompt (format nil "SYSTEM: Expert Lisp dev. Return ONLY code. NO PROSE.~%~
                                  USER: ~a~%~%CODE BLOCK:~%~a" 
                             instruction code-text)))
    (multiple-value-bind (response success-p)
        (run-chat-with-kw :remaining-args (list full-prompt)
                          :save filename)
      (if (and success-p response)
          (progn
            (xlogntft "Agentic fix successful. Result written to ~a" filename)
            response)
          (xlogntft "Agentic fix failed to return valid code.")))))




(defun agentic-fix-involved-section (path start-line end-line instruction &key (debug nil))
  "Fixes a specific line range in PATH while providing the first 50 lines as context.
This prevents the agent from hallucinating package names or global variables.
Returns the corrected section as a string."
  (let* ((full-text (uiop:read-file-string path))
         (lines (split-sequence:split-sequence #\Newline full-text))
         ;; Context: first 50 lines (packages/globals)
         (context (subseq lines 0 (min 50 (length lines))))
         ;; Target: the specific lines to fix (1-based indexing)
         (target (subseq lines (1- start-line) (min end-line (length lines))))
         (full-prompt (format nil "CONTEXT (Package/Globals):~%~{~A~%~}~%~
                                  TARGET CODE TO FIX:~%~{~A~%~}~%~
                                  INSTRUCTION: ~a" 
                             context target instruction)))
    (when (member :agent debug)
      (xlogntft "Agentic fix for ~a (lines ~a-~a)" (file-namestring path) start-line end-line))
    (multiple-value-bind (response success-p)
        (run-chat-with-kw :remaining-args (list full-prompt)
                          :save "complex-fix.lisp")
      (if (and success-p (stringp response))
          (cl-ppcre:regex-replace-all "(?s)^```(?:lisp)?\\n?|\\n?```$" response "")
          "Error: Complex fix failed."))))

;;; --- Core Logic Helpers ---

(defun %agentic-strip-markdown (text)
  "Internal helper to remove Markdown code fences if the agent includes them."
  (cl-ppcre:regex-replace-all "(?s)^```(?:lisp)?\\n?|\\n?```$" text ""))

(defun %agentic-call-and-save (prompt save-path &optional (context-msg "Fix"))
  "Internal bridge to the chat-KW system. Handles success/fail logging."
  (multiple-value-bind (response success-p)
      (run-chat-with-kw :remaining-args (list prompt) :save save-path)
    (if (and success-p (stringp response))
        (progn
          (xlogntft "Agentic ~a successful. Result in: ~a" context-msg save-path)
          (%agentic-strip-markdown response))
        (progn
          (xlogntft "Agentic ~a failed for ~a." context-msg save-path)
          nil))))

;;; --- User Facing Functions ---

(defun agentic-fix-region (code-text instruction &key (debug nil))
  "Directly transforms CODE-TEXT based on INSTRUCTION. Returns clean string.
Uses :debug '(:agent) to trigger extended logging."
  (let ((full-prompt (format nil "Provide ONLY the corrected Common Lisp source code. ~
                                  No prose, no markdown backticks, no explanations.~%~%~
                                  INSTRUCTION: ~a~%~%~
                                  CODE TO FIX:~%~a" 
                             instruction code-text)))
    (multiple-value-bind (response success-p)
        (run-chat-with-kw :remaining-args (list full-prompt)
                          :save "last-agent-fix.lisp")
      ;; Use the debug variable to gate the status log
      (when (member :agent debug)
        (xlogntft "Agent Status - Success: ~a | Response Length: ~a" 
                  success-p (if (stringp response) (length response) 0)))
      
      (if (and success-p (stringp response))
          (let ((clean (%agentic-strip-markdown response)))
            (when (member :agent debug)
              (xlogntft "Returning ~d characters to Emacs." (length clean)))
            clean)
          "Error: Gemini failed to return a valid string response."))))

(defun agentic-project-modify (project-dir instruction-files &key (extra-instruction ""))
  "Packs the project and appends specific instruction files PLUS iterative feedback."
  (let* ((base-path (uiop:ensure-directory-pathname project-dir))
         (project-files (remove-if-not 
                         (lambda (p) 
                           (let ((ext (pathname-type p)) (name (pathname-name p)))
                             (or (member ext '("lisp" "asd" "1") :test #'string-equal)
                                 (string-equal name "Makefile"))))
                         (uiop:directory-files base-path)))
         ;; NEW: Explicitly find the primary system map to emphasize it
         (asd-content (with-output-to-string (s)
                        (dolist (p project-files)
                          (when (string-equal (pathname-type p) "asd")
                            (format s "~%--- SYSTEM MAP: ~a ---~%~a~%" 
                                    (pathname-name p) (uiop:read-file-string p))))))
         (project-context 
          (with-output-to-string (s) 
            (file-packer-lib:pack-files-to-stream project-files s)))
         (instr-context
          (with-output-to-string (s)
            (dolist (f-path instruction-files)
              (format s "~%--- POLICY: ~a ---~%~a~%" f-path (uiop:read-file-string f-path)))
            (format s "~%--- ITERATION FEEDBACK ---~%~a~%" extra-instruction))))
    (run-chat-with-kw :remaining-args (list (format nil "SYSTEM STRUCTURE:~%~a~%~%FILES:~%~a~%~%INSTRUCTIONS:~%~a" 
                                                    asd-content project-context instr-context))
                      :save "project-update-plan.txt")))

(defun apply-agentic-plan (project-dir)
  "Parses project-update-plan.txt and writes contents to files in PROJECT-DIR.
Creates .bak files before overwriting."
  (let* ((plan-path (merge-pathnames "project-update-plan.txt" (uiop:ensure-directory-pathname project-dir)))
         (plan-text (uiop:read-file-string plan-path))
         ;; Regex to find blocks: ===BEGIN_FILE: path=== ... content ...
         (scanner (cl-ppcre:create-scanner "(?s)===BEGIN_FILE: (.*?)===\\n(.*?)(?=\\n===BEGIN_FILE:|$)" 
                                           :multi-line-mode t)))
    (cl-ppcre:do-register-groups (file-path content) (scanner plan-text)
      (let* ((full-path (merge-pathnames file-path (uiop:ensure-directory-pathname project-dir)))
             (bak-path (make-pathname :defaults full-path :type "bak")))
        (when (probe-file full-path)
          (uiop:copy-file full-path bak-path))
        (with-open-file (out full-path :direction :output :if-exists :supersede)
          (write-string (%agentic-strip-markdown content) out))
        (xlogntft "Applied updates to ~a (Backup created at ~a)" file-path (file-namestring bak-path))))))

(defun agentic-iterative-build (project-dir instruction-files &key (max-tries 3) (debug nil))
  "Refactor-Compile-Fix loop. Uses the project's Makefile 'all' target for validation."
  (let ((iteration 0)
        (success-p nil) ;; Renamed to avoid confusion and properly initialized
        (extra-context ""))
    (loop while (and (< iteration max-tries) (not success-p)) do
      (incf iteration)
      (xlogntft "Iteration ~d: Attempting refactor and build..." iteration)
      
      ;; 1. Generate and Apply Plan
      (let ((instruction (format nil "Refactor project. ~a" extra-context)))
        (agentic-project-modify project-dir instruction-files :extra-instruction instruction))
      (apply-agentic-plan project-dir)

      ;; 2. Run Makefile 'all'
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program "make all" 
                            :directory project-dir 
                            :ignore-error-status t 
                            :output :string 
                            :error-output :string)
        (if (zerop exit-code)
            (setf success-p t)
            (progn 
              (when (member :agent debug)
                (xlogntft "Build FAILED. Capturing errors for turn ~d." iteration))
              ;; Feed the error output into the next prompt
              (setf extra-context 
                    (format nil "PREVIOUS BUILD ERRORS:~%STDOUT:~%~a~%STDERR:~%~a" 
                            output error-output))))))
    (if success-p
        (xlogntft "Build SUCCESS.")
        (xlogntft "Build FAILED after ~d attempts." max-tries))
    success-p))

(defun example-project-modification ()
  (agentic-project-modify 
   "/home/wgl/lisplib/public/create-lisp-project/"
   '("~/lisplib/production/context/triple-escape.txt" 
     "~/lisplib/production/context/flag-refactor.txt"
     "~/lisplib/production/context/makefile-standard.txt")))


;;(gemini-chat-lib-init :static-key (get-key "ciex") :service-account "")


