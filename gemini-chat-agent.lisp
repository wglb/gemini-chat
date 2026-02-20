(in-package #:gemini-chat-lib)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

(defun gemini-fix-file (pathname instruction)
  "Loads a file and asks Gemini to fix it. Corrected for gemini-chat-lib signature."
  (let* ((original-code (uiop:read-file-string pathname))
         (full-prompt (format nil "SYSTEM: Expert Lisp dev. Return ONLY code. NO PROSE.~%USER: ~a~%~%CODE:~%~a" 
                              instruction original-code)))
    (multiple-value-bind (response success-p)
        (run-chat-with-kw :remaining-args (list full-prompt)
                          :save (format nil "~a.fixed" (file-namestring pathname)))
      (if (and success-p response)
          (progn 
            (xlogntft "Agentic fix successful for ~a. Result in .fixed file." (file-namestring pathname))
            ;; RETURN THE RESPONSE HERE
            response) 
          (progn
            (xlogntft "Agentic fix failed or returned empty response.")
            nil)))))

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
  "Applies the agent's plan, strictly blocking Markdown to train the model."
  (let* ((abs-dir (uiop:ensure-directory-pathname project-dir))
         (plan-path (merge-pathnames "project-update-plan.txt" abs-dir)))
    (cond ((uiop:file-exists-p plan-path)
           (let ((raw-text (uiop:read-file-string plan-path)))
             (cond ((cl-ppcre:scan "```" raw-text)
                    ;; This specific string is what iteration 2 will see
                    (error "CRITICAL FAILURE: You used Markdown code blocks (```). This breaks the machine parser. REPEAT THE PLAN using ONLY ===BEGIN_FILE: [path]=== tags."))
                   (t
                    (xlogntft "Architect: Unpacking via file-packer-lib...")
                    (uiop:with-current-directory (abs-dir)
                      (file-packer-lib:unpack-file plan-path))
                    (xlogntft "Unpack complete."))))))))

(defun agentic-iterative-build (project-dir instruction-files &key (max-tries 3) (debug '(:agent)))
  "The core iterative loop. Prompts Bill on turn 1 and catches format/build errors."
  (w/log ("agentic-iterative-build" :dates :hms :dir project-dir)
	(let ((extra-context "")
          (success-p nil)
          (abs-project-dir (uiop:ensure-directory-pathname project-dir)))
      (dolist (iteration (alexandria:iota max-tries :start 1))
		(cond (success-p (return success-p))
              (t
               (let ((instruction (cond ((= iteration 1)
										 (format t "~&Architect (Bill), enter project goal: ")
										 (force-output)
										 (read-line))
										(t (format nil "Fix errors and retry. ~a" extra-context))))
					 (plan-path (merge-pathnames "project-update-plan.txt" abs-project-dir)))
				 
				 (when (member :agent debug)
                   (xlogntft "Iteration ~d: Requesting plan..." iteration))
				 
				 (uiop:with-current-directory (abs-project-dir)
                   (agentic-project-modify abs-project-dir instruction-files 
                                           :extra-instruction instruction))
				 
				 (handler-case
					 (cond ((uiop:file-exists-p plan-path)
							(apply-agentic-plan abs-project-dir)
							(multiple-value-bind (output error-output exit-code)
								(uiop:run-program "make all" 
                                                  :directory abs-project-dir 
                                                  :ignore-error-status t 
                                                  :output :string 
                                                  :error-output :string)
                              (cond ((zerop exit-code) 
									 (setf success-p t)
									 (xlogntft "Build SUCCESS on iteration ~d." iteration))
									(t 
									 (when (member :agent debug)
                                       (xlogntft "Build FAILED. Analyzing output."))
									 (setf extra-context 
                                           (format nil "BUILD ERRORS:~%~a~%~a" output error-output))))))
                           (t (error "The Apprentice disappeared! No plan found at ~a" plan-path)))
                   (error (c)
					 (when (member :agent debug)
                       (xlogntft "Iteration ~d failed: ~a" iteration c))
					 (setf extra-context (format nil "FORMATTING ERROR: ~a" c))))))))
      success-p)))

(defun invoke-apprentice (goal &optional failure-log)
  "Constructs the prompt and calls Bill's actual chat interface."
  (let ((base-prompt (format nil "ARCHITECT'S ORDER: ~a~%~%~
                                  STRICT RULES:~%~
                                  1. Use ~~s for strings; NO backslashes in output.~%~
                                  2. No spaces in flag strings.~%~
                                  3. Output in ===BEGIN_FILE: [path]===; NO MARKDOWN." 
                             goal))
        (context (if failure-log 
                     (format nil "~%FAILURE CONTEXT FROM SBCL:~%~a" failure-log)
                     "")))
    ;; Bridge to your KW system 
    (run-chat-with-kw :remaining-args (list (format nil "~a~a" base-prompt context))
                      :save "project-update-plan.txt")))

(defun run-build-iteration (project-dir order-text &key (max-tries 5))
  "Orchestrates the Apprentice using either a raw string or a file-based order."
  (let ((iteration 0)
        (success nil)
        (last-error nil)
        (abs-dir (uiop:ensure-directory-pathname project-dir))
        (instruction-files '("~/lisplib/production/contexts/system-policy.txt"
                             "~/lisplib/production/contexts/triple-escape.txt"
                             "~/lisplib/production/contexts/makefile-standard.txt")))
    (let ((resolved-order (cond ((pathnamep order-text) (uiop:read-file-string order-text))
                                (t order-text))))
      (xlogntft "Starting Supervisor for Project: ~a" abs-dir)
      (do ((try 1 (1+ try)))
          ((or success (> try max-tries)))
        (setf iteration try)
        (xlogntft "Iteration ~d..." iteration)
        (let ((response (agentic-project-modify abs-dir instruction-files 
                                                :extra-instruction (cond ((= try 1) resolved-order)
                                                                         (t last-error)))))
          (cond ((and response (> (length response) 0))
                 (apply-agentic-plan abs-dir)
                 (multiple-value-bind (output error-output exit-code)
                     (uiop:run-program "make test" 
                                       :directory abs-dir 
                                       :ignore-error-status t 
                                       :output :string 
                                       :error-output :string)
                   (cond ((zerop exit-code)
                          (setf success t)
                          (xlogntft "SUCCESS! Build and Tier-3 tests passed."))
                         (t
                          (setf last-error (format nil "BUILD FAILED. ERRORS:~%~a~%~a" 
                                                   output error-output))
                          (xlogntft "FAILED (Code ~d). Retrying..." exit-code)))))
                (t (xlogntft "WARNING: Apprentice failed to return a plan string."))))))
    (cond (success
           (xlogntft "Goal achieved in ~d tries." iteration))
          (t (error "Supervisor failed to converge after ~d tries." max-tries)))))

(defun example-project-modification ()
  (agentic-project-modify 
   "/home/wgl/lisplib/public/create-lisp-project/"
   '("~/lisplib/production/contexts/system-policy.txt" ;; Added here
     "~/lisplib/production/contexts/triple-escape.txt" 
     "~/lisplib/production/contexts/flag-refactor.txt"
     "~/lisplib/production/contexts/makefile-standard.txt")))

