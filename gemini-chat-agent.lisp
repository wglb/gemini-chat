(in-package #:gemini-chat-lib)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

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

(defun agentic-iterative-build (project-dir instruction-files &key (max-tries 3) (debug '(:agent)))
  "The core iterative loop. Prompts the human on turn 1 to prevent silent failure."
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
                                      (t (format nil "Fix build errors. ~a" extra-context))))
                   (plan-path (merge-pathnames "project-update-plan.txt" abs-project-dir)))
               
               ;; 1. Request Plan from the Apprentice
               ;; Wrap in with-current-directory so relative writes land in project-dir
               (uiop:with-current-directory (abs-project-dir)
                 (agentic-project-modify abs-project-dir instruction-files 
                                         :extra-instruction instruction))
               
               ;; 2. Verify Plan Existence and apply
               (cond ((uiop:file-exists-p plan-path)
                      (apply-agentic-plan abs-project-dir)
                      ;; 3. Run Build/Check
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
                                 (xlogntft "Build FAILED. Capturing errors."))
                               (setf extra-context 
                                     (format nil "ERRORS:~%~a~%~a" output error-output))))))
                     (t (error "The Apprentice dropped the plan elsewhere! Expected: ~a" 
                               plan-path)))))))
    success-p))

(defun example-project-modification ()
  (agentic-project-modify 
   "/home/wgl/lisplib/public/create-lisp-project/"
   '("~/lisplib/production/contexts/triple-escape.txt" 
     "~/lisplib/production/contexts/flag-refactor.txt"
     "~/lisplib/production/contexts/makefile-standard.txt")))
