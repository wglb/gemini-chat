;;; gemini.el --- Emacs interface for Gemini Agentic Lisp Tools

(defun gemini-fix-lisp-region (start end instruction)
  "Send the active region to Gemini. Passes :debug '(:agent) to the library."
  (interactive "r\nsInstruction: ")
  (let ((code (buffer-substring-no-properties start end)))
    (message "Sending region to Gemini...")
    (slime-eval-async 
     `(gemini-chat-lib::agentic-fix-region ,code ,instruction :debug '(:agent))
     (lambda (response)
       (gemini--display-fix response)))))

(defun gemini-fix-involved-code (instruction)
  "Sends the highlighted lines and file context to Gemini."
  (interactive "sInstruction for complex fix: ")
  (let ((path (buffer-file-name))
        (start (line-number-at-pos (region-beginning)))
        (end (line-number-at-pos (region-end))))
    (message "Analyzing lines %d to %d..." start end)
    (slime-eval-async 
     `(gemini-chat-lib::agentic-fix-involved-section ,path ,start ,end ,instruction :debug '(:agent))
     (lambda (response)
       (gemini--display-fix response)))))

(defun gemini-modify-project (project-dir)
  "Triggers a single-pass project refactor using standard context rules."
  (interactive "DProject Directory: ")
  (let ((context-files '("~/lisplib/production/context/triple-escape.txt"
                         "~/lisplib/production/context/flag-refactor.txt"
                         "~/lisplib/production/context/makefile-standard.txt"
						 "~/lisplib/production/contexts/google-flag-migration.txt")))
    (message "Refactoring project in %s..." project-dir)
    (slime-eval-async
     `(gemini-chat-lib:agentic-project-modify ,project-dir ',context-files)
     (lambda (response)
       (message "Project refactor plan generated in project-update-plan.txt")))))

(defun gemini-iterative-refactor (project-dir)
  "Triggers a multi-pass Fix-Compile-Check loop until 'make all' succeeds."
  (interactive "DProject Directory: ")
  (let ((context-files '("~/lisplib/production/context/triple-escape.txt"
                         "~/lisplib/production/context/flag-refactor.txt"
                         "~/lisplib/production/context/makefile-standard.txt")))
    (message "Starting iterative refactor loop for %s..." project-dir)
    (slime-eval-async
     `(gemini-chat-lib:agentic-iterative-build ,project-dir ',context-files :debug '(:agent))
     (lambda (success)
       (if success
           (message "SUCCESS: Project refactored and compiled cleanly.")
         (message "FAILURE: Agent could not resolve errors. Check project-update-plan.txt."))))))

(defun gemini-modify-project-from-dired ()
  "Call this from a Dired buffer to refactor the directory under point."
  (interactive)
  (let ((file (dired-get-filename)))
    (if (file-directory-p file)
        (gemini-modify-project file)
      (message "Point is not on a directory."))))

(defun gemini-iterative-refactor-from-dired ()
  "Call this from a Dired buffer to start an iterative build loop."
  (interactive)
  (let ((file (dired-get-filename)))
    (if (file-directory-p file)
        (gemini-iterative-refactor file)
      (message "Point is not on a directory."))))

(defun gemini-help ()
  "List all Gemini agentic functions and their current keybindings."
  (interactive)
  (let ((buf (get-buffer-create "*Gemini-Help*"))
        (bindings '(("C-c g f" . gemini-fix-lisp-region)
                    ("C-c g i" . gemini-fix-involved-code)
                    ("C-c g p" . gemini-modify-project-from-dired)
                    ("C-c g r" . gemini-iterative-refactor-from-dired)
                    ("C-c g h" . gemini-help))))
    (with-current-buffer buf
      (read-only-mode -1) (erase-buffer)
      (insert "Gemini Agentic Lisp Tools\n==========================\n\n")
      (dolist (binding bindings)
        (insert (format "%-10s : %s\n             %s\n\n" 
                        (car binding) (cdr binding) (documentation (cdr binding)))))
      (read-only-mode 1) (display-buffer buf))))

(defun gemini-observe-last-update ()
  "Opens the project-update-plan.txt and the last fixed file for inspection."
  (interactive)
  (let ((plan "project-update-plan.txt")
        (last-fix "last-agent-fix.lisp"))
    (when (file-exists-p plan)
      (find-file-other-window plan)
      (auto-revert-mode 1))
    (when (file-exists-p last-fix)
      (find-file-other-window last-fix))))

;; Bind to C-c g o (Observe)
(define-key lisp-mode-map (kbd "C-c g o") 'gemini-observe-last-update)

;;; --- Internal Helpers & Bindings ---

(defun gemini--display-fix (response)
  (with-current-buffer (get-buffer-create "*Gemini-Fix*")
    (read-only-mode -1) (erase-buffer) (insert response) (lisp-mode)
    (display-buffer (current-buffer)) (message "Gemini fix received.")))

(define-key lisp-mode-map (kbd "C-c g f") 'gemini-fix-lisp-region)
(define-key lisp-mode-map (kbd "C-c g i") 'gemini-fix-involved-code)
(define-key lisp-mode-map (kbd "C-c g h") 'gemini-help)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c g p") 'gemini-modify-project-from-dired)
  (define-key dired-mode-map (kbd "C-c g r") 'gemini-iterative-refactor-from-dired))

(provide 'gemini)
