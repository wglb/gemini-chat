;;;; gemini-chat.lisp
;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:gemini-chat)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

;; A default tag to use if none is provided on the command line.
(defparameter *d-tag* "chat")

;; New special variable for the runtime output file stream
(defparameter *run-out-s* nil
  "Stream for saving conversation answers during a conversation.")

;; Global for help formatting
(defparameter *help-column-width* 80)

;; Function to retrieve the current version
(defun get-version ()
  (slot-value (asdf:find-system 'gemini-chat) 'asdf:version))

;; --- Define Flags using com.google.flag ---
(in-package #:gemini-chat)

;; --- Define Flags using cloned com.google.flag ---

(define-flag *keyname*
  :help "Name of gemini static api key to retrieve"
  :description "Name of gemini static api key to retrieve"
  :type string
  :selector "key"
  :default-value "personal")

(define-flag *api-url*
  :help "URL for gemini api"
  :description "URL for gemini api"
  :type string
  :selector "api-url"
  :default-value "https://generativelanguage.googleapis.com/v1beta/models/")

(define-flag *gemini-model*
  :help "Which model to get gemini to run"
  :description "Which model to get gemini to run"
  :type string
  :selector "model"
  :default-value "gemini-2.5-pro")

(define-flag *context*
  :help "Path to a context file. Can be specified multiple times. Example: --context file1.txt,file2.txt"
  :description "This flag specifies a context file. Its content is provided to the model for background information that should influence the response, but isn't the primary subject of your query."
  :type list
  :parser string-identity-parser
  :selector "context"
  :default-value nil)

(define-flag *save*
  :help "File to save Gemini's responses to. Responses will be appended. Example: --save conversation.log"
  :description "This flag tells gemini-chat to append all of the model's responses to the specified file."
  :type string
  :selector "save"
  :default-value "")

(define-flag *tag*
  :help "A unique tag for the conversation logs (default: chat). Example: --tag my-session"
  :description "A unique tag for the conversation logs (default: chat)."
  :type string
  :selector "tag"
  :default-value "chat")

(define-flag *input-files*
  :help "Comma-separated list of input files whose content will be sent with your prompt. Example: --input-files my-code.lisp,data.csv"
  :description "This flag specifies one or more input files. The content of these files is included directly in your prompt, enclosed in markers. This is for content you want the model to directly analyze, modify, or reference."
  :type list
  :parser string-identity-parser
  :selector "input-files"
  :default-value nil)

(define-flag *help-is*
  :help "Show this help message and exit."
  :description "Show this help message and exit."
  :type boolean
  :selector "help"
  :default-value nil
  :description "Print the detailed help using each of the description files")

(define-flag *single-shot*
  :help "Quit after first query completed"
  :description "Quit after first query completed"
  :type boolean
  :selector "single-shot"
  :default-value nil)

(define-flag *exit-on-error*
  :help "Exit the program immediately if an input file cannot be read."
  :description "Exit the program immediately if an input file cannot be read."
  :type boolean
  :selector "exit-on-error"
  :default-value nil)

(defun s-s (str delim &key (rem-empty nil))
  "Encapsulates calls to split-sequence. Splits a string by a single character delimiter.
   :rem-empty T will remove empty strings from the result list."
  (split-sequence:split-sequence delim str :remove-empty-subseqs rem-empty))

(defun s/z (str)
  (zerop (length str)))

(defun s/nz (str)
  (plusp (length str)))

;; --- Primitives for Help Formatting ---

(defun get-sorted-flags ()
  "Retrieves and sorts the registered flags alphabetically by their selector string."
  (sort (copy-list com.google.flag::*registered-flags*)
        #'string< :key #'car))

(defun find-max-selector-length (flags)
  "Finds the maximum length of the flag selector strings for alignment."
  (let ((max-length 0))
    (dolist (flag-pair flags)
      (setf max-length (max max-length (length (car flag-pair)))))
    max-length))

(defun print-flag-help (selector flag max-selector-length)
  "Prints the help message for a single flag with proper padding using ~vt."
  (let* ((help-text (slot-value flag 'com.google.flag::help))
         (padding (+ max-selector-length 5))) ; 5 spaces to separate --selector and help text
    (format t "  --~a~vt~a~%"
            selector
            padding
            help-text)))

(defun show-opts (&key (bad-args nil))
  "Prints the command-line options to the thinking log file, using the flag special variables."
  (w/log ("option" :dates :hour :show-log-file-name nil :append-or-replace :replace)
	(xlogntf "~&Entering run-chat with flags:" )
	(xlogntf "Keyname: ~a" *keyname*)
	(xlogntf "Context files: ~a" *context*)
	(xlogntf "Save file: ~a" *save*)
	(xlogntf "Tag: ~a" *tag*)
	(xlogntf "Input files: ~a" *input-files*)
	(xlogntf "Help requested: ~a" *help-is*)
	(xlogntf "Single shot: ~a" *single-shot*)
	(xlogntf "Exit on error: ~a" *exit-on-error*)
	(xlogntf "Using model: ~a" "gemini-2.5-pro") ; Model is currently hardcoded in run-chat
	(xlogntf (print-all-flag-values))
	(when bad-args
	  (xlogntf "Unprocessed command-line options: ~s" bad-args))))

(defun run-chat (cmd-line)
  (multiple-value-bind (remaining-args badargs)
      (chk-args cmd-line (get-version))
    (show-opts :bad-args badargs) 
    (cond (badargs
           (format t "Bad arguments ~s, exiting" badargs))
          (*help-is*
           (print-long-help (get-version)))
          (t
           (gemini-chat-lib-init :static-key (get-key *keyname*))
           (run-chat-with-kw :gemini-model *gemini-model*
                             :context (proc-ctx-files *context*)
                             :save (unless (s/z *save*) *save*)
                             :tag *tag*
                             :exit-on-error *exit-on-error*
                             :input-files *input-files*
                             :remaining-args remaining-args
                             :echo-to-console t
                             :single-shot *single-shot*))))) ;;  Pass the flag here

(defun top ()
  "Toplevel function for the compiled gemini-chat executable.
   It retrieves arguments from sb-ext:*posix-argv* and passes them to run-chat."
  ;; com.google.flag:parse-command-line without :argv defaults to sb-ext:*posix-argv*
  ;; However, run-chat expects a list of strings, so pass (rest sb-ext:*posix-argv*)
  (run-chat (command-line)))

(defun save-core-uncompressed ()
  "Saves the current Lisp image as an uncompressed executable for faster development."
  (format t "~&Building gemini-chat version ~a (uncompressed)~%~%" (get-version))
  (sb-ext:save-lisp-and-die "gemini-chat"
                            :toplevel #'top
                            :save-runtime-options t
                            :executable t))

(defun save-core ()
  "Saves the current Lisp image as an executable."
  (format t "~&Building gemini-chat version ~a~%~%" (get-version))
  (sb-ext:save-lisp-and-die "gemini-chat"
                            :toplevel #'top
                            :save-runtime-options t
                            :executable t
                            :compression t))

