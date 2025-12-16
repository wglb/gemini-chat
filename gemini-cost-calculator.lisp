(in-package :gemini-cost-calculator)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

;; The pricing data, based on the information provided (standard rates: prompts <= 200k tokens).
;;
;; Prices are in USD per 1,000,000 tokens.
(defparameter *gemini-pricing*
  '((:gemini-3.0-pro-preview
     (:input-cost-per-m 2.00)
     (:output-cost-per-m 12.00))
    (:gemini-2.5-pro
     (:input-cost-per-m 1.25)
     (:output-cost-per-m 10.00))
    (:gemini-2.5-flash
     (:input-cost-per-m 0.30)
     (:output-cost-per-m 2.50))
	(:n/a
	 (:input-cost-per-m 0.30)
     (:output-cost-per-m 2.50)))
  "Standard pricing (USD per 1 Million tokens) for various Gemini models.
   This excludes long-context prompts (> 200k tokens).")

(defun get-pricing-data (model-keyword &optional (pricing-list *gemini-pricing*))
  "Retrieves the pricing alist for a given model keyword from a specific
   pricing list, defaulting to *GEMINI-PRICING*."
  (format t "model keyword ~s~%" model-keyword)
  (let ((ans (cdr (assoc model-keyword pricing-list))))
	(unless ans
	  (error "No pricing data for ~s in ~s" model-keyword pricing-list))))

(defun get-cost-per-m (pricing-data cost-type)
  "Retrieves the specific cost (e.g., :input-cost-per-m) from a pricing alist."
  (let ((result (assoc cost-type pricing-data)))
    (if result
        (second result)
        (error "Cost type ~A not found for model pricing data ~s." cost-type pricing-data))))

(defun normalize-model-name (model-string)
  "Converts a model string to a canonical keyword for price lookup,
   handling the 'gemini-3' vs 'gemini-3.0' naming inconsistency."
  (let ((canonical-string
          (cond ((string-equal model-string "gemini-3-pro-preview")
                 "gemini-3.0-pro-preview") ; Map log name to pricing key
                (model-string
                 model-string)             ; Use string as is
                (t
                 "gemini-3.0-pro-preview")))) ; Use default pricing key
    (intern (string-upcase canonical-string) :keyword)))

(defun calculate-cost-from-log (log-s-expression &optional (pricing *gemini-pricing*))
  "Reads a single log entry S-expression and calculates the total USD cost.

   Expected log-s-expression format:
   '((:PROMPT-TOKEN-COUNT number)
     (:CANDIDATES-TOKEN-COUNT number)
     (:MODELVERSION \"model-id-string\"))
   
   If :MODELVERSION is missing, it defaults to \"gemini-3-pro-preview\"."
  (let* ((model-alist (assoc :modelversion log-s-expression))
         (model-string  (if model-alist
                            (cdr model-alist)
                            "gemini-3-pro-preview"))
         (input-tokens  (cdr (assoc :prompt-token-count log-s-expression)))
         (output-tokensr (cdr (assoc :candidates-token-count log-s-expression)))
		 (output-tokens (if (stringp output-tokensr)
						   0
						   output-tokensr))
         (model-keyword (normalize-model-name model-string)))
    (unless (and input-tokens output-tokens)
      (error "Invalid log format. Missing :PROMPT-TOKEN-COUNT or :CANDIDATES-TOKEN-COUNT."))

    (let* ((pricing-data (get-pricing-data model-keyword pricing))
           (input-cost-m (get-cost-per-m pricing-data :input-cost-per-m))
           (output-cost-m (get-cost-per-m pricing-data :output-cost-per-m))
           (token-multiplier 1000000.0)
           (input-cost (* (/ input-tokens token-multiplier) input-cost-m))
           (output-cost (* (/ output-tokens token-multiplier) output-cost-m))
           (total-cost (+ input-cost output-cost)))
      (values total-cost
              input-cost
              output-cost
              model-string
              input-tokens
              output-tokens))))

(defun print-cost-report (total input output model input-tokens output-tokens)
  "Prints a formatted report of the cost calculation."
  (format t "~&--- API Cost Report ---")
  (format t "~&Model: ~A" model)
  (format t "~&Input Tokens: ~:D" input-tokens)
  (format t "~&Output Tokens: ~:D" output-tokens)
  (format t "~&-----------------------")
  (format t "~&Input Cost:  $~8,6F" input)
  (format t "~&Output Cost: $~8,6F" output)
  (format t "~&Total Cost:  $~8,6F" total)
  (format t "~&-----------------------~%")
  (values))

(defun process-log-file-forms (log-forms &optional (pricing *gemini-pricing*))
  "Calculates the total cost and token counts for a list of log S-expressions
   and prints an aggregated report."
  (let ((total-cost 0.0)
        (total-input-tokens 0)
        (total-output-tokens 0)
        (model-name "Mixed or N/A"))
    
    (loop for log-entry in log-forms do
      (multiple-value-bind (entry-total-cost input-cost output-cost model input-tokens output-tokens)
          (calculate-cost-from-log log-entry pricing)
        (declare (ignore input-cost output-cost))
        
        (incf total-cost entry-total-cost)
        (incf total-input-tokens input-tokens)
        (incf total-output-tokens output-tokens)
        
        (when (string-equal model "gemini-3-pro-preview")
          (setf model-name model))))

    (let* ((model-keyword (normalize-model-name model-name))
           (pricing-data (get-pricing-data model-keyword pricing))
           (input-cost-m (get-cost-per-m pricing-data :input-cost-per-m))
           (output-cost-m (get-cost-per-m pricing-data :output-cost-per-m))
           (token-multiplier 1000000.0)
           (final-input-cost (* (/ total-input-tokens token-multiplier) input-cost-m))
           (final-output-cost (* (/ total-output-tokens token-multiplier) output-cost-m)))

      ;; Print the aggregated report
      (format t "~&=== Aggregated API Cost Report ===")
      (format t "~&Model(s) Used: ~A" model-name)
      (format t "~&Total Input Tokens: ~:D" total-input-tokens)
      (format t "~&Total Output Tokens: ~:D" total-output-tokens)
      (format t "~&----------------------------------")
      (format t "~&Total Input Cost:  $~8,6F" final-input-cost)
      (format t "~&Total Output Cost: $~8,6F" final-output-cost)
      (format t "~&Total Grand Cost:  $~8,6F" total-cost)
      (format t "~&==================================~%")

      ;; Return the aggregated values
      (values total-cost total-input-tokens total-output-tokens model-name))))

(defun process-all-tokens-in-directory (directory-path &optional (pricing *gemini-pricing*))
  "Reads all *.tkn files in the specified directory, aggregates their contents,
   and calculates the total cost for the project."
  (let ((all-forms nil))
    
    (format t "~&Searching for *.tkn files in: ~A" directory-path)
    
    ;; Use UIOP to find all files ending in .tkn within the directory
    (loop for file in (uiop:directory-files directory-path "*.tkn") do
      (format t "~&  Found and reading file: ~A" (file-namestring file))
      
      ;; Read forms from the current file and append them to the list efficiently
      (handler-case
          (setf all-forms (nconc all-forms (uiop:read-file-forms file)))
        (error (e)
          (format *error-output* "~&Error reading forms from ~A: ~A~%" 
                  (file-namestring file) e))))
    
    (if all-forms
        (process-log-file-forms all-forms pricing) ; THEN branch
        (progn ; CORRECTED: Use PROGN for multiple ELSE forms
          (format t "~&No *.tkn files found in ~A. Total cost is $0.00.~%" directory-path)
          (values 0.0 0 0 "N/A")))))



(defun top ()
  (let* ((argd sb-ext:*posix-argv*)
		 (arg (if argd
				  "."
				  argd)))
	(process-all-tokens-in-directory arg)))

(defun save-core ()
  (sb-ext:save-lisp-and-die "gemini-cost-calculator"
                            :toplevel #'top
                            :save-runtime-options t
                            :executable t
                            :compression t))
