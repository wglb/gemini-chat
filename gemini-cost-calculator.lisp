(in-package :gemini-cost-calculator)

;; The pricing data, based on the information provided (standard rates: prompts <= 200k tokens).
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
     (:output-cost-per-m 2.50)))
  "Standard pricing (USD per 1 Million tokens) for various Gemini models.
   This excludes long-context prompts (> 200k tokens).")

(defun get-pricing-data (model-keyword)
  "Retrieves the pricing alist for a given model keyword."
  (cdr (assoc model-keyword *gemini-pricing*)))

(defun get-cost-per-m (pricing-data cost-type)
  "Retrieves the specific cost (e.g., :input-cost-per-m) from a pricing alist."
  (let ((result (assoc cost-type pricing-data)))
    (if result
        (cdr result)
        (error "Cost type ~A not found for model pricing data." cost-type))))

(defun normalize-model-name (model-string)
  "Converts a model string (e.g., \"gemini-3.0-pro-preview\") into a
   canonical keyword (e.g., :GEMINI-3.0-PRO-PREVIEW) for map lookup."
  (cond (model-string
		 (let ((keyword-string (cl-ppcre:regex-replace-all "-" model-string "_")))
		   (intern (string-upcase keyword-string) :keyword)))
		(t (intern (string-upcase "gemini-3-pro-preview")))))

(defun calculate-cost-from-log (log-s-expression &optional (pricing *gemini-pricing*))
  "Reads a single log entry S-expression and calculates the total USD cost.

   Expected log-s-expression format:
   '((:model \"model-id-string\")
     (:input-tokens number)
     (:output-tokens number))"
  (let* ((model-string  (second (assoc :modelversion log-s-expression)))
         (input-tokens  (second (assoc :input-tokens log-s-expression)))
         (output-tokens (second (assoc :output-tokens log-s-expression)))
         (model-keyword (normalize-model-name model-string)))

    (unless (and model-string input-tokens output-tokens)
      (error "Invalid log format. Missing :MODEL, :INPUT-TOKENS, or :OUTPUT-TOKENS."))

    (let* ((pricing-data (get-pricing-data model-keyword))
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

;; Example usage:
#+nil
(let* ((sample-log '((:model "gemini-3.0-pro-preview")
                     (:input-tokens 5280)
                     (:output-tokens 932)))
       (costs (multiple-value-list (calculate-cost-from-log sample-log))))
  (apply 'print-cost-report costs))
