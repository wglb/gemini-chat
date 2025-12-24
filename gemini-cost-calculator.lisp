(in-package :gemini-cost-calculator)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

(defparameter *gemini-pricing*
  '((:GEMINI-3.0-PRO-PREVIEW (:INPUT-COST-PER-M 2.00) (:OUTPUT-COST-PER-M 12.00))
    (:GEMINI-3-PRO-PREVIEW   (:INPUT-COST-PER-M 2.00) (:OUTPUT-COST-PER-M 12.00))
    (:GEMINI-2.5-PRO         (:INPUT-COST-PER-M 1.25) (:OUTPUT-COST-PER-M 10.00))
    (:GEMINI-2.5-FLASH       (:INPUT-COST-PER-M 0.30) (:OUTPUT-COST-PER-M 2.50))
    (:N/A                    (:INPUT-COST-PER-M 0.30) (:OUTPUT-COST-PER-M 2.50)))
  "Standard pricing (USD per 1 Million tokens) for Gemini models." )

(defun get-pricing-data (model-keyword &optional (pricing-list *gemini-pricing*))
  "Retrieves the pricing alist for a given model keyword."
  (let ((ans (cdr (assoc model-keyword pricing-list))))
    (if ans
        ans
        (error "No pricing data for ~s" model-keyword)))) 

(defun get-cost-per-m (pricing-data cost-type)
  "Retrieves the specific cost from a pricing alist."
  (let ((result (assoc cost-type pricing-data)))
    (if result
        (second result)
        (error "Cost type ~A not found in ~s." cost-type pricing-data))))

(defun normalize-model-name (model-string)
  (let ((canonical (cond ((string-equal model-string "gemini-3-pro-preview") "gemini-3.0-pro-preview")
                         (model-string model-string)
                         (t "gemini-3.0-pro-preview"))))
    (intern (string-upcase canonical) :keyword))) 

(defun calculate-cost-from-log (log-s-expression &optional (pricing *gemini-pricing*))
  "Calculates cost for a single log entry."
  (let* ((model-alist (assoc :modelversion log-s-expression))
         (model-string (if model-alist (cdr model-alist) "gemini-3-pro-preview"))
         (input-tokens (cdr (assoc :prompt-token-count log-s-expression)))
         (output-raw   (cdr (assoc :candidates-token-count log-s-expression)))
         (output-tokens (if (numberp output-raw) output-raw 0))
         (model-keyword (normalize-model-name model-string)))
    (unless (and input-tokens output-tokens)
      (error "Missing token counts in log entry."))
    (let* ((pricing-data (get-pricing-data model-keyword pricing))
           (input-cost (* (/ input-tokens 1000000.0) (get-cost-per-m pricing-data :input-cost-per-m)))
           (output-cost (* (/ output-tokens 1000000.0) (get-cost-per-m pricing-data :output-cost-per-m))))
      (values (+ input-cost output-cost) input-cost output-cost model-string input-tokens output-tokens)))) 

(defun process-file (pathname &optional (pricing *gemini-pricing*))
  "Processes a single .tkn file and prints its subtotal."
  (let ((forms (uiop:read-file-forms pathname))
        (f-cost 0.0) (f-in 0) (f-out 0))
    (loop for entry in forms do
      (multiple-value-bind (cost i-c o-c model in out) (calculate-cost-from-log entry pricing)
        (declare (ignore i-c o-c model))
        (incf f-cost cost)
        (incf f-in in)
        (incf f-out out)))
    (format t "~&File: ~A [In: ~:D | Out: ~:D | Cost: $~4,4F]" 
            (file-namestring pathname) f-in f-out f-cost)
    (values forms f-cost f-in f-out)))

(defun process-command-line-targets (targets &optional (pricing *gemini-pricing*))
  "Handles multiple files or directories, aggregating all results."
  (let ((all-forms nil) (total-cost 0.0) (total-in 0) (total-out 0) (last-model "N/A"))
    (dolist (target targets)
      (let ((path (pathname target)))
        (cond ((uiop:directory-exists-p path)
               (dolist (file (uiop:directory-files path "*.tkn"))
                 (multiple-value-bind (forms cost in out) (process-file file pricing)
                   (setf all-forms (nconc all-forms forms))
                   (incf total-cost cost)
                   (incf total-in in)
                   (incf total-out out))))
              ((uiop:file-exists-p path)
               (multiple-value-bind (forms cost in out) (process-file path pricing)
                 (setf all-forms (nconc all-forms forms))
                 (incf total-cost cost)
                 (incf total-in in)
                 (incf total-out out)))
              (t (format *error-output* "~&Warning: ~A not found.~%" target)))))
    (when all-forms
      (format t "~&~%=== Aggregated API Cost Report ===")
      (format t "~&Total Input Tokens:  ~:D" total-in)
      (format t "~&Total Output Tokens: ~:D" total-out)
      (format t "~&Total Grand Cost:    $~8,6F" total-cost)
      (format t "~&==================================~%"))
    (values total-cost total-in total-out last-model))) 

(defun top ()
  "Handles command line arguments from sb-ext:*posix-argv*."
  (let ((args (rest sb-ext:*posix-argv*)))
    (if args
        (process-command-line-targets args)
        (process-command-line-targets '("."))))) 

(defun save-core ()
  (sb-ext:save-lisp-and-die "gemini-cost-calculator" :toplevel #'top :executable t))
