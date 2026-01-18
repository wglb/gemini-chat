(in-package :gemini-cost-calculator)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

(defparameter *gemini-pricing*
  '((:GEMINI-3.0-PRO-PREVIEW . ((:INPUT-COST-PER-M 2.0) (:OUTPUT-COST-PER-M 12.0)))
    (:GEMINI-3-PRO-PREVIEW .   ((:INPUT-COST-PER-M 2.0) (:OUTPUT-COST-PER-M 12.0)))
    (:GEMINI-2.5-PRO .         ((:INPUT-COST-PER-M 1.25) (:OUTPUT-COST-PER-M 10.0)))
    (:GEMINI-2.5-FLASH .       ((:INPUT-COST-PER-M 0.3) (:OUTPUT-COST-PER-M 2.5)))
    ;; Added for the new Lite model
    (:GEMINI-2.5-FLASH-LITE .  ((:INPUT-COST-PER-M 0.1) (:OUTPUT-COST-PER-M 0.4)))
    (:N/A .                    ((:INPUT-COST-PER-M 0.3) (:OUTPUT-COST-PER-M 2.5)))))

;; In gemini-cost-calculator.lisp

(defparameter *async-pricing*
  '((:GEMINI-2.5-PRO .         ((:INPUT-COST-PER-M 0.625) (:OUTPUT-COST-PER-M 5.0)))
    (:GEMINI-2.5-FLASH .       ((:INPUT-COST-PER-M 0.015) (:OUTPUT-COST-PER-M 0.15)))
    (:GEMINI-2.5-FLASH-LITE .  ((:INPUT-COST-PER-M 0.05)  (:OUTPUT-COST-PER-M 0.2)))
    (:N/A .                    ((:INPUT-COST-PER-M 0.15)  (:OUTPUT-COST-PER-M 1.25))))
  "Pricing for Async (Batch) jobs, typically 50% of the standard rate.")

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

(defun calculate-cost-from-log (log-s-expression &key (async nil))
  "Calculates cost for a single log entry, choosing pricing based on the :async flag."
  (let* ((pricing (if async *async-pricing* *gemini-pricing*))
         (model-alist (assoc :modelversion log-s-expression))
         (model-string (if model-alist (cdr model-alist) "gemini-2.5-pro"))
         (input-tokens (cdr (assoc :prompt-token-count log-s-expression)))
         (output-raw   (cdr (assoc :candidates-token-count log-s-expression)))
         (output-tokens (if (numberp output-raw) output-raw 0))
         (model-keyword (normalize-model-name model-string)))
    (unless (and input-tokens output-tokens)
      (error "Missing token counts in log entry: ~S" log-s-expression))
    (let* ((pricing-data (get-pricing-data model-keyword pricing))
           (input-cost (* (/ input-tokens 1000000.0) 
                          (get-cost-per-m pricing-data :input-cost-per-m)))
           (output-cost (* (/ output-tokens 1000000.0) 
                           (get-cost-per-m pricing-data :output-cost-per-m))))
      (values (+ input-cost output-cost) 
              input-cost 
              output-cost 
              model-string 
              input-tokens 
              output-tokens))))

(defun process-file (pathname &key (async nil))
  "Processes a single file. If async is T, uses discounted batch pricing."
  (let ((forms (uiop:read-file-forms pathname))
        (f-cost 0.0) (f-in 0) (f-out 0)
        (last-m "N/A"))
    (loop for entry in forms do
      (multiple-value-bind (cost i-c o-c model in out) 
          (calculate-cost-from-log entry :async async)
        (declare (ignore i-c o-c))
        (setf last-m model)
        (incf f-cost cost)
        (incf f-in in)
        (incf f-out out)))
    (format t "~&File: ~20A (~A) [In: ~:D | Out: ~:D | Cost: $~4,4F]" 
            (file-namestring pathname) 
            (if async "ASYNC" "LIVE ") 
            f-in f-out f-cost)
    (values forms f-cost f-in f-out last-m)))

;; Copyright 2026 Bill
(defun process-command-line-targets (targets)
  "Aggregates all results, automatically detecting async files by extension."
  (let ((all-forms nil) 
        (total-cost 0.0) 
        (total-in 0) 
        (total-out 0) 
        (last-model "N/A"))
    (dolist (target targets)
      (let ((path (pathname target)))
        (cond ((uiop:directory-exists-p path)
               ;; Look for standard live tokens and the new aggregated async tokens
               (dolist (file (append (uiop:directory-files path "*.tkn")
                                     (uiop:directory-files path "*.async-tkn")))
                 (let ((is-async (string= (pathname-type file) "async-tkn")))
                   (multiple-value-bind (forms cost in out model) 
                       (process-file file :async is-async)
                     (setf all-forms (nconc all-forms forms))
                     (incf total-cost cost)
                     (incf total-in in)
                     (incf total-out out)
                     (setf last-model model)))))
              
              ((uiop:file-exists-p path)
               (let ((is-async (string= (pathname-type path) "async-tkn")))
                 (multiple-value-bind (forms cost in out model) 
                     (process-file path :async is-async)
                   (setf all-forms (nconc all-forms forms))
                   (incf total-cost cost)
                   (incf total-in in)
                   (incf total-out out)
                   (setf last-model model))))
              
              (t (format *error-output* "~&Warning: ~A not found.~%" target)))))
    
    (when all-forms
      (format t "~&~%=== Aggregated API Cost Report ===")
      (format t "~&Total Input Tokens:  ~:D" total-in)
      (format t "~&Total Output Tokens: ~:D" total-out)
      (format t "~&Total Grand Cost:    $~8,6F" total-cost)
      (format t "~&Reference Model:     ~A" last-model)
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
