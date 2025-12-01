(defun extract-txt (parsed-json)
  "Extracts the generated text from the parsed Gemini API JSON response using jsown accessors.
Returns the text string or NIL if not found."

  ;; --- START Token Count Extraction ---
  (let ((usage-metadata (jsown:val-safe parsed-json "usageMetadata")))
    (when usage-metadata
      (let ((prompt-tokens (jsown:val-safe usage-metadata "promptTokenCount"))
            (candidate-tokens (jsown:val-safe usage-metadata "candidatesTokenCount"))
            (total-tokens (jsown:val-safe usage-metadata "totalTokenCount"))
            (thoughts-tokens (jsown:val-safe usage-metadata "thoughtsTokenCount"))
            (details (jsown:val-safe usage-metadata "promptTokensDetails")))
        (let ((token-log-data (list
                               (cons 'prompt-token-count (or prompt-tokens "N/A"))
                               (cons 'candidates-token-count (or candidate-tokens "N/A"))
                               (cons 'total-token-count (or total-tokens "N/A"))
                               (cons 'thoughts-token-count (or thoughts-tokens "N/A"))
                               (cons 'prompt-tokens-details (or details "N/A")))))
          (xlg :thinking-log "--- Token Usage (Last Response) --- ~S" token-log-data)
          (xlg :thinking-log "-----------------------------------")))))
  ;; --- END Token Count Extraction ---
  
  (cond ((jsown:keyp parsed-json "error")
...
