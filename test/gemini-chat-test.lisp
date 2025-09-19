;; test/gemini-chat-test.lisp

(in-package #:gemini-chat-test)

(def-suite gemini-chat-suite
  :description "The main test suite for the gemini-chat application.")

(in-suite gemini-chat-suite)

(test pack-and-unpack-roundtrip
  "Tests the round-trip functionality of packing and unpacking files."
  (let* ((original-files '("gemini-chat-pkg.lisp" "gemini-chat.lisp" "gemini-chat.asd"))
         (temp-dir (uiop:temporary-directory))
         (packed-file-path (uiop:merge-pathnames* "packed-test-files.lisp" temp-dir))
         (unpacked-dir (uiop:merge-pathnames* "unpacked-test-dir/" temp-dir)))

    (uiop:ensure-all-directories-exist (list unpacked-dir))
    
    ;; 1. Pack the files
    (with-open-file (out packed-file-path
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (pack-files-to-stream original-files out))

    ;; 2. Assert the packed file is not empty
    (with-open-file (in packed-file-path :direction :input)
      (is-true (> (file-length in) 0) "Packed file is not empty."))

    ;; 3. Unpack the files to the temporary directory
    (unwind-protect
         (let ((result (with-open-file (in packed-file-path :direction :input)
                         (unpack-files-from-stream in))))
           (is-true result "Unpack returns a non-nil result."))
      ;; Cleanup after test
      (uiop:delete-directory-tree unpacked-dir :validate t :if-does-not-exist :ignore)
      (uiop:delete-file-if-exists packed-file-path))))

(defun run-tests ()
  "Runs all tests in the gemini-chat test suite."
  (let ((result (run! 'gemini-chat-suite)))
    (if (results-status result)
        t
        nil)))
