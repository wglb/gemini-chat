;; test/gemini-chat-test.lisp
(in-package #:gemini-chat-test)

(defun create-test-files (file-list temp-dir)
  "Creates temporary test files in a specified directory."
  (ensure-directories-exist temp-dir)
  (loop for (filename content) in file-list
        do (let ((full-path (merge-pathnames filename temp-dir)))
             (with-open-file (out full-path
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
               (princ content out)))))

(defun delete-test-files (file-list temp-dir)
  "Deletes temporary test files from a specified directory."
  (loop for (filename content) in file-list
        do (let ((full-path (merge-pathnames filename temp-dir)))
             (when (probe-file full-path)
               (delete-file full-path)))))

(def-suite gemini-chat-suite
  :description "The main test suite for the gemini-chat application.")

(in-suite gemini-chat-suite)

(test pack-and-unpack-roundtrip
  "Tests the round-trip functionality of packing and unpacking files."
  (let* ((temp-dir (uiop:temporary-directory))
         (test-files-list '(("file1.txt" "This is the content of file one.")
                            ("file2.lisp" "(defun hello () (format t \"Hello, World!\"))")
                            ("file3.md" "# Test Markdown File\n\n- Item 1\n- Item 2")))
         (packed-file-path (merge-pathnames "packed-test-files.lisp" temp-dir)))
    
    ;; Setup: create the files to be packed
    (create-test-files test-files-list temp-dir)
    
    ;; Test: pack the files using the new file-packer library's public function
    (with-open-file (out packed-file-path
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (file-packer:pack-files-to-stream (mapcar #'(lambda (x) (merge-pathnames (car x) temp-dir)) test-files-list)
                                        out))
    
    ;; Assert: the packed file exists and is not empty
    (is-true (probe-file packed-file-path))
    (is-true (> (with-open-file (in packed-file-path)
                  (file-length in))
                0))

    ;; Test: unpack the files
    (with-open-file (in packed-file-path :direction :input)
      (file-packer:unpack-files-from-stream in))
      
    ;; Assert: the unpacked files exist and have the correct content
    (loop for (filename content) in test-files-list
          do (let ((unpacked-path (merge-pathnames filename temp-dir)))
               (is-true (probe-file unpacked-path))
               (is (string= content (uiop:read-file-string unpacked-path)))))

    ;; Cleanup: delete all temporary files
    (delete-file packed-file-path)
    (delete-test-files test-files-list temp-dir)))

(defun run-tests ()
  "Runs all tests in the gemini-chat test suite."
  (run! 'gemini-chat-suite))
