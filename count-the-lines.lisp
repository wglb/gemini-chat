Of course! Here is a complete and idiomatic Common Lisp solution to count lines in a directory of Lisp files.

This solution is broken down into two functions:
1.  A helper function to count lines in a single file.
2.  The main function that finds all Lisp files in a directory and uses the helper to sum their line counts.

This approach is clean, reusable, and easy to understand.

### The Code

```lisp
(defun count-lines-in-file (pathname)
  "Counts the number of lines in a single file specified by PATHNAME.
   Returns 0 if the file cannot be opened."
  (handler-case
      (with-open-file (stream pathname :direction :input)
        (loop for line = (read-line stream nil)
              while line
              count 1))
    ;; If the file can't be read (e.g., permissions error),
    ;; don't crash. Just print a warning and return 0.
    (error (c)
      (warn "Could not count lines in ~a: ~a" pathname c)
      0)))

(defun count-lisp-lines-in-directory (directory-path)
  "Counts the total number of lines in all '.lisp' files in a given directory.
   The DIRECTORY-PATH should be a string pointing to a directory."
  ;; Create a pathname that represents all '.lisp' files in the target directory.
  ;; This is the portable and correct way to handle file paths.
  (let ((lisp-files-wildcard (make-pathname :name :wild
                                            :type "lisp"
                                            :defaults directory-path)))
    ;; The LOOP macro is perfect for this task.
    (loop
      ;; 1. Iterate over each file found by the DIRECTORY function.
      for file in (directory lisp-files-wildcard)
      ;; 2. For each file, call our helper function and sum the results.
      summing (count-lines-in-file file))))
```

### How It Works: Step-by-Step

#### 1. `count-lines-in-file` (The Helper)

*   **`handler-case`**: This is a robust way to handle potential errors. If trying to open or read the file fails (e.g., permission denied), it will "catch" the error, print a warning using `warn`, and return `0` instead of crashing the program.
*   **`with-open-file`**: This is the standard, safe way to work with files in Lisp. It automatically opens the file and guarantees that it will be closed when the block is exited, even if an error occurs.
*   **`loop for line = (read-line stream nil) while line count 1`**: This is a classic Lisp idiom for iterating through a file.
    *   `read-line stream nil`: Reads one line from the `stream`. The crucial part is the second argument, `nil`. It tells `read-line` to return `nil` when it reaches the end of the file, instead of signaling an error.
    *   `while line`: The loop continues as long as `read-line` returns a value (i.e., not `nil`).
    *   `count 1`: For every successful line read, the loop's internal counter is incremented by 1. The final value of this counter is what the `loop` expression returns.

#### 2. `count-lisp-lines-in-directory` (The Main Function)

*   **`make-pathname`**: This is the key to portability. Instead of manually concatenating strings like `"my-dir/*.lisp"`, which can fail on different operating systems, `make-pathname` builds a proper path object.
    *   `:name :wild`: Corresponds to the `*` (wildcard) for the filename.
    *   `:type "lisp"`: Specifies that we only want files with the `.lisp` extension.
    *   `:defaults directory-path`: Uses the provided directory as the base for the search.
*   **`(directory lisp-files-wildcard)`**: This function takes a wildcard pathname and returns a list of all actual pathnames that match it.
*   **`loop for file in ... summing ...`**: The `loop` macro shines here.
    *   `for file in (directory ...)`: It iterates through the list of Lisp file pathnames.
    *   `summing (count-lines-in-file file)`: For each `file`, it calls our helper function and adds the result to a running total. The `loop` automatically manages and returns this final sum.

### How to Use

1.  **Save the Code**: Save the functions above into a file, for example, `line-counter.lisp`.

2.  **Load it into your Lisp environment**:
    ```lisp
    (load "line-counter.lisp")
    ```

3.  **Create some test files**:
    Let's say you have a directory `/tmp/my-lisp-project/` with the following files:

    *   `file1.lisp` (contains 10 lines)
    *   `file2.lisp` (contains 25 lines)
    *   `notes.txt` (contains 100 lines - should be ignored)
    *   `utils.lisp` (contains 50 lines)

4.  **Call the function**:

    ```lisp
    ;; Make sure to include the trailing slash for directories!
    (count-lisp-lines-in-directory "/tmp/my-lisp-project/")
    ```

    **Expected Output**:

    ```
    85
    ```

### Advanced Version: Recursive Search

If you want to count lines in the specified directory *and all of its subdirectories*, you would need a recursive approach. The easiest and most robust way to do this is by using the `UIOP` library, which is part of ASDF and available in virtually all modern Common Lisp implementations.

```lisp
;; You may need to (ql:quickload :uiop) if it's not already loaded
(defun count-lisp-lines-recursively (directory-path)
  "Recursively counts lines in all '.lisp' files in a directory and its subdirectories."
  (loop
    for file in (uiop:directory-files directory-path)
    when (string-equal "lisp" (pathname-type file))
      summing (count-lines-in-file file)
    into total
    finally (return
              (+ total
                 (loop for subdir in (uiop:subdirectories directory-path)
                       summing (count-lisp-lines-recursively subdir))))))

;; --- OR a more concise version using uiop:collect-sub*directories ---

(defun count-lisp-lines-recursively-simple (directory-path)
  "A simpler recursive version using UIOP's directory walking utilities."
  (let ((all-files (uiop:collect-sub*directories
                    directory-path
                    t     ; include the starting directory
                    t     ; follow symlinks
                    (lambda (p) (string-equal "lisp" (pathname-type p))))))
    (loop for file in all-files
          summing (count-lines-in-file file))))
```
This recursive version demonstrates how you can build upon the simple helper function to create more powerful tools.
