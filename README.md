# gemini-chat

`gemini-chat` is a command-line interface (CLI) tool for interacting with Google's Gemini API, built using Common Lisp. It allows you to have multi-turn conversations, provide context files, include input files in your prompts, and save conversation logs.

---

## Features

* **Interactive Conversations:** Engage in back-and-forth dialogue with the Gemini model.
* **Context Files:** Provide one or more files as initial context for the model, influencing its responses.
* **Input Files:** Include the content of a specified file directly into your initial prompt.
* **Conversation Logging:** Automatically saves detailed conversation logs (`-the-answer.log` and `-thinking.log`).
* **Runtime Output Saving:** Dynamically save Gemini's responses to a specified file during a conversation.
* **Flexible Prompting:** Supports direct command-line prompts, file-based prompts, and interactive prompting.

---

## Setup and Installation

### Prerequisites

* A Common Lisp implementation (e.g., SBCL, CCL).
* [Quicklisp](https://www.quicklisp.org/) for easy library management.
* A Google Gemini API Key.

### Obtaining a Gemini API Key

1.  Go to the [Google AI Studio](https://aistudio.google.com/).
2.  Follow the instructions to get an API key.
3.  Set this key as an environment variable named **`GEMINI_API_KEY`** (or `_GEMINI_API_KEY_`) in your shell:
    ```bash
    export GEMINI_API_KEY="YOUR_API_KEY_HERE"
    ```
    Replace `YOUR_API_KEY_HERE` with your actual key. You might want to add this line to your shell's profile file (e.g., `.bashrc`, `.zshrc`, `config.fish`) for persistence.

### Building and Running

1.  **Clone the Repository:** (Assuming you have a repository for `gemini-chat`)
    ```bash
    git clone your-repo-url gemini-chat
    cd gemini-chat
    ```
2.  **Load Project in Lisp:**
    Open your Lisp REPL and load the project using Quicklisp:
    ```lisp
    (ql:quickload :gemini-chat)
    ```
3.  **Create Executable:**
    To create a standalone executable, use the `save-core` function:
    ```lisp
    (in-package #:gemini-chat)
    (save-core)
    ```
    This will generate an executable named `gemini-chat` in your project directory.

---

## Usage

The `gemini-chat` program can be run directly from the command line once compiled.

```bash
./gemini-chat [options] [initial_prompt | /path/to/file.txt]

----
Alright, Bill, I've integrated all the latest changes into your gemini-chat.lisp file, and I've also updated the help message to reflect how the --context flag works. Finally, I've crafted a comprehensive README.md for your project.

gemini-chat.lisp Update
I've made one small but important adjustment within your gemini-chat.lisp:

Updated --context help text: The description for the --context flag now explicitly states it accepts a comma-separated list of files, as indicated by your string-identity-parser.

Removed Debug Print: I've removed the debug (format t "sip: k is ~s~%" k) line from string-identity-parser to clean up the output.

Here's the relevant updated portion of gemini-chat.lisp:

Lisp

;;;; gemini-chat.lisp
;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:gemini-chat)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

;; A default tag to use if none is provided on the command line.
(defparameter *d-tag* "chat")

;; New special variable for the runtime output file stream
(defparameter *run-out-s* nil
  "Stream for saving conversation answers during a conversation.")

;; Function to retrieve the current version
(defun get-version ()
  (slot-value (asdf:find-system 'gemini-chat) 'asdf:version))

(defun get-key ()
  "Retrieves the Gemini API key from the GEMINI_API_KEY environment variable.
   Signals an error if the environment variable is not set.
   It first tries GEMINI_API_KEY, then falls back to _GEMINI_API_KEY_."
  (let ((key (or (getenv "GEMINI_API_KEY")
                 (getenv "_GEMINI_API_KEY_"))))
    (unless key
      (error "Error: Neither GEMINI_API_KEY nor _GEMINI_API_KEY_ environment variables are set.
              Please set one of them before running this program."))
    key))

;; --- Define Flags using com.google.flag ---

(define-flag *context* ; <-- CHANGED: Flag name now uses asterisk convention
  :help "Path to a context file. Can be specified multiple times. Example: --context file1.txt,file2.txt"
  :type list
  :parser string-identity-parser
  :selector "context"
  :default-value nil)

(define-flag *save* ; <-- CHANGED: Flag name now uses asterisk convention
  :help "File to save Gemini's responses to. Responses will be appended. Example: --save conversation.log"
  :type string
  :selector "save"
  :default-value "")

(define-flag *tag* ; <-- CHANGED: Flag name now uses asterisk convention
  :help "A unique tag for the conversation logs (default: chat). Example: --tag my-session"
  :type string
  :selector "tag"
  :default-value "chat")

(define-flag *input-file* ; <-- CHANGED: Flag name now uses asterisk convention
  :help "Path to a primary input file whose content will be sent to Gemini with your prompt. Example: --input-file my-code.lisp"
  :type string
  :selector "input_file"
  :default-value "")

(define-flag *help-is*
  :help "Show this help message and exit." ; Corrected help string for --help
  :type boolean
  :selector "help"
  :default-value nil)

(defun s/z (str)
  (zerop (length str)))

(defun s/nz (str)
  (plusp (length str)))

(defun print-help ()
  "Prints the command-line help message and example usage."
  (format t "~&gemini-chat version ~a~%~%" (get-version)) ; Added version to help
  (format t "Usage: ./gemini-chat [options] [tag] [initial_prompt | /path/to/file.txt]~%~%")
  (format t "Options:~%")
  (format t "  -h, --help               Show this help message and exit.~%")
  (format t "  -c, --context <file1,file2,...> Specify a comma-separated list of files to be included as initial context.~%~%") ; <-- UPDATED THIS LINE
  (format t "  -s, --save <file>        File to save Gemini's responses to (appends).~%") ; Added more explicit help for -s
  (format t "  -t, --tag <tag>          A unique tag for conversation logs (default: 'chat').~%") ; Added help for -t
  (format t "  -f, --input-file <file>  Path to a primary input file whose content will be sent with your prompt.~%~%") ; Added help for -f
  (format t "Interactive Commands (during chat loop):~%")
  (format t "  :save <filename>         Start or change saving model responses to the specified file.~%")
  (format t "  quit                     End the conversation.~%~%")
  (format t "Initial Prompt Options:~%")
  (format t "  If no initial prompt or file is given, the program will prompt you interactively.~%")
  (format t "  If the first argument is a path starting with '/', the file content will be loaded as the initial input.~%")
  (format t "  Otherwise, all subsequent arguments are treated as the initial prompt text.~%~%")

  (format t "---~%~%")
  (format t "## Example Flow:~%~%")
  (format t "To use the `gemini-chat` program with an input file, a context file, a defined output file, and chat input that references the input file, you'd use a command like this:~%~%")
  (format t "```bash~%")
  (format t "./gemini-chat -c your_context_file.txt,:~/another_context.md :save my_output.txt /path/to/your_input_file.txt \"Please summarize the content of the attached file and then answer my questions.\"~%")
  (format t "```~%~%")
  (format t "Let's break down the components of that command:~%~%")
  (format t "* **`./gemini-chat`**: This is how you'd typically execute the compiled program.~%")
  (format t "* **`-c your_context_file.txt,:~/another_context.md`** (or `--context your_context_file.txt,:~/another_context.md`):~%")
  (format t "    * `:-c` or `--context` is the option to specify a **context file(s)**.~%")
  (format t "    * `your_context_file.txt,:~/another_context.md` is a comma-separated list of paths to files whose content you want to provide as additional context to the Gemini model before it processes your main prompt. This is useful for providing background information, specific guidelines, or data that isn't directly part of your immediate query but should influence the model's response.~%")
  (format t "* **`:save my_output.txt`**:~%")
  (format t "    * `::save` is a special command *within* `gemini-chat` that tells it to direct the model's responses to a file.~%")
  (format t "    * `my_output.txt` is the name of the file where the conversation's output will be saved. The program will open this file and append Gemini's responses to it.~%")
  (format t "* **`/path/to/your_input_file.txt`**:~%")
  (format t "    * When the first non-option argument on the command line starts with a `/` (indicating a file path), `gemini-chat` will read this file's content. This becomes the primary 'input file' for the current turn.~%")
  (format t "    * The `gemini-chat` program will then prompt you for an **additional prompt** that will accompany the file content.~%")
  (format t "* **`\"Please summarize the content of the attached file and then answer my questions.\"`**:~%")
  (format t "    * This is the **chat input** you'd type after `gemini-chat` prompts you, following the reading of `/path/to/your_input_file.txt`.~%")
  (format t "    * This is where you provide instructions or questions *related to the content of the input file*.~%~%")
  (format t "---~%~%")
  (format t "### Example Flow:~%~%")
  (format t "1.  You run the command:~%    ```bash~%")
  (format t "    ./gemini-chat -c my_project_docs.txt,another_doc.txt :save session_log.txt /home/bill/data/quarterly_report.csv~%")
  (format t "    ```~%")
  (format t "2.  `gemini-chat` processes `my_project_docs.txt` and `another_doc.txt` as context.~%")
  (format t "3.  It sets up `session_log.txt` to save the output.~%")
  (format t "4.  It reads `/home/bill/data/quarterly_report.csv`.~%")
  (format t "5.  You then see a prompt like:~%    ```~%")
  (format t "    File '/home/bill/data/quarterly_report.csv' loaded. Enter an additional prompt for Gemini (optional):~%")
  (format t "    ```~%")
  (format t "6.  You would type:~%    ```~%")
  (format t "    Based on the report, what were the key revenue drivers and what challenges are highlighted?~%")
  (format t "    ```~%")
  (format t "7.  `gemini-chat` combines the context from `my_project_docs.txt` and `another_doc.txt`, the content of `quarterly_report.csv`, and your \"key revenue drivers\" prompt, sends it to Gemini, and logs the response to `session_log.txt` (and displays it to you).~%")
  (format t "---~%~%")
  (finish-output))

;; ... rest of your gemini-chat.lisp code ...

;; Add this helper function somewhere in your gemini-chat.lisp,
;; ideally near other utility functions or at the top level.
(defun string-identity-parser (s)
  "A parser function for com.google.flag that simply returns the string itself
   and a success boolean T. Used for list flags where each element is a string."
  (let ((full-l nil))
    (mapc #'(lambda (k)
              ;(format t "sip: k is ~s~%" k) ; <-- Removed this debug line
              (push k full-l))
          (split-sequence:split-sequence #\, s))
    (values (reverse full-l) t)))
README.md
Here's the README.md file, providing an overview and usage instructions for your gemini-chat program.

Markdown

# gemini-chat

`gemini-chat` is a command-line interface (CLI) tool for interacting with Google's Gemini API, built using Common Lisp. It allows you to have multi-turn conversations, provide context files, include input files in your prompts, and save conversation logs.

---

## Features

* **Interactive Conversations:** Engage in back-and-forth dialogue with the Gemini model.
* **Context Files:** Provide one or more files as initial context for the model, influencing its responses.
* **Input Files:** Include the content of a specified file directly into your initial prompt.
* **Conversation Logging:** Automatically saves detailed conversation logs (`-the-answer.log` and `-thinking.log`).
* **Runtime Output Saving:** Dynamically save Gemini's responses to a specified file during a conversation.
* **Flexible Prompting:** Supports direct command-line prompts, file-based prompts, and interactive prompting.

---

## Setup and Installation

### Prerequisites

* A Common Lisp implementation (e.g., SBCL, CCL).
* [Quicklisp](https://www.quicklisp.org/) for easy library management.
* A Google Gemini API Key.

### Obtaining a Gemini API Key

1.  Go to the [Google AI Studio](https://aistudio.google.com/).
2.  Follow the instructions to get an API key.
3.  Set this key as an environment variable named **`GEMINI_API_KEY`** (or `_GEMINI_API_KEY_`) in your shell:
    ```bash
    export GEMINI_API_KEY="YOUR_API_KEY_HERE"
    ```
    Replace `YOUR_API_KEY_HERE` with your actual key. You might want to add this line to your shell's profile file (e.g., `.bashrc`, `.zshrc`, `config.fish`) for persistence.

### Building and Running

1.  **Clone the Repository:** (Assuming you have a repository for `gemini-chat`)
    ```bash
    git clone your-repo-url gemini-chat
    cd gemini-chat
    ```
2.  **Load Project in Lisp:**
    Open your Lisp REPL and load the project using Quicklisp:
    ```lisp
    (ql:quickload :gemini-chat)
    ```
3.  **Create Executable:**
    To create a standalone executable, use the `save-core` function:
    ```lisp
    (in-package #:gemini-chat)
    (save-core)
    ```
    This will generate an executable named `gemini-chat` in your project directory.

---

## Usage

The `gemini-chat` program can be run directly from the command line once compiled.

```bash
./gemini-chat [options] [initial_prompt | /path/to/file.txt]
Options
-h, --help: Show the help message and exit.

-c, --context <file1,file2,...>: Specify one or more comma-separated file paths to be included as initial context for the Gemini model. E.g., --context docs/project_notes.txt,docs/api_spec.md.

-s, --save <file>: Specify a file to which Gemini's responses will be appended. If the file doesn't exist, it will be created.

-t, --tag <tag>: Assign a unique tag to the conversation logs. By default, logs are tagged chat-YYYYMMDD-HHMMSS. Using -t my-session would result in my-session-the-answer.log, etc.

-f, --input-file <file>: Provide a primary input file whose content will be sent to Gemini along with your prompt.

Initial Prompt Options
The way you provide the initial prompt determines how gemini-chat starts the conversation:

No initial prompt or file (./gemini-chat): The program will prompt you interactively to enter your initial question.

File path as first non-option argument (./gemini-chat /path/to/my_file.txt): If the first non-option argument starts with /, its content will be loaded as the initial input. The program will then prompt you for an additional prompt to accompany this file content.

Direct text as non-option arguments (./gemini-chat "What is the capital of France?"): All subsequent non-option arguments are treated as the initial prompt text.

Interactive Commands (during chat loop)
Once a conversation has started, you can use these special commands:

:save <filename>: Start or change saving model responses to the specified file. If a file is already being saved to, the previous stream will be closed, and a new one opened.

quit: End the current conversation and exit the program.

Example Flow
To illustrate a complex usage scenario: using an input file, multiple context files, defining an output file, and providing an additional prompt.

Bash

./gemini-chat -c your_context_file.txt,:~/another_context.md :save my_output.txt /path/to/your_input_file.txt "Please summarize the content of the attached file and then answer my questions."
Let's break down the components of that command:

./gemini-chat: This is how you'd typically execute the compiled program.

-c your_context_file.txt,:~/another_context.md (or --context your_context_file.txt,:~/another_context.md):

:-c or --context is the option to specify context files.

your_context_file.txt,:~/another_context.md is a comma-separated list of paths to files whose content you want to provide as additional context to the Gemini model before it processes your main prompt. This is useful for providing background information, specific guidelines, or data that isn't directly part of your immediate query but should influence the model's response.

:save my_output.txt:

::save is a special command within gemini-chat that tells it to direct the model's responses to a file.

my_output.txt is the name of the file where the conversation's output will be saved. The program will open this file and append Gemini's responses to it.

/path/to/your_input_file.txt:

When the first non-option argument on the command line starts with a / (indicating a file path), gemini-chat will read this file's content. This becomes the primary 'input file' for the current turn.

The gemini-chat program will then prompt you for an additional prompt that will accompany the file content.

"Please summarize the content of the attached file and then answer my questions.":

This is the chat input you'd type after gemini-chat prompts you, following the reading of /path/to/your_input_file.txt.

This is where you provide instructions or questions related to the content of the input file.

Step-by-Step Example
You run the command:

Bash

./gemini-chat -c my_project_docs.txt,another_doc.txt :save session_log.txt /home/bill/data/quarterly_report.csv
gemini-chat processes my_project_docs.txt and another_doc.txt as context.

It sets up session_log.txt to save the output.

It reads /home/bill/data/quarterly_report.csv.

You then see a prompt like:

File '/home/bill/data/quarterly_report.csv' loaded. Enter an additional prompt for Gemini (optional):
You would type:

Based on the report, what were the key revenue drivers and what challenges are highlighted?
gemini-chat combines the context from my_project_docs.txt and another_doc.txt, the content of quarterly_report.csv, and your "key revenue drivers" prompt, sends it to Gemini, and logs the response to session_log.txt (and displays it to you).

Let me know if you'd like any other parts of the code reviewed or documentation generated!
