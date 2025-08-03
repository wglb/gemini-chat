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
--help: Show the help message and exit.

--context <file1,file2,...>: Specify one or more comma-separated file paths to be included as initial context for the Gemini model. E.g., --context docs/project_notes.txt,docs/api_spec.md.

--save <file>: Specify a file to which Gemini's responses will be appended. If the file doesn't exist, it will be created.

--tag <tag>: Assign a unique tag to the conversation logs. By default, logs are tagged chat-YYYYMMDD-HHMMSS. Using -t my-session would result in my-session-the-answer.log, etc.

--input-file <file>: Provide a primary input file whose content will be sent to Gemini along with your prompt.

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

--context your_context_file.txt,:~/another_context.md (or --context your_context_file.txt,:~/another_context.md):

:--context is the option to specify context files.

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

./gemini-chat -c my_project_docs.txt,another_doc.txt :save session_log.txt /home/<your home directory>/data/quarterly_report.csv
gemini-chat processes my_project_docs.txt and another_doc.txt as context.

It sets up session_log.txt to save the output.

It reads /home/<your home directory>/data/quarterly_report.csv.

You then see a prompt like:

File '/home/<your home directory>/data/quarterly_report.csv' loaded. Enter an additional prompt for Gemini (optional):
You would type:

Based on the report, what were the key revenue drivers and what challenges are highlighted?
gemini-chat combines the context from my_project_docs.txt and another_doc.txt, the content of quarterly_report.csv, and your "key revenue drivers" prompt, sends it to Gemini, and logs the response to session_log.txt (and displays it to you).

