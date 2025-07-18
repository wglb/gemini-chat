---
## CHANGELOG.md

### Version 1.1.2 - 2025-07-18

This release significantly enhances the command-line interface for `gemini-chat`, providing more flexible ways to initiate conversations and analyze files directly from your terminal.

#### New Features

* **Flexible Command-Line Argument Parsing**: The `gemini-top` function has been refactored to better distinguish between conversation tags, file paths, and direct prompts.
    * You can now specify a conversation tag, an optional file path, and your initial prompt all as command-line arguments.
    * Examples:
        * `gemini-chat my-log-tag "Explain quantum physics."`
        * `gemini-chat my-analysis /path/to/my-code.lisp "Analyze this code for potential bugs."`
        * `gemini-chat /path/to/my-notes.txt "Summarize the key points in this document."`
* **Intelligent File Detection**: The application now intelligently checks if an argument provided on the command line is an existing file. If it is, the file's content will be automatically included in your initial prompt to Gemini.
* **Improved Interactive Fallback**: If no arguments are provided, or only a tag, the application will still guide you through an interactive process to either provide a file path or enter a direct question.

#### Improvements

* **Cleaner Prompt Formatting**: Leading/trailing whitespace is now automatically trimmed from command-line prompts, ensuring cleaner input to the Gemini API.
* **Explicit Default Tag**: A new `*default-conversation-tag*` parameter ensures consistent use of "chat" as the default log file tag when no custom tag is specified.

