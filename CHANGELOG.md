---
## CHANGELOG.md

---
## CHANGELOG.md

### Version 1.3.1 - 2025-07-19

This release introduces significant enhancements for managing conversation context and controlling output logging.

#### New Features

* **Context File Inclusion**: You can now include the content of one or more files as initial context for Gemini. Use the command-line options `-c <filepath>` or `--context <filepath>`. This is ideal for providing code, documentation, or other background information at the start of a conversation.
* **Runtime Answer Logging**: A new interactive command, `:save <filename>`, lets you record Gemini's responses (and any API errors related to generating text) to a specified file during a conversation. This file will contain only the model's output, offering a clean log of the answers provided. Typing `:save` again will switch the logging to a new file or close the current one.

#### Bug Fixes

* **Corrected Command-Line Argument Access**: Fixed an issue where `sb-ext:*posix-argv*` was incorrectly invoked as a function. It is now correctly accessed as a special variable, ensuring that command-line arguments are parsed as intended. This resolves a potential runtime error when the executable starts up.

---

Version 1.2.3 - 2025-07-18
This patch release corrects a subtle but important error in how command-line arguments were processed, ensuring proper handling of command-line arguments.

Bug Fixes
Fixed an issue where sb-ext:*posix-argv* was incorrectly invoked as a function. It is now correctly accessed as a special variable, ensuring that command-line arguments are parsed as intended. 

### Version 1..2 - 2025-07-18

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

