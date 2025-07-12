# gemini-chat
### By <wgl@ciex-security.com>

This is a project to do conversation with gemini.google.com with conversation history.

## License

GPLv3

## Construction

This was an experiment with gemini to generate a command line interface to Gemini chat.

I asked gemini how to build agents for programming. It started out with
emacs code, then I steered it to lisp, sbcl in particular. 

The first version started with one query per conversation. I asked to have
the conversations retained. In that case, it pumps the previously asked
questions in front of the latest query.

There were lots of struggles with cl-jsown and getting it to do the proper
translation between lisp expressions and json. Finally, it reverted to
"(format nil ...)".

I then suggested jsown.  After several iterations, the process was
successful.

## Components

This also needs github.com:wglb/xlg.git for logging, mostly for creating
the user interaction and the "thinking" that gemini and client are doing.

## Installing

To build the executable (sbcl compressed), do

make

To install in your ~/bin directory, type 

make install

## Usage

From the command line do

gemini-chat tag First Prompt

Where 'tag' is the prefix to the answer and the thinking file. These log
files are date stamped.

The "First Prompt" goes to gemini.

# API Key

You need to obtain a GEMINI_API_KEY and set it in your shell environment.
