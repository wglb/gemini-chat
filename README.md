# gemini-chat
### By <wgl@ciex-security.com>

This is a project to do conversation with gemini.google.com with conversation history.

## License

GPLv3

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


