gemini-chat-dev: gemini-chat.lisp gemini-chat.asd gemini-chat-pkg.lisp
	sbcl  --dynamic-space-size 2000 --disable-debugger  --eval "(asdf:operate 'asdf:load-op 'gemini-chat)"   --eval '(in-package #:gemini-chat)'  --eval '(save-core-uncompressed)'
	touch gemini-chat-dev

gemini-chat: gemini-chat.lisp gemini-chat.asd gemini-chat-pkg.lisp
	sbcl  --dynamic-space-size 2000 --disable-debugger  --eval "(asdf:operate 'asdf:load-op 'gemini-chat)"   --eval '(in-package #:gemini-chat)'  --eval '(save-core)'

~/bin/gemini-chat: gemini-chat
		cp -v gemini-chat ~/bin/gemini-chat

install: ~/bin/gemini-chat
