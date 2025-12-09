all: gemini-calculate-cost gemini-chat-dev

gemini-chat-dev: gemini-chat.lisp gemini-chat.asd gemini-chat-pkg.lisp gemini-chat-lib-pkg.lisp gemini-chat-lib.asd gemini-chat-lib.lisp
	sbcl  --dynamic-space-size 2000 --disable-debugger  --eval "(asdf:operate 'asdf:load-op 'gemini-chat)"   --eval '(in-package #:gemini-chat)'  --eval '(save-core-uncompressed)'
	touch gemini-chat-dev

gemini-chat: gemini-chat.lisp gemini-chat.asd gemini-chat-pkg.lisp gemini-chat-lib-pkg.lisp gemini-chat-lib.asd gemini-chat-lib.lisp
	sbcl  --dynamic-space-size 2000 --disable-debugger  --eval "(asdf:operate 'asdf:load-op 'gemini-chat)"   --eval '(in-package #:gemini-chat)'  --eval '(save-core)'

~/bin/gemini-chat: gemini-chat
		cp -v gemini-chat ~/bin/gemini-chat

~/bin/gemini-cost-calculator: gemini-cost-calculator
		cp -v gemini-cost-calculator ~/bin/gemini-cost-calculator
install: ~/bin/gemini-chat ~/bin/gemini-cost-calculator

gemini-calculate-cost: gemini-cost-calculator.asd gemini-cost-calculator.lisp gemini-cost-calculator-pkg.lisp gemini-cost-calculator-sample-usage.lisp
	sbcl  --dynamic-space-size 2000 --disable-debugger  --eval "(asdf:operate 'asdf:load-op 'gemini-cost-calculator)"   --eval '(in-package #:gemini-cost-calculator)'  --eval '(save-core)'
