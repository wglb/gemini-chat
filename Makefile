# Revised Makefile for Bill
all: gemini-cost-calculator gemini-chat-dev

gemini-cost-calculator: gemini-cost-calculator.asd gemini-cost-calculator.lisp gemini-cost-calculator-pkg.lisp
	sbcl --dynamic-space-size 2000 --disable-debugger \
		--eval "(asdf:operate 'asdf:load-op 'gemini-cost-calculator)" \
		--eval '(in-package #:gemini-cost-calculator)' \
		--eval '(save-core)'

gemini-chat-dev: gemini-chat.lisp gemini-chat.asd gemini-chat-pkg.lisp gemini-chat-lib-pkg.lisp gemini-chat-lib.asd gemini-chat-lib.lisp
	sbcl --dynamic-space-size 2000 --disable-debugger \
		--eval "(asdf:operate 'asdf:load-op 'gemini-chat)" \
		--eval '(in-package #:gemini-chat)' \
		--eval '(save-core-uncompressed)'
	touch gemini-chat-dev

install: ~/bin/gemini-cost-calculator

~/bin/gemini-cost-calculator: gemini-cost-calculator
	cp -v gemini-cost-calculator ~/bin/gemini-cost-calculator

clean:
	rm -f gemini-cost-calculator gemini-chat-dev
