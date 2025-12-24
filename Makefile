# Makefile optimized for Bill's F2 workflow
BIN_DIR = ~/bin
CALC_EXE = gemini-cost-calculator
CHAT_EXE = gemini-chat-dev

all: $(CALC_EXE) $(CHAT_EXE)

$(CALC_EXE): gemini-cost-calculator.asd gemini-cost-calculator.lisp gemini-cost-calculator-pkg.lisp
	sbcl --dynamic-space-size 2000 --disable-debugger \
		--eval "(asdf:operate 'asdf:load-op 'gemini-cost-calculator)" \
		--eval '(in-package #:gemini-cost-calculator)' \
		--eval '(save-core)'

$(CHAT_EXE): gemini-chat.lisp gemini-chat.asd gemini-chat-pkg.lisp gemini-chat-lib-pkg.lisp gemini-chat-lib.asd gemini-chat-lib.lisp
	sbcl --dynamic-space-size 2000 --disable-debugger \
		--eval "(asdf:operate 'asdf:load-op 'gemini-chat)" \
		--eval '(in-package #:gemini-chat)' \
		--eval '(save-core-uncompressed)'
	touch $(CHAT_EXE)

install: $(BIN_DIR)/$(CALC_EXE)

$(BIN_DIR)/$(CALC_EXE): $(CALC_EXE)
	cp -v $(CALC_EXE) $(BIN_DIR)/$(CALC_EXE)

clean:
	rm -f $(CALC_EXE) $(CHAT_EXE)
