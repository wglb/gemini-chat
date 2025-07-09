#!/bin/sh
#build the sucker.
sbcl  --dynamic-space-size 2000\
      --disable-debugger \
      --eval "(asdf:operate 'asdf:load-op 'gemini-chat)" \
      --eval '(in-package #:gemini-chat)' \
      --eval '(save-core)'
