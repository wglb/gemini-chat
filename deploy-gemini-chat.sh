#!/bin/sh
#build the sucker.
sbcl  --dynamic-space-size 2000\
      --disable-debugger \
	  --eval '(ql:quickload :gemini-chat)' \
      --eval '(progn  (asdf:operate '"'"'deploy:deploy-op'"'"' :gemini-chat))' \
      --quit


