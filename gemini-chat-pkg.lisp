;;;; package.lisp

(defpackage #:gemini-chat
  (:use #:cl #:drakma #:cl-json #:uiop)
  (:shadowing-import-from :drakma :parameter-error))
