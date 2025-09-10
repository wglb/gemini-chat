;; api-tests.lisp
(in-package #:gemini-chat-test)

(def-suite api-suite
  :description "Test suite for the Gemini API interactions."
  :in gemini-chat-suite)

(in-suite api-suite)

;; Mock server variables
(defparameter *mock-api-response* nil
  "Holds the mock HTTP response string.")

(defparameter *captured-request* nil
  "Holds the details of the last API request made to the mock.")

(defun set-mock-response (status body &key (headers '(("Content-Type" . "application/json"))))
  "Sets the mock response for the next API call."
  (setf *mock-api-response* (list :status status :body body :headers headers)))

(defun mock-http-request (url &key (method :get) content-type content additional-headers want-stream force-ssl)
  "Mocks the drakma:http-request call to return a pre-set response.
   Captures the request details for later inspection."
  (declare (ignorable url method content-type additional-headers want-stream force-ssl))
  (setf *captured-request* (jsown:parse content))
  (destructuring-bind (&key status body headers) *mock-api-response*
    (declare (ignorable status headers))
    (make-string-input-stream body)))

(test api-req-success
  "Tests that api-req correctly handles a successful API response."
  (let ((mock-response-json "{\"candidates\": [{\"content\": {\"parts\": [{\"text\": \"This is a mock response.\"}]}}]}")
        (mock-url "https://generativelanguage.googleapis.com/v1beta/models/")
        (mock-model "mock-model")
        (*api-url* "http://mock-api/")) ; Define a local mock API URL
    
    (set-mock-response 200 mock-response-json)

    (let ((real-http-request #'drakma:http-request))
      (unwind-protect
           (progn
             (setf (fdefinition 'drakma:http-request) #'mock-http-request)
             (let ((result-stream (gemini-chat:api-req `(,(jsown:new-js
                                                             ("role" . "user")
                                                             ("parts" . `(,(jsown:new-js ("text" . "some-prompt"))))))
                                                        :model mock-model)))
               (is (string= (uiop:slurp-stream-string result-stream) mock-response-json))
               (is (jsown:to-json *captured-request*))))
        (setf (fdefinition 'drakma:http-request) real-http-request)))))

(test api-req-failure
  "Tests that api-req correctly handles a failed API response."
  (let ((mock-response-json "{\"error\":{\"code\":400,\"message\":\"Bad Request\"}}")
        (mock-url "https://generativelanguage.googleapis.com/v1beta/models/")
        (mock-model "mock-model")
        (*api-url* "http://mock-api/"))
    (set-mock-response 400 mock-response-json)
    (let ((real-http-request #'drakma:http-request))
      (unwind-protect
           (progn
             (setf (fdefinition 'drakma:http-request) #'mock-http-request)
             (signals error (gemini-chat:api-req `(,(jsown:new-js
                                                     ("role" . "user")
                                                     ("parts" . `(,(jsown:new-js ("text" . "some-prompt"))))))
                                                :model mock-model)))
        (setf (fdefinition 'drakma:http-request) real-http-request)))))
