;; test/api-tests.lisp
(in-package #:gemini-chat-test)

(def-suite api-suite
  :description "Test suite for the Gemini API interactions."
  :in gemini-chat-suite)

(in-suite api-suite)

(defun run-tests ()
  "Runs all tests in the gemini-chat test suite."
  (let ((result (run! 'gemini-chat-suite)))
    (if (results-status result)
        t
        nil)))

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
        (mock-url (com.google.flag:flag-value '*api-url*))
        (mock-model "mock-model"))
    
    (set-mock-response 200 mock-response-json)

    (let ((real-http-request #'drakma:http-request))
      (flet ((run-mocked-test ()
               ;; This is where we replace the real function with our mock
               (setf (fdefinition 'drakma:http-request) #'mock-http-request)
               (let ((result-stream (gemini-chat:api-req '("some-prompt") :model mock-model)))
                 (is (string= (slurp-stream-string result-stream) mock-response-json))
                 ;; Check the captured payload
                 (is (jsown:to-json *captured-request*))))
             (unwind-protect (run-mocked-test)
               ;; Always restore the real function
               (setf (fdefinition 'drakma:http-request) real-http-request))))))
