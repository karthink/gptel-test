;;; gptel-request-test.el --- Tests for gptel-request variable propagation  -*- lexical-binding: t; -*-

;; Tests for buffer-local/dynamic-binding interactions in the
;; `gptel-request' pipeline.  Uses :dry-run t to run the full request
;; construction (including `gptel--realize-query' and
;; `gptel--request-data') without making HTTP requests, then inspects
;; the FSM's info plist to verify correct propagation of system prompts,
;; schemas, and streaming settings.

(require 'ert)
(require 'gptel)
(require 'gptel-request)
(require 'gptel-test-backends)

;; Test-only variable for verifying buffer-local evaluation context.
;; Deliberately NOT in the `gptel--with-buffer-copy' copy list, so it
;; has no value in the prompt construction buffer.  This makes it a
;; reliable probe for which buffer a function was evaluated in.
(defvar gptel-test--buf-local nil)


;;;; System prompt propagation

(ert-deftest gptel-request-test-system-prompt-string ()
  "String system prompt via :system appears as messages[0].
Tests that a plain string passed as :system propagates through the
let-binding, buffer copy, and request-data construction to become
the system message in the final payload."
  (let* ((backend (alist-get 'openai gptel-test-backends))
         (fsm (let ((gptel-backend backend)
                    (gptel-model 'gpt-4o-mini)
                    (gptel-track-response nil))
                (gptel-request "hello" :system "custom system message" :dry-run t)))
         (messages (plist-get (plist-get (gptel-fsm-info fsm) :data) :messages)))
    (should (equal (plist-get (aref messages 0) :role) "system"))
    (should (equal (plist-get (aref messages 0) :content) "custom system message"))))

(ert-deftest gptel-request-test-system-prompt-function ()
  "Function-valued system prompt is evaluated and the result is used.
Tests that a function passed as :system is called, and its return
value is used as the system message content."
  (let* ((backend (alist-get 'openai gptel-test-backends))
         (fsm (let ((gptel-backend backend)
                    (gptel-model 'gpt-4o-mini)
                    (gptel-track-response nil))
                (gptel-request "hello"
                  :system (lambda () "dynamic system")
                  :dry-run t)))
         (messages (plist-get (plist-get (gptel-fsm-info fsm) :data) :messages)))
    (should (equal (plist-get (aref messages 0) :content) "dynamic system"))))

(ert-deftest gptel-request-test-system-prompt-function-buffer-context ()
  "Function-valued system prompt is evaluated in the request buffer.
`gptel-test--buf-local' is set buffer-locally in the request buffer
but is NOT in the `gptel--with-buffer-copy' copy list, so the prompt
construction buffer has no value for it.  If the function is evaluated
in the request buffer (correct), it sees the value.  If evaluated in
the prompt buffer (bug), it would see nil."
  (with-temp-buffer
    (setq-local gptel-test--buf-local "from-request-buffer")
    (let* ((backend (alist-get 'openai gptel-test-backends))
           (fsm (let ((gptel-backend backend)
                      (gptel-model 'gpt-4o-mini)
                      (gptel-track-response nil))
                  (gptel-request "hello"
                    :system (lambda () gptel-test--buf-local)
                    :dry-run t)))
           (messages (plist-get (plist-get (gptel-fsm-info fsm) :data) :messages)))
      (should (equal (plist-get (aref messages 0) :content)
                     "from-request-buffer")))))

(ert-deftest gptel-request-test-system-prompt-default ()
  "When :system is omitted, the dynamic value of gptel-system-prompt is used.
The :system argument defaults to the dynamic value of
`gptel-system-prompt' at call time.  Let-binding
`gptel-system-prompt' should propagate."
  (let* ((backend (alist-get 'openai gptel-test-backends))
         (fsm (let ((gptel-backend backend)
                    (gptel-model 'gpt-4o-mini)
                    (gptel-track-response nil)
                    (gptel-system-prompt "let-bound system"))
                (gptel-request "hello" :dry-run t)))
         (messages (plist-get (plist-get (gptel-fsm-info fsm) :data) :messages)))
    (should (equal (plist-get (aref messages 0) :content) "let-bound system"))))

(ert-deftest gptel-request-test-system-prompt-nil ()
  "Nil system prompt means no system message in the payload.
The `gptel-openai' backend guards with (when gptel-system-prompt ...),
so nil should result in no system message."
  (let* ((backend (alist-get 'openai gptel-test-backends))
         (fsm (let ((gptel-backend backend)
                    (gptel-model 'gpt-4o-mini)
                    (gptel-track-response nil))
                (gptel-request "hello" :system nil :dry-run t)))
         (messages (plist-get (plist-get (gptel-fsm-info fsm) :data) :messages)))
    ;; First message should be the user prompt, not a system message
    (should (equal (plist-get (aref messages 0) :role) "user"))))

(ert-deftest gptel-request-test-system-prompt-string-overrides-function ()
  "String :system overrides a buffer-local function-valued system prompt.
The request buffer has a function as buffer-local `gptel-system-prompt'.
Passing :system with a string should shadow it via let-binding, and the
function should NOT be evaluated."
  (with-temp-buffer
    (setq-local gptel-system-prompt (lambda () "should not be called"))
    (let* ((backend (alist-get 'openai gptel-test-backends))
           (fsm (let ((gptel-backend backend)
                      (gptel-model 'gpt-4o-mini)
                      (gptel-track-response nil))
                  (gptel-request "hello" :system "explicit string" :dry-run t)))
           (messages (plist-get (plist-get (gptel-fsm-info fsm) :data) :messages)))
      (should (equal (plist-get (aref messages 0) :content) "explicit string")))))

(ert-deftest gptel-request-test-system-prompt-function-overrides-string ()
  "Function :system overrides a buffer-local string system prompt.
The request buffer has a string as buffer-local `gptel-system-prompt'.
Passing :system with a function shadows it, and the function should be
evaluated to produce the system message."
  (with-temp-buffer
    (setq-local gptel-system-prompt "buffer local string")
    (let* ((backend (alist-get 'openai gptel-test-backends))
           (fsm (let ((gptel-backend backend)
                      (gptel-model 'gpt-4o-mini)
                      (gptel-track-response nil))
                  (gptel-request "hello"
                    :system (lambda () "from function")
                    :dry-run t)))
           (messages (plist-get (plist-get (gptel-fsm-info fsm) :data) :messages)))
      (should (equal (plist-get (aref messages 0) :content) "from function")))))


;;;; Schema propagation

(ert-deftest gptel-request-test-schema-propagation ()
  "Schema passed via :schema appears as :response_format in the payload.
Tests that the schema plist is copied into the prompt construction
buffer and consumed by `gptel--request-data' to produce a
JSON-schema :response_format."
  (let* ((backend (alist-get 'openai gptel-test-backends))
         (fsm (let ((gptel-backend backend)
                    (gptel-model 'gpt-4o-mini)
                    (gptel-track-response nil))
                (gptel-request "hello"
                  :schema '(:type object :properties (:name (:type string)))
                  :dry-run t)))
         (data (plist-get (gptel-fsm-info fsm) :data)))
    (should (plist-get data :response_format))
    (should (equal (plist-get (plist-get data :response_format) :type)
                   "json_schema"))))

(ert-deftest gptel-request-test-schema-nil ()
  "When :schema is omitted, no :response_format in the payload."
  (let* ((backend (alist-get 'openai gptel-test-backends))
         (fsm (let ((gptel-backend backend)
                    (gptel-model 'gpt-4o-mini)
                    (gptel-track-response nil))
                (gptel-request "hello" :dry-run t)))
         (data (plist-get (gptel-fsm-info fsm) :data)))
    (should-not (plist-get data :response_format))))


;;;; Stream propagation

(ert-deftest gptel-request-test-stream-enabled ()
  "Streaming is enabled when all conditions are met: :stream arg,
buffer-local `gptel-stream', `gptel-use-curl', and backend stream slot."
  (let* ((backend (alist-get 'openai-stream gptel-test-backends))
         (fsm (let ((gptel-backend backend)
                    (gptel-model 'gpt-4o-mini)
                    (gptel-track-response nil)
                    (gptel-use-curl t)
                    (gptel-stream t))
                (gptel-request "hello" :stream t :dry-run t)))
         (info (gptel-fsm-info fsm))
         (data (plist-get info :data)))
    ;; info :stream should be present
    (should (plist-get info :stream))
    ;; payload :stream should be t (not :json-false)
    (should (eq (plist-get data :stream) t))
    ;; OpenAI adds :stream_options when streaming
    (should (plist-get data :stream_options))))

(ert-deftest gptel-request-test-stream-default-disabled ()
  "When :stream is not passed (defaults to nil), streaming is off.
The :stream key is removed from info by `cl-remf', and the payload
has :stream set to :json-false (OpenAI convention)."
  (let* ((backend (alist-get 'openai-stream gptel-test-backends))
         (fsm (let ((gptel-backend backend)
                    (gptel-model 'gpt-4o-mini)
                    (gptel-track-response nil)
                    (gptel-use-curl t)
                    (gptel-stream t))
                (gptel-request "hello" :dry-run t)))
         (info (gptel-fsm-info fsm))
         (data (plist-get info :data)))
    ;; :stream should have been removed from info
    (should-not (plist-get info :stream))
    ;; payload :stream should be :json-false
    (should (eq (plist-get data :stream) :json-false))))

(ert-deftest gptel-request-test-stream-disabled-by-gptel-stream ()
  "Stream arg is truthy but gptel-stream is nil: streaming is off.
Tests that the buffer-local `gptel-stream' variable gates streaming
even when the :stream argument is t."
  (let* ((backend (alist-get 'openai-stream gptel-test-backends))
         (fsm (let ((gptel-backend backend)
                    (gptel-model 'gpt-4o-mini)
                    (gptel-track-response nil)
                    (gptel-use-curl t)
                    (gptel-stream nil))   ; disabled by user preference
                (gptel-request "hello" :stream t :dry-run t)))
         (info (gptel-fsm-info fsm))
         (data (plist-get info :data)))
    (should-not (plist-get info :stream))
    (should (eq (plist-get data :stream) :json-false))))

(ert-deftest gptel-request-test-stream-disabled-by-backend ()
  "Stream conditions met but backend doesn't support streaming: off.
Tests the (gptel-backend-stream gptel-backend) check in
`gptel--realize-query'."
  ;; Backend WITHOUT :stream t (slot defaults to nil)
  (let* ((backend (alist-get 'openai gptel-test-backends))
         (fsm (let ((gptel-backend backend)
                    (gptel-model 'gpt-4o-mini)
                    (gptel-track-response nil)
                    (gptel-use-curl t)
                    (gptel-stream t))
                (gptel-request "hello" :stream t :dry-run t)))
         (info (gptel-fsm-info fsm))
         (data (plist-get info :data)))
    (should-not (plist-get info :stream))
    (should (eq (plist-get data :stream) :json-false))))

(provide 'gptel-request-test)
;;; gptel-request-test.el ends here
