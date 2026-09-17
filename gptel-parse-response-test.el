;; -*- lexical-binding: t; -*-
;; Commentary
;;
;; Tests for parsing LLM responses (streaming and batched) with reasoning
;; integrated.  These tests do not cover responses with tool calls.

(require 'ert)
(require 'gptel)
(require 'gptel-request)
(require 'gptel-test-backends)

;;; Reasoning non-stream tests
(defun gptel-test--insert-response (output-file)
  "Fill the current buffer with a dummy curl response.

A stub HTTP header block is inserted, followed by the contents of
OUTPUT-FILE and the write-out data `gptel-curl--parse-response'
expects at the end of the buffer."
  (erase-buffer)
  (insert "HTTP/2 200
date: Sun, 15 Mar 2026 16:15:46 GMT
content-type: application/json
server: cloudflare
x-ratelimit-limit-requests: 5000
x-ratelimit-limit-tokens: 2000000
x-ratelimit-remaining-requests: 4999
x-ratelimit-remaining-tokens: 1999841
x-ratelimit-reset-requests: 12ms
x-ratelimit-reset-tokens: 4ms
x-request-id: req_fab8a9ebcf21467788a686760360906d
openai-processing-ms: 9098
cf-cache-status: DYNAMIC
strict-transport-security: max-age=31536000; includeSubDomains; preload
x-content-type-options: nosniff
content-encoding: br
cf-ray: 9dccd15e4d31232c-SJC
alt-svc: h3=\":443\"; ma=86400

")
  (insert-file-contents output-file)
  (goto-char (point-max))
  (skip-chars-backward "\n\r")
  (insert "(uuid . 589)")       ;dummy for gptel-curl--parse-response
  (goto-char (point-min)))

(defun gptel-test-sentinel (backend-name output-file)
  "Parse a stream output for BACKEND-NAME.

The keys of `gptel-test-backends' are possible values of BACKEND-NAME.

OUTPUT-FILE is the output (txt) file containing the response JSON."
  (declare (indent 1))
  (let* ((fsm (gptel-make-fsm
               :table '((INIT . ((t . DONE))))
               :handlers '((DONE))))
         (backend (alist-get backend-name gptel-test-backends))
         info reasoning response
         (callback (lambda (resp info)
                     (pcase resp
                       (`(reasoning . ,text) (setq reasoning text))
                       ((pred stringp)       (setq response resp)))))
         (proc-buf (generate-new-buffer "*gptel-test-sentinel*"))
         (proc (make-process :name "gptel-test" :command nil
                             :buffer proc-buf))
         (insert-output
          (lambda ()
            (with-current-buffer proc-buf
              (gptel-test--insert-response output-file)))))
    (setq info (list :uuid "uuid"       ;dummy for gptel-curl--parse-response
                     :backend backend
                     :data (list :contents [(:role "user" :parts [(:text "Test")])])
                     :include-reasoning t
                     :http-status "200"
                     :status "HTTP/2 200"
                     :callback callback))
    (setf (gptel-fsm-info fsm) info
          (alist-get proc gptel--request-alist) (list fsm))
    (unwind-protect
        (progn
          (funcall insert-output)
          (cl-letf* (((symbol-function 'process-status) (lambda (_) 'exit)))
            ;; (set-process-sentinel proc 'gptel-curl--sentinel)
            ;; (accept-process-output proc)
            (gptel-curl--sentinel proc nil)))
      (setf (alist-get proc gptel--request-alist nil t) nil)
      (delete-process proc)
      (when (buffer-live-p proc-buf) (kill-buffer proc-buf)))
    (list reasoning response)))

(defun gptel-test-sentinel-error (backend-name output-file)
  "Parse an error response for BACKEND-NAME.

The keys of `gptel-test-backends' are possible values of BACKEND-NAME.

OUTPUT-FILE is the output (txt) file containing the response body.

Unlike `gptel-test-sentinel', this returns a list of two items:
 (<t if the callback was called with a nil response>
  <the value of :error in the info plist>)"
  (declare (indent 1))
  (let* ((fsm (gptel-make-fsm
               :table '((INIT . ((t . DONE))))
               :handlers '((DONE))))
         (backend (alist-get backend-name gptel-test-backends))
         info called-with-nil
         (callback (lambda (resp _info)
                     (when (null resp) (setq called-with-nil t))))
         (proc-buf (generate-new-buffer "*gptel-test-sentinel-error*"))
         (proc (make-process :name "gptel-test" :command nil
                             :buffer proc-buf)))
    (setq info (list :uuid "uuid"       ;dummy for gptel-curl--parse-response
                     :backend backend
                     :data (list :contents [(:role "user" :parts [(:text "Test")])])
                     :include-reasoning t
                     :http-status "200"
                     :status "HTTP/2 200"
                     :callback callback))
    (setf (gptel-fsm-info fsm) info
          (alist-get proc gptel--request-alist) (list fsm))
    (unwind-protect
        (progn
          (with-current-buffer proc-buf
            (gptel-test--insert-response output-file))
          (cl-letf* (((symbol-function 'process-status) (lambda (_) 'exit)))
            (gptel-curl--sentinel proc nil)))
      (setf (alist-get proc gptel--request-alist nil t) nil)
      (delete-process proc)
      (when (buffer-live-p proc-buf) (kill-buffer proc-buf)))
    (list called-with-nil (plist-get info :error))))

(defmacro gptel-test-response-parsing
    (name backend-name output-file result-file)
  "Create test for response parsing that covers reasoning and response.

Tool use parsing is not covered in this test.

NAME is the name of the test, any string.
BACKEND-NAME is a key from `gptel-test-backends'.
STREAM-OUTPUT-FILE is the output (txt) file containing the streaming.
INCREMENT is the size of chunks to feed the process filter, defaults to 800.

RESULT-FILE is the corresponding (eld) file containing the parsed
output.  It should contain a list of three items:
 (<t or nil>           ; depending on whether a reasoning stream was found
  \"reasoning text\"
  \"response text\")"
  (declare (indent 2))
  `(ert-deftest ,(intern (concat "gptel-test-response-parsing-" name)) ()
     (should
      (equal
       (with-temp-buffer
         (save-excursion (insert-file-contents ,result-file))
         (read (current-buffer)))
       (gptel-test-sentinel ,backend-name ,output-file)))))

;;; Reasoning stream tests
(defun gptel-test-stream-filter (backend-name stream-output-file &optional increment)
  "Parse a stream output for BACKEND-NAME.

The keys of `gptel-test-backends' are possible values of BACKEND-NAME.

STREAM-OUTPUT-FILE is the output (txt) file containing the streaming
response contents.

INCREMENT is the size of chunks to feed the process filter."
  (declare (indent 1))
  (let* ((fsm (gptel-make-fsm))
         (backend (alist-get backend-name gptel-test-backends))
         (increment (or increment 800))
         info reasoning-list response-list
         (proc (make-process :name "gptel-test" :command nil))
         (callback (lambda (resp info)
                     (pcase resp
                       (`(reasoning . ,text) (push text reasoning-list))
                       ((pred stringp)       (push resp response-list)))))
         (outputs
          (with-temp-buffer
            (insert-file-contents stream-output-file)
            (seq-partition (buffer-string) increment))))
    (setq info (list :backend backend
                     :data (list :contents [(:role "user" :parts [(:text "Test")])])
                     :include-reasoning t
                     :http-status "200"
                     :status "HTTP/2 200"
                     :callback callback))
    (setf (gptel-fsm-info fsm) info
          (alist-get proc gptel--request-alist) (list fsm))
    (unwind-protect
        (with-temp-buffer
          (setf (process-buffer proc) (current-buffer))
          (while outputs (gptel-curl--stream-filter proc (pop outputs))))
      (setf (alist-get proc gptel--request-alist nil t) nil)
      (delete-process proc))
    (list (car-safe reasoning-list)     ;Did the reasoning stream end with `t'?
          (string-join (nreverse (cdr-safe reasoning-list))) ;Correct reasoning content?
          (string-join (nreverse response-list)))))

(defun gptel-test-stream-filter-error (backend-name stream-output-file &optional increment)
  "Parse an error stream for BACKEND-NAME.

The keys of `gptel-test-backends' are possible values of BACKEND-NAME.

STREAM-OUTPUT-FILE is the output (txt) file containing the streaming
response contents.

INCREMENT is the size of chunks to feed the process filter.

Unlike `gptel-test-stream-filter', this returns a list of two items:
 (<the response text received before the stream ended>
  <the value of :error in the info plist>)"
  (declare (indent 1))
  (let* ((fsm (gptel-make-fsm))
         (backend (alist-get backend-name gptel-test-backends))
         (increment (or increment 800))
         info response-list
         (callback (lambda (resp _info)
                     (when (stringp resp) (push resp response-list))))
         (proc (make-process :name "gptel-test" :command nil))
         (outputs
          (with-temp-buffer
            (insert-file-contents stream-output-file)
            (seq-partition (buffer-string) increment))))
    (setq info (list :backend backend
                     :data (list :contents [(:role "user" :parts [(:text "Test")])])
                     :include-reasoning t
                     :http-status "200"
                     :status "HTTP/2 200"
                     :callback callback))
    (setf (gptel-fsm-info fsm) info
          (alist-get proc gptel--request-alist) (list fsm))
    (unwind-protect
        (with-temp-buffer
          (setf (process-buffer proc) (current-buffer))
          (while outputs (gptel-curl--stream-filter proc (pop outputs))))
      (setf (alist-get proc gptel--request-alist nil t) nil)
      (delete-process proc))
    (list (string-join (nreverse response-list))
          (plist-get info :error))))

(defmacro gptel-test-stream-parsing
    (name backend-name stream-output-file result-file &optional increment)
  "Create test for stream parsing that covers reasoning and response.

Tool use parsing is not covered in this test.

NAME is the name of the test, any string.
BACKEND-NAME is a key from `gptel-test-backends'.
STREAM-OUTPUT-FILE is the output (txt) file containing the streaming.
INCREMENT is the size of chunks to feed the process filter, defaults to 800.

RESULT-FILE is the corresponding (eld) file containing the parsed
output.  It should contain a list of three items:
 (<t or nil>           ; depending on whether a reasoning stream was found
  \"reasoning text\"
  \"response text\")"
  (declare (indent 2))
  `(ert-deftest ,(intern (concat "gptel-test-stream-parsing-" name)) ()
     (should
      (equal
       (with-temp-buffer
         (save-excursion (insert-file-contents ,result-file))
         (read (current-buffer)))
       (gptel-test-stream-filter
           ,backend-name ,stream-output-file ,increment)))))

;;;; OpenAI-compatible

;;;;; llama.cpp
;;;;;; non-streaming
(gptel-test-response-parsing "llamacpp-nostream-01" 'openai
  "examples-responses/openai/thinking_nostream_llamacpp_glm4.5_01.txt"
  "examples-responses/openai/thinking_nostream_llamacpp_glm4.5_01.eld")

;;;;;; streaming
(gptel-test-stream-parsing "llamacpp-01a" 'openai
  "examples-responses/openai/thinking_stream_llamacpp_glm4.5_01.txt"
  "examples-responses/openai/thinking_stream_llamacpp_glm4.5_01.eld"
  800)

(gptel-test-stream-parsing "llamacpp-01b" 'openai
  "examples-responses/openai/thinking_stream_llamacpp_glm4.5_01.txt"
  "examples-responses/openai/thinking_stream_llamacpp_glm4.5_01.eld"
  400)

(gptel-test-stream-parsing "llamacpp-01c" 'openai
  "examples-responses/openai/thinking_stream_llamacpp_glm4.5_01.txt"
  "examples-responses/openai/thinking_stream_llamacpp_glm4.5_01.eld"
  40)

;;;;; vLLM
(gptel-test-stream-parsing "vllm-01a" 'openai
  "examples-responses/openai/thinking_stream_vllm_glm4.5_01.txt"
  "examples-responses/openai/thinking_stream_vllm_glm4.5_01.eld"
  800)

(gptel-test-stream-parsing "vllm-01b" 'openai
  "examples-responses/openai/thinking_stream_vllm_glm4.5_01.txt"
  "examples-responses/openai/thinking_stream_vllm_glm4.5_01.eld"
  400)

(gptel-test-stream-parsing "vllm-01c" 'openai
  "examples-responses/openai/thinking_stream_vllm_glm4.5_01.txt"
  "examples-responses/openai/thinking_stream_vllm_glm4.5_01.eld"
  40)


;;;;; Openrouter
(gptel-test-stream-parsing "openrouter-01a" 'openai
  "examples-responses/openai/thinking_stream_openrouter_01.txt"
  "examples-responses/openai/thinking_stream_openrouter_01.eld"
  800)

(gptel-test-stream-parsing "openrouter-01b" 'openai
  "examples-responses/openai/thinking_stream_openrouter_01.txt"
  "examples-responses/openai/thinking_stream_openrouter_01.eld"
  400)

(gptel-test-stream-parsing "openrouter-02a" 'openai
  "examples-responses/openai/thinking_stream_openrouter_02.txt"
  "examples-responses/openai/thinking_stream_openrouter_02.eld"
  800)

(gptel-test-stream-parsing "openrouter-02b" 'openai
  "examples-responses/openai/thinking_stream_openrouter_02.txt"
  "examples-responses/openai/thinking_stream_openrouter_02.eld"
  400)

;;;;; Deepseek
;;;;;; non-streaming
(gptel-test-response-parsing "deepseek-nostream-01" 'deepseek
  "examples-responses/openai/thinking_nostream_deepseek_01.txt"
  "examples-responses/openai/thinking_nostream_deepseek_01.eld")

;;;;;; streaming
(gptel-test-stream-parsing "deepseek-01a" 'deepseek
  "examples-responses/openai/thinking_stream_deepseek_01.txt"
  "examples-responses/openai/thinking_stream_deepseek_01.eld"
  800)

;;;; OpenAI-responses API
;;;;; non-streaming
(gptel-test-response-parsing "openai-responses-nostream-01" 'openai-responses
  "examples-responses/openai-responses/thinking_nostream_gpt5.2_01.txt"
  "examples-responses/openai-responses/thinking_nostream_gpt5.2_01.eld")

(gptel-test-response-parsing "openai-responses-nostream-02" 'openai-responses
  "examples-responses/openai-responses/nostream_gpt5.2_01.txt"
  "examples-responses/openai-responses/nostream_gpt5.2_01.eld")

(gptel-test-response-parsing "openai-responses-nostream-03" 'openai-responses
  "examples-responses/openai-responses/thinking_nostream_oss120_01.txt"
  "examples-responses/openai-responses/thinking_nostream_oss120_01.eld")

(gptel-test-response-parsing "openai-responses-nostream-04" 'openai-responses
  "examples-responses/openai-responses/thinking_nostream_gemini3_01.txt"
  "examples-responses/openai-responses/thinking_nostream_gemini3_01.eld")

(ert-deftest gptel-test-response-error-responses-nostream-failed-01 ()
  "A failed response delivered under HTTP 200 is reported to the caller.
The body is plain JSON with a nested error and no output, so the
callback must run with a nil response and :error must carry the
server's message."
  (pcase-let ((`(,called-with-nil ,error)
               (gptel-test-sentinel-error 'openai-responses
                 "examples-responses/openai-responses/nostream_failed_01.txt")))
    (should called-with-nil)
    (should error)
    (should (string-match-p "The server had an error processing your request"
                            (format "%S" error)))))

;;;;; streaming
(gptel-test-stream-parsing "openai-responses-01a" 'openai-responses
  "examples-responses/openai-responses/thinking_stream_gpt5.2_01.txt"
  "examples-responses/openai-responses/thinking_stream_gpt5.2_01.eld"
  800)

(gptel-test-stream-parsing "openai-responses-01b" 'openai-responses
  "examples-responses/openai-responses/thinking_stream_gpt5.2_01.txt"
  "examples-responses/openai-responses/thinking_stream_gpt5.2_01.eld"
  400)

(gptel-test-stream-parsing "openai-responses-02a" 'openai-responses
  "examples-responses/openai-responses/stream_gpt5.2_02.txt"
  "examples-responses/openai-responses/stream_gpt5.2_02.eld"
  800)

(gptel-test-stream-parsing "openai-responses-02b" 'openai-responses
  "examples-responses/openai-responses/stream_gpt5.2_02.txt"
  "examples-responses/openai-responses/stream_gpt5.2_02.eld"
  400)

(gptel-test-stream-parsing "openai-responses-03a" 'openai-responses
  "examples-responses/openai-responses/thinking_stream_oss120_01.txt"
  "examples-responses/openai-responses/thinking_stream_oss120_01.eld"
  800)

(gptel-test-stream-parsing "openai-responses-03b" 'openai-responses
  "examples-responses/openai-responses/thinking_stream_oss120_01.txt"
  "examples-responses/openai-responses/thinking_stream_oss120_01.eld"
  400)

(gptel-test-stream-parsing "openai-responses-04a" 'openai-responses
  "examples-responses/openai-responses/thinking_stream_gemini3_01.txt"
  "examples-responses/openai-responses/thinking_stream_gemini3_01.eld"
  800)

(gptel-test-stream-parsing "openai-responses-04b" 'openai-responses
  "examples-responses/openai-responses/thinking_stream_gemini3_01.txt"
  "examples-responses/openai-responses/thinking_stream_gemini3_01.eld"
  400)

;;;;; OpenAI OAuth (Codex)
;;;;;; non-streaming
;; The Codex endpoint always replies with a server-sent-event body, even when
;; gptel is not streaming.
(gptel-test-response-parsing "codex-nostream-01" 'openai-oauth
  "examples-responses/openai-oauth/nostream_gpt5.6_01.txt"
  "examples-responses/openai-oauth/nostream_gpt5.6_01.eld")

;; The reasoning item here has a null `content', which must not signal.
(gptel-test-response-parsing "codex-nostream-02" 'openai-oauth
  "examples-responses/openai-oauth/nostream_gpt5.6_02.txt"
  "examples-responses/openai-oauth/nostream_gpt5.6_02.eld")

(ert-deftest gptel-test-response-error-codex-nostream-01 ()
  "An error event in an HTTP 200 SSE body is reported to the caller.
The body carries a flat `error' event and no terminal event, so
the callback must run with a nil response and :error must carry
the server's message."
  (pcase-let ((`(,called-with-nil ,error)
               (gptel-test-sentinel-error 'openai-oauth
                 "examples-responses/openai-oauth/nostream_error_01.txt")))
    (should called-with-nil)
    (should error)
    (should (string-match-p "Rate limit reached for gpt-5\\.6-luna"
                            (format "%S" error)))))

(ert-deftest gptel-test-response-error-codex-nostream-02 ()
  "A `response.failed' terminal event is reported to the caller.
Its response object nests the error and has a null `output', which
must not be mistaken for a malformed body."
  (pcase-let ((`(,called-with-nil ,error)
               (gptel-test-sentinel-error 'openai-oauth
                 "examples-responses/openai-oauth/nostream_error_02.txt")))
    (should called-with-nil)
    (should error)
    (should (string-match-p "The model produced invalid output"
                            (format "%S" error)))
    (should-not (string-match-p "Malformed JSON" (format "%S" error)))))

(ert-deftest gptel-test-response-error-codex-nostream-truncated-01 ()
  "A truncated SSE body is reported as an incomplete response.
The body carries neither a terminal event nor an error event, so
the callback must run with a nil response and :error must say the
response was cut short rather than blame the JSON."
  (pcase-let ((`(,called-with-nil ,error)
               (gptel-test-sentinel-error 'openai-oauth
                 "examples-responses/openai-oauth/nostream_truncated_01.txt")))
    (should called-with-nil)
    (should error)
    (should-not (string-match-p "Malformed JSON" (format "%S" error)))
    (should (let ((case-fold-search t))
              (string-match-p "incomplete\\|truncated" (format "%S" error))))))

(ert-deftest gptel-test-response-error-codex-nostream-html-01 ()
  "A body that is neither JSON nor SSE is reported to the caller.
A proxy can answer with an HTML page under HTTP 200, which must
not be mistaken for an empty response."
  (pcase-let ((`(,called-with-nil ,error)
               (gptel-test-sentinel-error 'openai-oauth
                 "examples-responses/openai-oauth/nostream_html_01.txt")))
    (should called-with-nil)
    (should error)
    (should (let ((case-fold-search t))
              (string-match-p "Malformed JSON\\|Could not parse"
                              (format "%S" error))))))

;;;;;; streaming
(ert-deftest gptel-test-stream-error-codex-01 ()
  "A flat error event in a stream is reported to the caller.
The event carries the code and message directly instead of
nesting them under `error', and arrives after some text has
already been streamed."
  (pcase-let ((`(,response ,error)
               (gptel-test-stream-filter-error 'openai-oauth
                 "examples-responses/openai-oauth/stream_error_01.txt"
                 800)))
    (should (equal response "Hello"))
    (should error)
    (should (string-match-p "Rate limit reached for gpt-5\\.6-luna"
                            (format "%S" error)))))

;; ;; TODO: This test file is too large, generate a smaller one
;; (gptel-test-stream-parsing "deepseek-01b" 'deepseek
;;   "examples-responses/openai/thinking_stream_deepseek_02.txt"
;;   "examples-responses/openai/thinking_stream_deepseek_02.eld"
;;   400)

;; ;; MAYBE The test file for this next text is very large, ignoring for now

;; (gptel-test-stream-parsing "deepseek-02a" 'deepseek
;;   "examples-responses/openai/thinking_stream_deepseek_01.txt"
;;   "examples-responses/openai/thinking_stream_deepseek_01.eld"
;;   800)

;; (gptel-test-stream-parsing "deepseek-02b" 'deepseek
;;   "examples-responses/openai/thinking_stream_deepseek_02.txt"
;;   "examples-responses/openai/thinking_stream_deepseek_02.eld"
;;   400)

;;;; Anthropic
;;;;; non-streaming
(gptel-test-response-parsing "anthropic-nostream-01" 'anthropic
  "examples-responses/anthropic/thinking_nostream_01.txt"
  "examples-responses/anthropic/thinking_nostream_01.eld")

(gptel-test-response-parsing "anthropic-nostream-02" 'anthropic
  "examples-responses/anthropic/thinking_nostream_02.txt"
  "examples-responses/anthropic/thinking_nostream_02.eld")

;;;;; streaming
(gptel-test-stream-parsing "anthropic-01a" 'anthropic
  "examples-responses/anthropic/thinking_stream_01.txt"
  "examples-responses/anthropic/thinking_stream_01.eld"
  800)

(gptel-test-stream-parsing "anthropic-01b" 'anthropic
  "examples-responses/anthropic/thinking_stream_01.txt"
  "examples-responses/anthropic/thinking_stream_01.eld"
  400)

(gptel-test-stream-parsing "anthropic-02a" 'anthropic
  "examples-responses/anthropic/thinking_stream_02.txt"
  "examples-responses/anthropic/thinking_stream_02.eld"
  800)

(gptel-test-stream-parsing "anthropic-02b" 'anthropic
  "examples-responses/anthropic/thinking_stream_02.txt"
  "examples-responses/anthropic/thinking_stream_02.eld"
  400)

(gptel-test-stream-parsing "anthropic-03a" 'anthropic
  "examples-responses/anthropic/thinking_stream_03.txt"
  "examples-responses/anthropic/thinking_stream_03.eld"
  800)

(gptel-test-stream-parsing "anthropic-03b" 'anthropic
  "examples-responses/anthropic/thinking_stream_03.txt"
  "examples-responses/anthropic/thinking_stream_03.eld"
  400)

(gptel-test-stream-parsing "anthropic-04a" 'anthropic
  "examples-responses/anthropic/thinking_stream_04.txt"
  "examples-responses/anthropic/thinking_stream_04.eld"
  800)

(gptel-test-stream-parsing "anthropic-04b" 'anthropic
  "examples-responses/anthropic/thinking_stream_04.txt"
  "examples-responses/anthropic/thinking_stream_04.eld"
  400)

;;;; Gemini

(gptel-test-stream-parsing "gemini-01a" 'gemini
  "examples-responses/gemini/thinking_stream_01.txt"
  "examples-responses/gemini/thinking_stream_01.eld"
  800)

(gptel-test-stream-parsing "gemini-01b" 'gemini
  "examples-responses/gemini/thinking_stream_01.txt"
  "examples-responses/gemini/thinking_stream_01.eld"
  400)

(gptel-test-stream-parsing "gemini-02a" 'gemini
  "examples-responses/gemini/thinking_stream_02.txt"
  "examples-responses/gemini/thinking_stream_02.eld"
  800)

(gptel-test-stream-parsing "gemini-02b" 'gemini
  "examples-responses/gemini/thinking_stream_02.txt"
  "examples-responses/gemini/thinking_stream_02.eld"
  400)

(gptel-test-stream-parsing "gemini-03a" 'gemini
  "examples-responses/gemini/thinking_stream_03.txt"
  "examples-responses/gemini/thinking_stream_03.eld"
  800)

(gptel-test-stream-parsing "gemini-03b" 'gemini
  "examples-responses/gemini/thinking_stream_03.txt"
  "examples-responses/gemini/thinking_stream_03.eld"
  400)
