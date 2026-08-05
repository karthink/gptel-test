;;; gptel-org-test.el --- Tests for gptel-org interactions with gptel-request  -*- lexical-binding: t; -*-

;; Tests for Org-mode interactions with the gptel-request pipeline.
;; Two groups:
;;
;; 1. `gptel-request' called directly from an Org buffer (exercises
;;    `gptel-org--create-prompt-buffer' via `gptel--create-prompt-buffer').
;;
;; 2. `gptel-send' called from an Org buffer with GPTEL_* properties
;;    (exercises `gptel-org--send-with-props' advice and preset
;;    application in the prompt construction buffer).

(require 'ert)
(require 'gptel)
(require 'gptel-request)
(require 'gptel-org)
(require 'gptel-test-backends)

;; Test-only variable for verifying buffer-local evaluation context.
;; NOT in the `gptel--with-buffer-copy' copy list, so the prompt
;; construction buffer has no value for it.
(defvar gptel-test--buf-local nil)

;; Captured by the mocked HTTP function during `gptel-send' tests.
(defvar gptel-test--captured-fsm nil)

(defmacro with-gptel-send-mock (&rest body)
  "Run BODY with `gptel-send' set up for dry-run testing.
Intercepts the `gptel-request' call inside `gptel-send' to add
:dry-run t and capture the FSM.  The captured FSM is stored in
`gptel-test--captured-fsm' for inspection."
  (declare (indent 0))
  (let ((orig (make-symbol "orig-request")))
    `(let ((,orig (symbol-function 'gptel-request)))
       ;; Keep the capture after the macro exits so the enclosing test
       ;; can inspect the resulting FSM.
       (setq gptel-test--captured-fsm nil)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (&rest args)
                    ;; ARGS begins with gptel-request's positional PROMPT,
                    ;; so append the dry-run keyword rather than plist-put.
                    (setq gptel-test--captured-fsm
                          (apply ,orig (append args '(:dry-run t)))))))
         ,@body))))


;;;; gptel-request from Org buffers

(ert-deftest gptel-org-test-request-system-prompt-string ()
  "String system prompt propagates through the Org prompt buffer path.
Exercises `gptel-org--create-prompt-buffer' (Org variant) and verifies
that a let-bound `gptel-system-prompt' makes it into the final payload."
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert "* Test\nHello world")
    (goto-char (point-max))
    (let* ((backend (alist-get 'openai gptel-test-backends))
           (fsm (let ((gptel-backend backend)
                      (gptel-model 'gpt-4o-mini)
                      (gptel-track-response nil)
                      (gptel-system-prompt "org system"))
                  (gptel-request nil :dry-run t)))
           (messages (plist-get (plist-get (gptel-fsm-info fsm) :data) :messages)))
      (should (equal (plist-get (aref messages 0) :content) "org system")))))

(ert-deftest gptel-org-test-request-system-prompt-function-context ()
  "Function system prompt is evaluated in the Org request buffer, not the
prompt construction buffer.  Same as `gptel-request-test-system-prompt-function-buffer-context'
but via the Org prompt buffer creation path."
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert "* Test\nHello world")
    (goto-char (point-max))
    (setq-local gptel-test--buf-local "from-org-buffer")
    (let* ((backend (alist-get 'openai gptel-test-backends))
           (fsm (let ((gptel-backend backend)
                      (gptel-model 'gpt-4o-mini)
                      (gptel-track-response nil))
                  (gptel-request nil
                    :system (lambda () gptel-test--buf-local)
                    :dry-run t)))
           (messages (plist-get (plist-get (gptel-fsm-info fsm) :data) :messages)))
      (should (equal (plist-get (aref messages 0) :content) "from-org-buffer")))))

(ert-deftest gptel-org-test-request-system-explicit-overrides-buffer-local ()
  "Explicit :system arg overrides buffer-local system prompt in Org buffer.
The :system keyword let-binds `gptel-system-prompt', shadowing the
buffer-local value.  The buffer copy and function check must respect
this shadowing."
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert "* Test\nHello world")
    (goto-char (point-max))
    (setq-local gptel-system-prompt "buffer-local")
    (let* ((backend (alist-get 'openai gptel-test-backends))
           (fsm (let ((gptel-backend backend)
                      (gptel-model 'gpt-4o-mini)
                      (gptel-track-response nil))
                  (gptel-request nil :system "explicit" :dry-run t)))
           (messages (plist-get (plist-get (gptel-fsm-info fsm) :data) :messages)))
      (should (equal (plist-get (aref messages 0) :content) "explicit")))))

(ert-deftest gptel-org-test-request-gptel-system-not-applied ()
  "GPTEL_SYSTEM property does not affect direct `gptel-request' calls.
Org properties should only be applied through `gptel-send' (via
`gptel-org--send-with-props' advice), not through direct `gptel-request'
calls.  Without `gptel-mode' enabled, the property should be ignored."
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert "* Test\n")
    (org-set-property "GPTEL_SYSTEM" "from org property")
    (insert "\nHello world")
    (goto-char (point-max))
    (let* ((backend (alist-get 'openai gptel-test-backends))
           ;; Use a distinctive default system prompt so we can tell it apart
           (fsm (let ((gptel-backend backend)
                      (gptel-model 'gpt-4o-mini)
                      (gptel-track-response nil)
                      (gptel-system-prompt "default system"))
                  (gptel-request nil :dry-run t)))
           (messages (plist-get (plist-get (gptel-fsm-info fsm) :data) :messages)))
      ;; Should be the default, NOT "from org property"
      (should (equal (plist-get (aref messages 0) :content) "default system")))))

(ert-deftest gptel-org-test-request-schema-and-stream ()
  "Schema and stream propagate correctly through the Org prompt buffer path.
Ensures `gptel-org--create-prompt-buffer' doesn't break schema or
stream propagation."
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert "* Test\nHello world")
    (goto-char (point-max))
    (let* ((backend (alist-get 'openai-stream gptel-test-backends))
           (fsm (let ((gptel-backend backend)
                      (gptel-model 'gpt-4o-mini)
                      (gptel-track-response nil)
                      (gptel-use-curl t)
                      (gptel-stream t))
                  (gptel-request nil
                    :schema '(:type object :properties (:x (:type string)))
                    :stream t :dry-run t)))
           (info (gptel-fsm-info fsm))
           (data (plist-get info :data)))
      (should (plist-get data :response_format))
      (should (plist-get info :stream)))))


;;;; gptel-send with Org properties

(ert-deftest gptel-org-test-send-gptel-system-property ()
  "GPTEL_SYSTEM property is reflected in the final payload via `gptel-send'.
`gptel-org--send-with-props' let-binds `gptel-system-prompt' from the
property value, which then propagates through `gptel-request' to the
payload."
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert "* Test\n")
    (org-set-property "GPTEL_SYSTEM" "from org property")
    (insert "\nHello world")
    (goto-char (point-max))
    (let ((gptel-backend (alist-get 'openai gptel-test-backends))
          (gptel-model 'gptel-4o-mini)
          (gptel-stream nil)
          (gptel-track-response nil)
          (gptel-prompt-transform-functions nil)
          (inhibit-message t))
      (with-gptel-send-mock
        (gptel-send))
      (let ((messages (plist-get (plist-get (gptel-fsm-info gptel-test--captured-fsm) :data)
                                 :messages)))
        (should (equal (plist-get (aref messages 0) :content) "from org property"))))))

(ert-deftest gptel-org-test-send-gptel-system-overrides-function ()
  "GPTEL_SYSTEM string overrides a buffer-local function-valued system prompt.
The request buffer has a function as buffer-local `gptel-system-prompt',
but `gptel-org--send-with-props' let-binds it to the GPTEL_SYSTEM string
value (property wins via `seq-mapn' + `(or a b)').  The function should
NOT be evaluated."
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert "* Test\n")
    (org-set-property "GPTEL_SYSTEM" "from property")
    (insert "\nHello world")
    (goto-char (point-max))
    ;; Buffer-local function that would produce a different value
    (setq-local gptel-system-prompt (lambda () "should not be called"))
    (let ((gptel-backend (alist-get 'openai gptel-test-backends))
          (gptel-model 'gptel-4o-mini)
          (gptel-stream nil)
          (gptel-track-response nil)
          (gptel-prompt-transform-functions nil)
          (inhibit-message t))
      (with-gptel-send-mock
        (gptel-send))
      (let ((messages (plist-get (plist-get (gptel-fsm-info gptel-test--captured-fsm) :data)
                                 :messages)))
        ;; Property string wins over function
        (should (equal (plist-get (aref messages 0) :content) "from property"))))))

(ert-deftest gptel-org-test-send-gptel-preset-system ()
  "GPTEL_PRESET's :system overrides GPTEL_SYSTEM in the final payload.
`gptel-org--send-with-props' let-binds both `gptel--preset' (from
GPTEL_PRESET) and `gptel-system-prompt' (from GPTEL_SYSTEM).  But
`gptel-org--create-prompt-buffer' then applies the preset buffer-locally
in the prompt construction buffer, which overrides `gptel-system-prompt'
with the preset's :system value."
  (let ((gptel--known-presets (copy-tree gptel--known-presets)))
    (gptel-make-preset 'test-preset-org
      :system "preset system message")
    (unwind-protect
        (with-temp-buffer
          (delay-mode-hooks (org-mode))
          (insert "* Test\n")
          (org-set-property "GPTEL_PRESET" "test-preset-org")
          (org-set-property "GPTEL_SYSTEM" "should be overridden")
          (insert "\nHello world")
          (goto-char (point-max))
          (let ((gptel-backend (alist-get 'openai gptel-test-backends))
                (gptel-model 'gptel-4o-mini)
                (gptel-stream nil)
                (gptel-track-response nil)
                (gptel-prompt-transform-functions nil)
                (inhibit-message t))
            (with-gptel-send-mock
              (gptel-send))
            (let ((messages (plist-get (plist-get (gptel-fsm-info gptel-test--captured-fsm) :data)
                                       :messages)))
              ;; Preset :system overrides GPTEL_SYSTEM
              (should (equal (plist-get (aref messages 0) :content)
                             "preset system message")))))
      ;; Cleanup
      (setq gptel--known-presets
            (assq-delete-all 'test-preset-org gptel--known-presets)))))

(ert-deftest gptel-org-test-send-gptel-preset-function-system ()
  "Preset with function-valued :system is evaluated correctly.
This tests the inverse case: the request buffer has a string system
prompt, but the preset (applied in the prompt construction buffer) sets
a function.  The function check at `gptel-request' line 2260 must catch
this and evaluate the function in the request buffer context."
  (let ((gptel--known-presets (copy-tree gptel--known-presets)))
    (gptel-make-preset 'test-preset-fn
      :system (lambda () "from preset function"))
    (unwind-protect
        (with-temp-buffer
          (delay-mode-hooks (org-mode))
          (insert "* Test\n")
          (org-set-property "GPTEL_PRESET" "test-preset-fn")
          (insert "\nHello world")
          (goto-char (point-max))
          ;; Set a context variable in the request buffer to verify
          ;; the function evaluates here
          (setq-local gptel-test--buf-local "request-context")
          (let ((gptel-backend (alist-get 'openai gptel-test-backends))
                (gptel-model 'gptel-4o-mini)
                (gptel-stream nil)
                (gptel-track-response nil)
                (gptel-prompt-transform-functions nil)
                (gptel-system-prompt "buffer local string") ; would be used if not for preset
                (inhibit-message t))
            (with-gptel-send-mock
              (gptel-send))
            (let ((messages (plist-get (plist-get (gptel-fsm-info gptel-test--captured-fsm) :data)
                                       :messages)))
              ;; Preset's function :system was evaluated
              (should (equal (plist-get (aref messages 0) :content)
                             "from preset function")))))
      ;; Cleanup
      (setq gptel--known-presets
            (assq-delete-all 'test-preset-fn gptel--known-presets)))))

(ert-deftest gptel-org-test-send-gptel-model-property ()
  "GPTEL_MODEL property is reflected in the final payload via `gptel-send'."
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert "* Test\n")
    (org-set-property "GPTEL_MODEL" "gpt-4o-mini")
    (insert "\nHello world")
    (goto-char (point-max))
    (let ((gptel-backend (alist-get 'openai gptel-test-backends))
          (gptel-stream nil)
          (gptel-track-response nil)
          (gptel-prompt-transform-functions nil)
          (inhibit-message t))
      (with-gptel-send-mock
        (gptel-send))
      (let ((data (plist-get (gptel-fsm-info gptel-test--captured-fsm) :data)))
        (should (equal (plist-get data :model) "gpt-4o-mini"))))))

(ert-deftest gptel-org-test-send-gptel-system-multiline ()
  "GPTEL_SYSTEM with escaped newlines is unescaped in the payload.
`gptel-org--entry-properties' replaces literal \"\\n\" with actual
newlines via (string-replace \"\\\\n\" \"\\n\" system)."
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert "* Test\n")
    (org-set-property "GPTEL_SYSTEM" "Line one\\nLine two")
    (insert "\nHello world")
    (goto-char (point-max))
    (let ((gptel-backend (alist-get 'openai gptel-test-backends))
          (gptel-model 'gptel-4o-mini)
          (gptel-stream nil)
          (gptel-track-response nil)
          (gptel-prompt-transform-functions nil)
          (inhibit-message t))
      (with-gptel-send-mock
        (gptel-send))
      (let ((messages (plist-get (plist-get (gptel-fsm-info gptel-test--captured-fsm) :data)
                                 :messages)))
        (should (equal (plist-get (aref messages 0) :content)
                       "Line one\nLine two"))))))

(provide 'gptel-org-test)
;;; gptel-org-test.el ends here
