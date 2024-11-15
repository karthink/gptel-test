;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'gptel)
(require 'gptel-org)
(require 'gptel-openai)
(require 'gptel-anthropic)
(require 'gptel-gemini)
(require 'gptel-ollama)
(require 'gptel-kagi)

;; Go from a buffer with included metadata to a list of prompts.

;;; General

(defmacro gptel-test-prompt-creation (name result-file &rest body)
  (declare (indent 2))
  `(ert-deftest
       ,(intern (concat "gptel-test-prompt-" name))
       ()
     (should (equal
              (with-temp-buffer
                (save-excursion (insert-file ,result-file))
                (read (current-buffer)))
              ,(macroexp-progn body)))))

(defmacro with-gptel-chat-file (filename backend-sym &optional model &rest body)
  `(let* ((gptel-track-response t)
         (org-inhibit-startup t)
         (buf (delay-mode-hooks (find-file-noselect ,filename)))
         (gptel--num-messages-to-send nil)
         (gptel-context--alist nil)
         (gptel--system-message gptel-test-system-message)
         (inhibit-message t))
    (with-current-buffer buf
      (deactivate-mark)
      (gptel--restore-state)
      (prog1
          (let* ((gptel-backend (alist-get ',backend-sym gptel-test-backends))
                 (gptel-model (or ,model (car (gptel-backend-models gptel-backend)))))
            ,(macroexp-progn body))
        (set-buffer-modified-p nil)))))

(defconst gptel-test-system-message
  "To assist: Be very terse.  Respond in under 100 words if possible.  Speak in specific, topic relevant terminology. Do NOT hedge or qualify. Speak directly and be willing to make creative guesses. Explain your reasoning. if you don’t know, say you don’t know. Never apologize.  Ask questions when unsure."
  "System message for prompt creation tests.  Do not edit, as this
is used in the test data.")

(defvar gptel-test-backends
  `((openai    . ,(gptel--make-openai
                :name "OpenAI" :models '(gpt-4o-mini)))
    (anthropic . ,(gptel--make-anthropic
                   :name "Claude" :models '(claude-3-5-sonnet-20240620)))
    (gemini    . ,(gptel--make-gemini
                :name "Gemini" :models '(gemini-1.5-pro-latest)))
    (ollama    . ,(gptel--make-ollama
                :name "Ollama" :models '(testmodel)))
    (kagi      . ,(gptel--make-kagi :name "Kagi" :models '(fastgpt summarize:agnes))))
  "Dummy models for testing gptel.")

;;; Setup code

(cl-defun gptel-test-write-prompt-data (source-file dest-file &key backend branching-context scope)
  "Generate test prompts.

SOURCE-FILE is an Org or MD file

DEST-FILE is the .eld file to write the prompt to

Keys:

BACKEND is one of openai, gemini, anthropic, ollama or kagi, see
`gptel-test-backends'.

BRANCHING-CONTEXT is a boolean specifying if branching context
should be used in Org mode.

SCOPE is the extent of the buffer to create a prompt from.  It
can be nil (for the whole buffer), a point value or the symbol
`region' for a specific region.  (The choice of region is
preselected for now, see below.)"
  (declare (indent 1))
  (let ((buf (find-file-noselect source-file))
        (file-name-handler-alist nil)
        (coding-system-for-write 'utf-8)
        (gptel-prompt-prefix-alist
         '((markdown-mode . "#### ")
           (org-mode . "*Prompt*: ")
           (text-mode . "#### ")))
        (gptel-response-prefix-alist
         '((markdown-mode . "")
           (org-mode . "*Response*:
")
           (text-mode . "")))
        (gptel--system-message gptel-test-system-message)
        (write-region-annotate-functions nil)
        (write-region-post-annotation-function nil)
        (gptel-org-branching-context branching-context))
    (with-current-buffer buf
      (deactivate-mark)
      (gptel--restore-state)
      ;; ;; For region
      (when (eq scope 'region)
        (goto-char 1046)
        (push-mark)
        (goto-char 2383)
        (activate-mark))
      (setq-local gptel-backend (alist-get backend gptel-test-backends)
                  gptel-model (car (gptel-backend-models gptel-backend)))
      (write-region (prin1-to-string
                     (gptel--create-prompt
                      (and (integerp scope) scope)))
                    nil dest-file)
      (set-buffer-modified-p nil))))

;; MD file end of prompt: 1792
;; Org file region bounds: (1046 . 2383)
;; Org file end of prompt: (point-max)

;; (gptel-test-write-prompt-data (expand-file-name "examples/prompt-creation.md")
;;                               (expand-file-name "examples/gemini-prompt-md.eld")
;;                               :backend 'gemini)

;; (gptel-test-write-prompt-data (expand-file-name "examples/prompt-creation.md")
;;                               (expand-file-name "examples/openai-prompt-md.eld")
;;                               :backend 'openai)

;; (gptel-test-write-prompt-data (expand-file-name "examples/prompt-creation.md")
;;                               (expand-file-name "examples/anthropic-prompt-md.eld")
;;                               :backend 'anthropic)

;; (gptel-test-write-prompt-data (expand-file-name "examples/prompt-creation.md")
;;                               (expand-file-name "examples/ollama-prompt-md.eld")
;;                               :backend 'ollama)

;; (gptel-test-write-prompt-data (expand-file-name "examples/branching-prompt-creation.org")
;;                               (expand-file-name "examples/openai-prompt-branching-org.eld")
;;                               :backend 'openai
;;                               :branching-context t)

;; (gptel-test-write-prompt-data (expand-file-name "examples/branching-prompt-creation.org")
;;                               (expand-file-name "examples/anthropic-prompt-branching-org.eld")
;;                               :backend 'anthropic
;;                               :branching-context t)

;; (gptel-test-write-prompt-data (expand-file-name "examples/branching-prompt-creation.org")
;;                               (expand-file-name "examples/gemini-prompt-region-org.eld")
;;                               :backend 'gemini)

;; (gptel-test-write-prompt-data (expand-file-name "examples/branching-prompt-creation.org")
;;                               (expand-file-name "examples/ollama-prompt-branching-org.eld")
;;                              :backend 'ollama
;;                              :branching-context t)
;;; Markdown
;;;; OpenAI
(gptel-test-prompt-creation
    "openai-md" "examples/openai-prompt-md.eld"
  (with-gptel-chat-file
   "examples/prompt-creation.md" openai nil
   (gptel--create-prompt 1792)))

;;;; Anthropic
(gptel-test-prompt-creation
    "anthropic-md" "examples/anthropic-prompt-md.eld"
  (with-gptel-chat-file
   "examples/prompt-creation.md" anthropic nil
   (gptel--create-prompt 1792)))

;;;; Gemini
(gptel-test-prompt-creation
    "gemini-md" "examples/gemini-prompt-md.eld"
  (with-gptel-chat-file
   "examples/prompt-creation.md" gemini nil
   (gptel--create-prompt 1792)))

;;;; Ollama
(gptel-test-prompt-creation
    "ollama-md" "examples/ollama-prompt-md.eld"
  (with-gptel-chat-file
   "examples/prompt-creation.md" ollama nil
   (gptel--create-prompt 1792)))

;;;; Kagi
;; TODO: Test for Kagi backend

;;; Org mode with branching
;;;; OpenAI
(gptel-test-prompt-creation
    "openai-branching-org" "examples/openai-prompt-branching-org.eld"
  (with-gptel-chat-file
   "examples/branching-prompt-creation.org" openai nil
   (let ((gptel-org-branching-context t))
     (gptel--create-prompt (point-max)))))

;;;; Anthropic
(gptel-test-prompt-creation
    "anthropic-branching-org" "examples/anthropic-prompt-branching-org.eld"
  (with-gptel-chat-file
   "examples/branching-prompt-creation.org" anthropic nil
   (let ((gptel-org-branching-context t))
     (gptel--create-prompt (point-max)))))

;;;; Gemini
(gptel-test-prompt-creation
    "gemini-branching-org" "examples/gemini-prompt-branching-org.eld"
  (with-gptel-chat-file
   "examples/branching-prompt-creation.org" gemini nil
   (let ((gptel-org-branching-context t))
     (gptel--create-prompt (point-max)))))

;;;; Ollama
(gptel-test-prompt-creation
    "ollama-branching-org" "examples/ollama-prompt-branching-org.eld"
  (with-gptel-chat-file
   "examples/branching-prompt-creation.org" ollama nil
   (let ((gptel-org-branching-context t))
     (gptel--create-prompt (point-max)))))

;;;; Kagi
;;;;; FastGPT
;; TODO
;;;;; Summarizer
;; TODO

;;;; Branching context edge cases (#476)
(ert-deftest gptel-test-prompt-branching-heading-at-top ()
  "Test branching prompt creation when there is an Org heading at
the top of the buffer.

This case requires special logic because of the behavior of
`org-element-lineage', see `gptel-org--create-prompt' for
details."
    (let ((gptel-track-response t)
          (gptel-org-branching-context t)
          (gptel--num-messages-to-send nil)
          (gptel-context--alist nil)
          (gptel-backend (alist-get 'openai gptel-test-backends))
          (gptel--system-message gptel-test-system-message)
          (text "*** This is heading 1\n\nSome details\n\n**** This is heading 2")
          (result '((:role "system" :content "To assist: Be very terse.  Respond in under 100 words if possible.  Speak in specific, topic relevant terminology. Do NOT hedge or qualify. Speak directly and be willing to make creative guesses. Explain your reasoning. if you don’t know, say you don’t know. Never apologize.  Ask questions when unsure.")
                   (:role "user" :content "*** This is heading 1

Some details

**** This is heading 2")))
          (inhibit-message t)
          (org-inhibit-startup t))
      (should (equal
               (with-temp-buffer
                 (delay-mode-hooks
                   (insert text)
                   (org-mode)
                   (gptel--create-prompt (point-max))))
               result))))

;;; Org-mode without branching
;;;; OpenAI
(gptel-test-prompt-creation "openai-org" "examples/openai-prompt-org.eld"
  (with-gptel-chat-file
   "examples/branching-prompt-creation.org" openai nil
   (let ((gptel-org-branching-context nil))
     (gptel--create-prompt (point-max)))))

;;;; Anthropic
(gptel-test-prompt-creation "anthropic-org" "examples/anthropic-prompt-org.eld"
  (with-gptel-chat-file
   "examples/branching-prompt-creation.org" anthropic nil
   (let ((gptel-org-branching-context nil))
     (gptel--create-prompt (point-max)))))

;;;; Gemini
(gptel-test-prompt-creation "gemini-org" "examples/gemini-prompt-org.eld"
  (with-gptel-chat-file
   "examples/branching-prompt-creation.org" gemini nil
   (let ((gptel-org-branching-context nil))
     (gptel--create-prompt (point-max)))))

;;;; Ollama
(gptel-test-prompt-creation "ollama-org" "examples/ollama-prompt-org.eld"
  (with-gptel-chat-file
   "examples/branching-prompt-creation.org" ollama nil
   (let ((gptel-org-branching-context nil))
     (gptel--create-prompt (point-max)))))

;;; Org-mode with region

;;;; OpenAI
(gptel-test-prompt-creation "openai-region-org" "examples/openai-prompt-region-org.eld"
  (with-gptel-chat-file
   "examples/branching-prompt-creation.org" openai nil
   (let ((gptel-org-branching-context nil))
     (goto-char 1046) (push-mark)
     (goto-char 2383) (activate-mark)
     (gptel--create-prompt))))

;;;; Anthropic
(gptel-test-prompt-creation "anthropic-region-org" "examples/anthropic-prompt-region-org.eld"
  (with-gptel-chat-file
   "examples/branching-prompt-creation.org" anthropic nil
   (let ((gptel-org-branching-context nil))
     (goto-char 1046) (push-mark)
     (goto-char 2383) (activate-mark)
     (gptel--create-prompt))))

;;;; Gemini
(gptel-test-prompt-creation "gemini-region-org" "examples/gemini-prompt-region-org.eld"
  (with-gptel-chat-file
   "examples/branching-prompt-creation.org" gemini nil
   (let ((gptel-org-branching-context nil))
     (goto-char 1046) (push-mark)
     (goto-char 2383) (activate-mark)
     (gptel--create-prompt))))

;;;; Ollama
(gptel-test-prompt-creation "ollama-region-org" "examples/ollama-prompt-region-org.eld"
  (with-gptel-chat-file
   "examples/branching-prompt-creation.org" ollama nil
   (let ((gptel-org-branching-context nil))
     (goto-char 1046) (push-mark)
     (goto-char 2383) (activate-mark)
     (gptel--create-prompt))))
