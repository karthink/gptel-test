;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'gptel)
(require 'gptel-org)
(require 'gptel-openai)
(require 'gptel-anthropic)
(require 'gptel-gemini)
(require 'gptel-ollama)
(require 'gptel-kagi)
(require 'cl-lib)
(require 'map)

;; Read an API output stream and construct the response and tool calls

;;; General

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

;;; Anthropic

(ert-deftest gptel-test-anthropic-tool-stream-parallel ()
  (let* ((backend (alist-get 'anthropic gptel-test-backends))
         (testinfo
          `(:backend ,backend
            :data (:messages [(:role "user"
                               :content
                               "My Emacs *scratch* buffer contains a list of directory names.  Create directories under \"/tmp/\" with each of these names.")])))
         (response (with-temp-buffer
                     (insert-file-contents "examples/anthropic-tool-stream-parallel.txt")
                     (goto-char (point-min))
                     (gptel-curl--parse-stream backend testinfo))))
    ;; text parsing: Is the return value of the parser correct?
    (should (string= response "\n\nI'll create these directories in /tmp/:"))
    ;; tool parsing: Is it capturing the tool calls in testinfo, the info plist?
    (let* ((tool-use (plist-get testinfo :tool-use)))
      (cl-loop for tool-call-result in ;note: prefix "toolu_" has been stripped from the ids
               '((:id "toolu_01Q7ptGyMTHtj8NTAu1q93qS" :name "make_directory" :input nil :args (:parent "/tmp" :name "testdir3"))
                 (:id "toolu_01Jqbxt5WYUt6RfpoBCHpA6X" :name "make_directory" :input nil :args (:parent "/tmp" :name "testdir2"))
                 (:id "toolu_01GwpAyin6URSPn7ZuGSjXKz" :name "make_directory" :input nil :args (:parent "/tmp" :name "testdir1")))
               for id = (plist-get tool-call-result :id)
               for tool-call-found = (cl-find-if (lambda (tool-use-each) (equal (plist-get tool-use-each :id) id)) tool-use)
               do (should (equal (plist-get tool-call-found :args)
                                 (plist-get tool-call-result :args)))))
    ;; Messages list update in prompts: Has it updated the list of prompts with the tool calls?
    (let* ((last-message (map-nested-elt testinfo '(:data :messages 1)))
           (role (plist-get last-message :role))
           (content (append (plist-get last-message :content) nil)))
      (should (equal role "assistant"))
      (dolist (tool-call '((:type "tool_use" :id "toolu_01Q7ptGyMTHtj8NTAu1q93qS" :name "make_directory" :input
                            (:parent "/tmp" :name "testdir3"))
                           (:type "tool_use" :id "toolu_01Jqbxt5WYUt6RfpoBCHpA6X" :name "make_directory" :input
                            (:parent "/tmp" :name "testdir2"))
                           (:type "tool_use" :id "toolu_01GwpAyin6URSPn7ZuGSjXKz" :name "make_directory" :input
                            (:parent "/tmp" :name "testdir1"))))
        (should (member tool-call content))))))


