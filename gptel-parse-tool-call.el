;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'gptel)
(require 'gptel-org)
(require 'gptel-test-backends)
(require 'cl-lib)
(require 'map)

;; Unit tests for `gptel--inject-tool-args'
(ert-deftest gptel-test-inject-tool-args ()
  "Ensure that tool argument injection into messages arrays works for all backends."
  (skip-unless (fboundp 'gptel--inject-tool-args))
  (let* ((backend (alist-get 'openai gptel-test-backends))
         (testinfo
          `( :backend ,backend
             :data (:messages
                    [( :role "system" :content
                       "You are a large language model living in Emacs and a helpful assistant.")
                     ( :role "user" :content
                       [(:type "text" :text "what are the reviews like for Hamlet (1996)?")])
                     ( :role "assistant" :content :null :tool_calls
                       [( :id "fc_bb3ceed0-6c06-49ca-8851-8bde396c85aa" :type "function"
                          :function ( :name "WebSearch"
                                      :arguments "{\"count\":5,\"query\":\"Hamlet 1996 reviews\"}")
                          :index 0)]
                       :reasoning
                       "We need up-to-date info about reviews for the film Hamlet (1996). \
We need to search the web. Use functions.WebSearch.")])))
         (tool-call '( :id "fc_bb3ceed0-6c06-49ca-8851-8bde396c85aa" :name "WebSearch"
                       :args (:count 5 :query "Hamlet 1996 reviews")))
         (new-args '(:count 10 :query "Some other movie")))
    (gptel--inject-tool-args backend (plist-get testinfo :data) tool-call new-args)
    (should (equal (map-nested-elt testinfo '(:data :messages 2 :tool_calls 0 :function :arguments))
                   (gptel--json-encode new-args))))

  (let* ((backend (alist-get 'anthropic gptel-test-backends))
         (testinfo
          `( :backend ,backend
             :data (:messages
                    [( :role "user"
                       :content [(:type "text" :text
                                        "what are the reviews like for Hamlet (1996)?"
                                        :cache_control (:type "ephemeral"))])
                     ( :role "assistant"
                       :content [(:type "text" :text
                                        "I'll search for reviews of Hamlet (1996) for you.")
                                 (:type "tool_use" :id "toolu_01J1ZgM54cxXMsEwQn8zMJxH" :name
                                        "WebSearch" :input (:query "Hamlet 1996 reviews"))])])))
         (tool-call '( :id "toolu_01J1ZgM54cxXMsEwQn8zMJxH" :name "WebSearch" :input nil
                       :args (:query "Hamlet 1996 reviews")))
         (new-args '(:query "Some other moview")))
    (gptel--inject-tool-args backend (plist-get testinfo :data) tool-call new-args)
    (should (eq (map-nested-elt testinfo '(:data :messages 1 :content 1 :input))
                new-args)))

  (let* ((backend (alist-get 'gemini gptel-test-backends))
         (testinfo
          `( :backend ,backend
             :data (:contents
                    [( :role "user" :parts
                       [(:text "what are the reviews like for Hamlet (1996)?")])
                     ( :role "model" :parts
                       [(:text "")
                        (:functionCall
                         (:name "WebSearch" :args
                                (:query "Hamlet movie 1996 reviews critical reception"))
                         :thoughtSignature "EtEDCs4DAb4")])])))
         (tool-call '( :name "WebSearch"
                       :args (:query "Hamlet movie 1996 reviews critical reception")))
         (new-args '(:query "Some other movie")))
    (gptel--inject-tool-args backend (plist-get testinfo :data) tool-call new-args)
    (should (eq (map-nested-elt testinfo '(:data :contents 1 :parts 1 :functionCall :args))
                new-args)))

  (let* ((backend (alist-get 'ollama gptel-test-backends))
         (testinfo
          `( :backend ,backend
             :model "qwen3"
             :data
             (:messages
              [( :role "user"
                 :content "What are the current weather conditions and temperature in New York and London?")
               ( :role "assistant"
                 :tool_calls [( :type "function"
                                :function ( :index 0 :name "get_temperature"
                                            :arguments (:city "New York")))
                              ( :type "function"
                                :function ( :index 1 :name "get_conditions"
                                            :arguments (:city "New York")))
                              ( :type "function"
                                :function ( :index 2 :name "get_temperature"
                                            :arguments (:city "London")))
                              ( :type "function"
                                :function ( :index 3 :name "get_conditions"
                                            :arguments (:city "London")))])]
              :stream :json-false)))
         (tool-call '( :name "get_temperature" :args (:city "London")))
         (new-args '(:city "Khartoum")))
    (gptel--inject-tool-args backend (plist-get testinfo :data) tool-call new-args)
    (should (eq (map-nested-elt testinfo '(:data :messages 1 :tool_calls 2 :function :arguments))
                new-args))))

;; ;; Ollama sample messages array
;; ( :model "qwen3"
;;   :messages
;;   [( :role "user"
;;      :content "What are the current weather conditions and temperature in New York and London?")
;;    ( :role "assistant"
;;      :tool_calls [( :type "function"
;;                     :function ( :index 0 :name "get_temperature"
;;                                 :arguments (:city "New York")))
;;                   ( :type "function"
;;                     :function ( :index 1 :name "get_conditions"
;;                                 :arguments (:city "New York")))
;;                   ( :type "function"
;;                     :function ( :index 2 :name "get_temperature"
;;                                 :arguments (:city "London")))
;;                   ( :type "function"
;;                     :function ( :index 3 :name "get_conditions"
;;                                 :arguments (:city "London")))])
;;    ( :role "tool" :tool_name "get_temperature" :content "22°C")
;;    ( :role "tool" :tool_name "get_conditions" :content "Partly cloudy")
;;    ( :role "tool" :tool_name "get_temperature" :content "15°C")
;;    ( :role "tool" :tool_name "get_conditions" :content "Rainy")]
;;   :stream :json-false)

;; Read an API output stream and construct the response and tool calls

;;; General

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


