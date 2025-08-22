;;; gptel-unit-tests.el --- Gptel Unit Tests  -*- lexical-binding: t; -*-
(require 'ert)
(require 'gptel)

(defmacro setup (&rest body)
  `(let ((gptel-prompt-prefix-alist
          '((fundamental-mode . "*Prompt*: ")))
         (gptel-response-prefix-alist
          '((fundamental-mode . "*Response*: ")))
         (gptel-response-separator "\n\n"))
     ,(macroexp-progn body)))

(defvar gptel-test-openai
  (gptel-make-openai "openai-test"
   :models '((testmodel
              :capabilities (media)
              :mime-types ("image/png"))))
  "Dummy OpenAI backend for testing gptel.")
(setf (alist-get "openai-test" gptel--known-backends
                 nil t #'equal)
      nil)

(ert-deftest gptel-test-prefix-trimming ()
  (setup
   ;; Empty string is nil
   (should (equal nil (gptel--trim-prefixes "")))

   ;; Prefixes trim to nil
   (should (equal nil (gptel--trim-prefixes (gptel-prompt-prefix-string))))
   (should (equal nil (gptel--trim-prefixes (gptel-response-prefix-string))))

   ;; Trimp both prefixes to nil
   (should (equal nil (gptel--trim-prefixes
                       (format " %s  %s "
                               (gptel-prompt-prefix-string)
                               (gptel-response-prefix-string)))))
   ;; Trim extra whitespace
   (should (equal nil (gptel--trim-prefixes
                       (format "\n\t %s \n\t  %s \n\t"
                               (gptel-prompt-prefix-string)
                               (gptel-response-prefix-string)))))
   ;; Trim it all down to FOO
   (should (equal "FOO" (gptel--trim-prefixes
                         (format "\n\t %s \n\tFOO  %s \n\t"
                                 (gptel-prompt-prefix-string)
                                 (gptel-response-prefix-string)))))

   ;; Trim the final response prefix and whitespace
   (should (equal "DERP\n\t *Prompt*:  \n\tFOO"
                  (gptel--trim-prefixes
                   (concat "DERP"
                           "\n\t "
                           (gptel-prompt-prefix-string)
                           " \n\tFOO  "
                           (gptel-response-prefix-string)
                           " \n\t"))))))

;;; Tests for media parsing in buffers: `gptel--parse-media-links'
(ert-deftest gptel-test-media-link-parsing-org-1 ()
  (let ((mediatext "Some text here, just checking.")
        (buftext "Some text followed by a link:

[[file:/tmp/medialinks.txt]]

then more text, then another link

[[file:/tmp/medialinks.yaml]]

then some more text to end."))
    (unwind-protect
        (progn
          (with-temp-file "/tmp/medialinks.yaml" (insert mediatext))
          (with-temp-file "/tmp/medialinks.txt" (insert mediatext))
          (let ((org-inhibit-startup t)
                (gptel-backend gptel-test-openai)
                (gptel-model 'testmodel))
            (with-temp-buffer
              (insert buftext)
              (delay-mode-hooks (org-mode))
              (should (equal (gptel--parse-media-links
                              major-mode (point-min) (point-max))
                             '((:text "Some text followed by a link:\n\n")
                               (:textfile "/tmp/medialinks.txt")
                               (:text "\n\nthen more text, then another link\n\n")
                               (:textfile "/tmp/medialinks.yaml")
                               (:text "\n\nthen some more text to end.")))))))
      (delete-file "/tmp/medialinks.yaml")
      (delete-file "/tmp/medialinks.txt"))))

(ert-deftest gptel-test-media-link-parsing-org-2 ()
  (let ((mediatext "Some text here, just checking.")
        (buftext "Some text followed by a link:

[[file:/tmp/medialinks.txt]]

then more text, then an image

[[file:./examples/hundred.png]]

then some more text to end."))
    (unwind-protect
        (progn
          (with-temp-file "/tmp/medialinks.txt" (insert mediatext))
          (let ((org-inhibit-startup t)
                (gptel-backend gptel-test-openai)
                (gptel-model 'testmodel))
            (with-temp-buffer
              (insert buftext)
              (delay-mode-hooks (org-mode))
              (should (equal (gptel--parse-media-links
                              major-mode (point-min) (point-max))
                             '((:text "Some text followed by a link:\n\n")
                               (:textfile "/tmp/medialinks.txt")
                               (:text "\n\nthen more text, then an image\n\n")
                               (:media "./examples/hundred.png" :mime "image/png")
                               (:text "\n\nthen some more text to end.")))))))
      (delete-file "/tmp/medialinks.txt"))))

;;; Tests for parsing JSON schema supplied in various forms
(ert-deftest gptel-test-dispatch-schema-type () 
  "Shorthand form tests for `gptel--dispatch-schema-type'."
  (should (equal (gptel--dispatch-schema-type
                  "name, chemical_formula str, toxicity num")
                 '( :type "object" :properties ( :name (:type "string")
                                                 :chemical_formula (:type "string")
                                                 :toxicity (:type "number")))))
  (should (equal (gptel--dispatch-schema-type
                  "[name, chemical_formula string, toxicity number]")
                 (list :type "object"
                       :properties (list :items
                                         '( :type "array"
                                            :items
                                            ( :type "object"
                                              :properties ( :name (:type "string")
                                                            :chemical_formula (:type "string")
                                                            :toxicity (:type "number")))))
                       :required ["items"]
                       :additionalProperties :json-false)))
  (should (equal (gptel--dispatch-schema-type
                  "name: Colloquial name of compound
                   chemical_formula str: Formula for compound
                   toxicity int: 1-10 denoting toxicity to humans")
                 '( :type "object"
                    :properties ( :name ( :type "string"
                                          :description "Colloquial name of compound")
                                  :chemical_formula ( :type "string"
                                                      :description "Formula for compound")
                                  :toxicity ( :type "integer"
                                              :description "1-10 denoting toxicity to humans")))))
  (should (equal (gptel--dispatch-schema-type
                  "[name: Colloquial name of compound
                    chemical_formula str: Formula for compound
                    toxicity bool: whether the compound is toxic   ]")
                 '( :type "object"
                    :properties
                    ( :items
                      ( :type "array"
                        :items
                        ( :type "object"
                          :properties
                          ( :name ( :type "string"
                                    :description "Colloquial name of compound")
                            :chemical_formula ( :type "string"
                                                :description "Formula for compound")
                            :toxicity ( :type "boolean"
                                        :description "whether the compound is toxic")))))
                    :required ["items"]
                    :additionalProperties :json-false))))

(ert-deftest gptel-test-dispatch-schema-type-advanced ()
  "Advanced shorthand form test for `gptel--dispatch-schema-type'."
  ;; Test with
  ;; - missing type
  ;; - missing description
  ;; - missing ":" separator
  ;; - missing type, description and separator
  ;; - leading and trailing whitespace
  (should
   (equal (gptel--dispatch-schema-type
           "  [name  str: Name of cat
                      age   num
                      hobby
                      bio      : One-line biography for cat    ]

           ")
          '( :type "object"
             :properties
             ( :items
               ( :type "array" :items
                 ( :type "object" :properties
                   ( :name ( :type "string" :description "Name of cat")
                     :age ( :type "number")
                     :hobby ( :type "string")
                     :bio ( :type "string" :description
                            "One-line biography for cat")))))
             :required ["items"] :additionalProperties :json-false))))

;; ;; Markdown mode is not installed in emacs -q

;; (ert-deftest gptel-test-media-link-parsing-md-1 ()
;;   (let ((mediatext "Some text here, just checking.")
;;         (buftext "Some text followed by a link:

;; [medialinks](/tmp/medialinks.txt)

;; then more text, then another link

;; [some text](/tmp/medialinks.yaml)

;; then some more text to end."))
;;     (unwind-protect
;;         (progn
;;           (with-temp-file "/tmp/medialinks.yaml" (insert mediatext))
;;           (with-temp-file "/tmp/medialinks.txt" (insert mediatext))
;;           (let ((gptel-backend gptel-test-openai)
;;                 (gptel-model 'testmodel))
;;             (with-temp-buffer
;;               (insert buftext)
;;               (delay-mode-hooks (markdown-mode))
;;               (should (equal (gptel--parse-media-links
;;                               major-mode (point-min) (point-max))
;;                              '((:text "Some text followed by a link:\n\n")
;;                                (:textfile "/tmp/medialinks.txt")
;;                                (:text "\n\nthen more text, then another link\n\n")
;;                                (:textfile "/tmp/medialinks.yaml")
;;                                (:text "\n\nthen some more text to end.")))))))
;;       (delete-file "/tmp/medialinks.yaml")
;;       (delete-file "/tmp/medialinks.txt"))))

;; (ert-deftest gptel-test-media-link-parsing-md-2 ()
;;   (let ((mediatext "Some text here, just checking.")
;;         (buftext "Some text followed by a link:

;; [medialinks](/tmp/medialinks.txt)

;; then more text, then an image

;; ![an image](./examples/hundred.png)

;; then some more text to end."))
;;     (unwind-protect
;;         (progn
;;           (with-temp-file "/tmp/medialinks.txt" (insert mediatext))
;;           (let ((gptel-backend gptel-test-openai)
;;                 (gptel-model 'testmodel))
;;             (with-temp-buffer
;;               (insert buftext)
;;               (delay-mode-hooks (markdown-mode))
;;               (should (equal (gptel--parse-media-links
;;                               major-mode (point-min) (point-max))
;;                              '((:text "Some text followed by a link:\n\n")
;;                                (:textfile "/tmp/medialinks.txt")
;;                                (:text "\n\nthen more text, then an image\n\n")
;;                                (:media "./examples/hundred.png" :mime "image/png")
;;                                (:text "\n\nthen some more text to end.")))))))
;;       (delete-file "/tmp/medialinks.txt"))))

(provide 'gptel-unit-tests)
;;; gptel-unit-tests.el ends here
