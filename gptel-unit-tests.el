;;; gptel-unit-tests.el --- Gptel Unit Tests  -*- lexical-binding: t; -*-
(require 'ert)
(require 'gptel)
(require 'gptel-test-backends)

(ert-deftest gptel-test-prefix-trimming ()
  (let ((gptel-prompt-prefix-alist
         '((fundamental-mode . "*Prompt*: ")))
        (gptel-response-prefix-alist
         '((fundamental-mode . "*Response*: ")))
        (gptel-response-separator "\n\n"))
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
                (gptel-backend (alist-get 'openai gptel-test-backends))
                (gptel-model 'gpt-4o-mini)) ;any model will do (no media)
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
                (gptel-backend (alist-get 'openai gptel-test-backends))
                (gptel-model 'gpt-4o-mini)) ;media-capable model
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

(ert-deftest gptel-test-media-link-parsing-md-1 ()
  (skip-unless (fboundp 'markdown-mode))
  (let ((mediatext "Some text here, just checking.")
        (buftext "Some text followed by a link:

[medialinks](/tmp/medialinks.txt)

then more text, then another link

[some text](/tmp/medialinks.yaml)

then some more text to end."))
    (unwind-protect
        (progn
          (with-temp-file "/tmp/medialinks.yaml" (insert mediatext))
          (with-temp-file "/tmp/medialinks.txt" (insert mediatext))
          (let ((gptel-backend (alist-get 'openai gptel-test-backends))
                (gptel-model 'gpt-4o-mini)) ;any model will do (no media)
            (with-temp-buffer
              (insert buftext)
              (delay-mode-hooks (markdown-mode))
              (should (equal (gptel--parse-media-links
                              major-mode (point-min) (point-max))
                             '((:text "Some text followed by a link:\n\n")
                               (:textfile "/tmp/medialinks.txt")
                               (:text "\n\nthen more text, then another link\n\n")
                               (:textfile "/tmp/medialinks.yaml")
                               (:text "\n\nthen some more text to end.")))))))
      (delete-file "/tmp/medialinks.yaml")
      (delete-file "/tmp/medialinks.txt"))))

(ert-deftest gptel-test-media-link-parsing-md-2 ()
  (skip-unless (fboundp 'markdown-mode))
  (let ((mediatext "Some text here, just checking.")
        (buftext "Some text followed by a link:

[medialinks](/tmp/medialinks.txt)

then more text, then an image

![an image](./examples/hundred.png)

then some more text to end."))
    (unwind-protect
        (progn
          (with-temp-file "/tmp/medialinks.txt" (insert mediatext))
          (let ((gptel-backend (alist-get 'openai gptel-test-backends))
                (gptel-model 'gpt-4o-mini)) ;media-capable model
            (with-temp-buffer
              (insert buftext)
              (delay-mode-hooks (markdown-mode))
              (should (equal (gptel--parse-media-links
                              major-mode (point-min) (point-max))
                             '((:text "Some text followed by a link:\n\n")
                               (:textfile "/tmp/medialinks.txt")
                               (:text "\n\nthen more text, then an image\n\n")
                               (:media "./examples/hundred.png" :mime "image/png")
                               (:text "\n\nthen some more text to end.")))))))
      (delete-file "/tmp/medialinks.txt"))))

;;; Tests for merging and summing plists
(ert-deftest gptel-test--merge-plists-1 ()
  "`gptel--merge-plists' should handle nil plist arguments."
  (should (equal (gptel--merge-plists nil nil) nil))
  (should (equal (gptel--merge-plists nil '(:a 1)) '(:a 1)))
  (should (equal (gptel--merge-plists '(:a 1) nil) '(:a 1))))

(ert-deftest gptel-test--merge-plists-2 ()
  "`gptel--merge-plists' should merge differing keys into the first plist."
  (let* ((first (list :a 1 :b 2))
         (second (list :b 3 :c 4))
         (result (gptel--merge-plists first second)))
    (should (eq result first))
    (should (equal result '(:a 1 :b 3 :c 4)))
    (should (equal first '(:a 1 :b 3 :c 4)))
    (should (equal second '(:b 3 :c 4))))
  (let* ((first (list :input 10))
         (result (gptel--merge-plists first '(:output 20 :cached 30))))
    (should (eq result first))
    (should (equal result '(:input 10 :output 20 :cached 30)))))

(ert-deftest gptel-test--sum-plists ()
  "`gptel--sum-plists' should handle nil plist arguments and return a fresh plist."
  (let* ((first (list :a 1 :b 2))
         (second (list :b 3 :c 4))
         (result (gptel--sum-plists nil first nil second)))
    (should (equal result '(:a 1 :b 5 :c 4)))
    (should-not (eq result first))
    (should-not (eq result second)))
  (let* ((first (list :input 10 :output 5 :cached 4))
         (second (list :cached 8 :input 10 :output 30 :cache 45))
         (result (gptel--sum-plists first second)))
    (should (equal result '(:input 20 :output 35 :cached 12 :cache 45)))
    (should-not (eq result first))
    (should-not (eq result second))))

;;; Test for declarative list modification DSL
(ert-deftest gptel-test--modify-value ()
  "Test `gptel--modify-value'."
  ;; string and string
  (should (equal (gptel--modify-value "original\n" "extra") "extra"))
  (should (equal (gptel--modify-value "original\n" '(:append "extra")) "original\nextra"))
  (should (equal (gptel--modify-value "original\n" '(:prepend "extra")) "extraoriginal\n"))
  (should (equal (gptel--modify-value "original\n" '(:function upcase)) "ORIGINAL\n"))
  ;; list and list
  (should (equal (gptel--modify-value '(a b c) '(d e f)) '(d e f)))
  (should (equal (gptel--modify-value '(a b c) '(:append (d e))) '(a b c d e)))
  (should (equal (gptel--modify-value '(a b c) '(:prepend (x y))) '(x y a b c)))
  (should (equal (gptel--modify-value '(1 2 3) '(:function reverse)) '(3 2 1)))
  (should (equal (gptel--modify-value '("hello") '(:append (" world"))) '("hello" " world")))
  (should (equal (gptel--modify-value '("world") '(:prepend ("hello "))) '("hello " "world")))
  ;; :merge test
  (should (equal (gptel--modify-value '(:a 1 :b 2) '(:merge (:b 3 :c 4))) '(:a 1 :b 3 :c 4)))
  (should (equal (gptel--modify-value '(:x "hello" :y 42) '(:merge (:x "world" :z nil)))
                 '(:x "world" :y 42 :z nil)))
  ;; :eval test
  (should (equal (gptel--modify-value "unused" '(:eval (+ 2 3))) 5))
  (should (equal (gptel--modify-value '(a b) '(:eval (reverse '(x y z)))) '(z y x)))
  ;; string and list combinations
  (should (equal (gptel--modify-value "hello" '(:append " world")) "hello world"))
  (should (equal (gptel--modify-value "world" '(:prepend "hello ")) "hello world"))
  ;; multiple operations
  (should (equal (gptel--modify-value "base" '(:append "1" :prepend "0")) "0base1"))
  (should (equal (gptel--modify-value '(b) '(:append (c) :prepend (a))) '(a b c)))
  ;; non-list mutation (edge cases)
  (should (equal (gptel--modify-value "original" 42) 42))
  (should (equal (gptel--modify-value '(a b c) :symbol) :symbol))
  ;; empty cases
  (should (equal (gptel--modify-value "" '(:append "text")) "text"))
  (should (equal (gptel--modify-value '() '(:append (a b))) '(a b)))
  (should (equal (gptel--modify-value "text" '(:prepend "")) "text"))
  (should (equal (gptel--modify-value '(a b) '(:prepend ())) '(a b))))

;;; Tests for header-line alignment

(ert-deftest gptel-test-header-line-pixel-alignment ()
  "Header-line uses pixel-based alignment when `string-pixel-width' is available."
  (skip-unless (fboundp 'string-pixel-width))
  (let* ((rhs "[test-model]")
         (spec `(space :align-to (- right (,(string-pixel-width rhs)))))
         (align-to (plist-get (cdr spec) :align-to))
         (offset (caddr align-to)))
    ;; Pixel path wraps the value in a list: (PIXELS)
    (should (listp offset))
    (should (numberp (car offset)))
    (should (> (car offset) 0))))

(ert-deftest gptel-test-header-line-char-fallback-offset ()
  "Header-line char fallback includes the +5 padding offset."
  (let* ((rhs "[test-model]")
         (spec `(space :align-to (- right ,(+ 5 (string-width rhs)))))
         (align-to (plist-get (cdr spec) :align-to))
         (offset (caddr align-to)))
    ;; Char path: offset is a plain number, not a list
    (should (numberp offset))
    ;; Should be string-width + 5
    (should (= offset (+ 5 (string-width rhs))))))


;;; Tests for presets

(ert-deftest gptel-test-apply-preset-symbol ()
  "Test `gptel--apply-preset' with a symbol."
  (let ((gptel-backend nil)
        (gptel-model nil)
        (gptel--preset nil)
        (test-backend (alist-get 'openai gptel-test-backends)))
    (gptel-make-preset 'test-preset-symbol
      :backend test-backend
      :model 'test-model)
    (gptel--apply-preset 'test-preset-symbol)
    (should (eq gptel--preset 'test-preset-symbol))
    (should (eq gptel-backend test-backend))
    (should (eq gptel-model 'test-model))))

(ert-deftest gptel-test-apply-preset-plist ()
  "Test `gptel--apply-preset' with a plist."
  (let ((gptel-backend nil)
        (gptel-model nil)
        (gptel--preset 'old-preset)
        (test-backend (alist-get 'openai gptel-test-backends)))
    (gptel--apply-preset `(:backend ,test-backend
                           :model test-model-plist))
    (should (eq gptel--preset 'old-preset)) ;; applying plist doesn't change gptel--preset
    (should (eq gptel-backend test-backend))
    (should (eq gptel-model 'test-model-plist))))

(ert-deftest gptel-test-apply-preset-parents ()
  "Test `gptel--apply-preset' with parents."
  (let ((gptel-backend nil)
        (gptel-model nil)
        (gptel-temperature nil)
        (gptel--preset nil)
        (test-backend (alist-get 'openai gptel-test-backends)))
    (gptel-make-preset 'test-parent-preset
      :backend test-backend
      :model 'test-parent-model
      :temperature 1.0)
    (gptel-make-preset 'test-child-preset
      :parents '(test-parent-preset)
      :model 'test-child-model)
    (gptel--apply-preset 'test-child-preset)
    (should (eq gptel--preset 'test-child-preset)) ;; not test-parent-preset
    (should (eq gptel-backend test-backend))
    (should (eq gptel-model 'test-child-model))
    (should (equal gptel-temperature 1.0))))

(ert-deftest gptel-test-apply-preset-anonymous-parents ()
  "Test `gptel--apply-preset' with anonymous parents."
  (let ((gptel-backend nil)
        (gptel-model nil)
        (gptel-temperature nil)
        (gptel--preset nil)
        (test-backend (alist-get 'openai gptel-test-backends)))
    (gptel-make-preset 'test-child-preset-anon
      :parents `((:backend ,test-backend
                  :model test-anon-parent-model
                  :temperature 1.0))
      :model 'test-child-model-anon)
    (gptel--apply-preset 'test-child-preset-anon)
    (should (eq gptel--preset 'test-child-preset-anon))
    (should (eq gptel-backend test-backend))
    (should (eq gptel-model 'test-child-model-anon))
    (should (equal gptel-temperature 1.0))))

(ert-deftest gptel-test-with-preset ()
  "Test `gptel-with-preset'."
  (let ((gptel-backend (alist-get 'openai gptel-test-backends))
        (gptel-model 'base-model)
        (gptel--preset 'base-preset)
        (preset-backend (alist-get 'anthropic gptel-test-backends)))
    (gptel-make-preset 'test-with-preset
      :backend preset-backend
      :model 'preset-model)
    (gptel-with-preset 'test-with-preset
      (should (eq gptel-backend preset-backend))
      (should (eq gptel-model 'preset-model))
      (should (eq gptel--preset 'test-with-preset)))
    ;; Check values are restored
    (should (eq gptel-backend (alist-get 'openai gptel-test-backends)))
    (should (eq gptel-model 'base-model))
    (should (eq gptel--preset 'base-preset))))

(ert-deftest gptel-test-save-preset ()
  "Test `gptel--save-preset'."
  ;; Prevent prompt from read-string/completing-read
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Test save"))
            ((symbol-function 'completing-read) (lambda (&rest _) "test-saved-preset")))
    (let ((gptel-backend (alist-get 'openai gptel-test-backends))
          (gptel-model 'save-model)
          (gptel--system-message "save system")
          (gptel-tools nil)
          (gptel-stream nil)
          (gptel-temperature 1.0)
          (gptel-max-tokens 100)
          (gptel-use-context nil)
          (gptel-track-media nil)
          (gptel-include-reasoning t)
          (kill-ring nil))
      (gptel--save-preset 'test-saved-preset "Test save")
      ;; It should create the preset
      (let ((preset-spec (gptel-get-preset 'test-saved-preset)))
        (should preset-spec)
        (should (equal (plist-get preset-spec :description) "Test save"))
        (should (equal (plist-get preset-spec :model) 'save-model))
        (should (equal (plist-get preset-spec :temperature) 1.0)))
      ;; It should save Lisp expression to kill-ring
      (should (> (length kill-ring) 0))
      (should (string-match-p "gptel-make-preset 'test-saved-preset" (car kill-ring))))))

(provide 'gptel-unit-tests)
;;; gptel-unit-tests.el ends here
