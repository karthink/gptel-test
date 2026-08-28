;;; gptel-tool-dedupe-test.el --- Tests for gptel tool deduplication  -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-request)
(require 'gptel)

;; Ensure struct definition is available
(require 'cl-lib)

(defun gptel-test--make-dummy-tool (name category)
  "Helper to create a dummy tool."
  (gptel--make-tool-internal
   :name name
   :function (lambda () t)
   :category category))

(ert-deftest gptel-test-find-duplicate-tool-names ()
  "Test detection of duplicate tool names."
  (let* ((tool1 (gptel-test--make-dummy-tool "list_files" "filesystem"))
         (tool2 (gptel-test--make-dummy-tool "list_files" "memory"))
         (tool3 (gptel-test--make-dummy-tool "read_file" "filesystem"))
         (duplicates (gptel--find-duplicate-tool-names (list tool1 tool2 tool3))))
    (should (assoc "list_files" duplicates))
    (let ((cats (cdr (assoc "list_files" duplicates))))
      (should (member "filesystem" cats))
      (should (member "memory" cats)))
    (should-not (assoc "read_file" duplicates))))

(ert-deftest gptel-test-disambiguate-tool-name ()
  "Test renaming of duplicate tools."
  (let* ((tool1 (gptel-test--make-dummy-tool "list_files" "filesystem"))
         (tool3 (gptel-test--make-dummy-tool "read_file" "filesystem"))
         (duplicates '(("list_files" "filesystem" "memory"))))

    (should (equal (gptel--disambiguate-tool-name tool1 duplicates)
                   "filesystem_list_files"))
    (should (equal (gptel--disambiguate-tool-name tool3 duplicates)
                   "read_file"))))

(ert-deftest gptel-test-maybe-disambiguate-tools ()
  "Test the orchestration of disambiguation."
  (let* ((tool1 (gptel-test--make-dummy-tool "list_files" "filesystem"))
         (tool2 (gptel-test--make-dummy-tool "list_files" "memory"))
         (tool3 (gptel-test--make-dummy-tool "read_file" "filesystem"))
         (input-tools (list tool1 tool2 tool3))
         (gptel-request-disambiguate-tool t))

    (let ((result (gptel--maybe-disambiguate-tools input-tools)))
      (should (= (length result) 3))
      ;; Check names in result
      (let ((names (mapcar #'gptel-tool-name result)))
        (should (member "filesystem_list_files" names))
        (should (member "memory_list_files" names))
        (should (member "read_file" names))))

    ;; Test with feature disabled
    (let* ((gptel-request-disambiguate-tool nil)
           (result (gptel--maybe-disambiguate-tools input-tools)))
      (should (equal (mapcar #'gptel-tool-name result)
                     '("list_files" "list_files" "read_file"))))))

(ert-deftest gptel-test-resolve-tool-name ()
  "Test resolving tool names from strings (original and disambiguated)."
  (let* ((tool1 (gptel-test--make-dummy-tool "list_files" "filesystem"))
         (tool2 (gptel-test--make-dummy-tool "list_files" "memory"))
         (tool3 (gptel-test--make-dummy-tool "read_file" "filesystem"))
         (tool4 (gptel-test--make-dummy-tool "search_stuff" "web_search")) ; Underscores in category
         (gptel--known-tools
          (list
           (cons "filesystem" (list (cons "list_files" tool1) (cons "read_file" tool3)))
           (cons "memory" (list (cons "list_files" tool2)))
           (cons "web_search" (list (cons "search_stuff" tool4))))))

    ;; Direct lookup (first match)
    (should (gptel--resolve-tool-name "list_files"))

    ;; Disambiguated lookup
    (should (equal (gptel--resolve-tool-name "filesystem_list_files") tool1))
    (should (equal (gptel--resolve-tool-name "memory_list_files") tool2))

    ;; Disambiguated lookup with underscores in category/name
    (should (equal (gptel--resolve-tool-name "web_search_search_stuff") tool4))

    ;; List lookup
    (should (equal (gptel--resolve-tool-name '("memory" "list_files")) tool2))

    ;; Non-existent
    (should-not (gptel--resolve-tool-name "unknown_tool"))))

(ert-deftest gptel-test-validate-and-prepare-tools ()
  "Test high-level validation and preparation."
  (let* ((tool1 (gptel-test--make-dummy-tool "list_files" "filesystem"))
         (tool2 (gptel-test--make-dummy-tool "list_files" "memory"))
         (gptel--known-tools
          (list
           (cons "filesystem" (list (cons "list_files" tool1)))
           (cons "memory" (list (cons "list_files" tool2)))))
         (gptel-request-disambiguate-tool t))

    ;; Input: List of strings (disambiguated format)
    (let ((result (gptel--validate-and-prepare-tools
                   '("filesystem_list_files" "memory_list_files"))))
      (should (= (length result) 2))
      (should (equal (gptel-tool-name (car result)) "filesystem_list_files"))
      (should (equal (gptel-tool-name (cadr result)) "memory_list_files")))

    ;; Input: List of structs
    (let ((result (gptel--validate-and-prepare-tools
                   (list tool1 tool2))))
      (should (= (length result) 2))
      ;; Note: tool1 gets renamed because it's ambiguous with the resolved tool2
      (should (equal (gptel-tool-name (car result)) "filesystem_list_files"))
      (should (equal (gptel-tool-name (cadr result)) "memory_list_files")))))

(provide 'gptel-tool-dedupe-test)
