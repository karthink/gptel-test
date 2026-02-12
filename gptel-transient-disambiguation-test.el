;;; gptel-transient-disambiguation-test.el --- Tests for gptel transient disambiguation  -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel)
(require 'gptel-transient)

(defvar gptel--known-tools) ; Silence byte-compiler

(ert-deftest gptel-transient-disambiguation-test ()
  "Test tool disambiguation logic in gptel-transient."
  (let ((gptel--known-tools
         '(("filesystem"
            ("list_files" . :tool-obj-1)
            ("read_file" . :tool-obj-2))
           ("memory"
            ("read_memory" . :tool-obj-3)
            ("list_files" . :tool-obj-4)) ; Duplicate name
           ("web"
            ("search" . :tool-obj-5)))))

    ;; Test gptel-transient--providers-for
    (should (equal (sort (gptel-transient--providers-for "list_files") #'string<)
                   '("filesystem" "memory")))
    (should (equal (gptel-transient--providers-for "read_file")
                   '("filesystem")))
    (should (equal (gptel-transient--providers-for "unknown_tool")
                   nil))

    ;; Test exclusion in providers-for
    (should (equal (gptel-transient--providers-for "list_files" "filesystem")
                   '("memory")))

    ;; Test gptel-transient--ambiguous-p
    (should (gptel-transient--ambiguous-p "list_files"))
    (should-not (gptel-transient--ambiguous-p "read_file"))
    (should-not (gptel-transient--ambiguous-p "search"))

    ;; Test gptel-transient--category-conflict-count
    (should (= (gptel-transient--category-conflict-count "filesystem") 1)) ; list_files is ambiguous
    (should (= (gptel-transient--category-conflict-count "memory") 1))     ; list_files is ambiguous
    (should (= (gptel-transient--category-conflict-count "web") 0))))      ; no ambiguous tools

(ert-deftest gptel-transient-disambiguation-ui-config-test ()
  "Test the UI configuration variable."
  (should (boundp 'gptel-transient-disambiguate-tools))
  (should (memq gptel-transient-disambiguate-tools '(on off))))

(provide 'gptel-transient-disambiguation-test)
;;; gptel-transient-disambiguation-test.el ends here
