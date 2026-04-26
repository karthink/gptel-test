;;; gptel-token-tests.el --- Unit tests for gptel token tracking UI -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel)

(ert-deftest gptel-test-token-formatting ()
  "Test gptel--format-token-usage with SI units."
  (let ((tokens '(:input 1000 :output 2000 :cached 500)))
    (should (equal (gptel--format-token-usage tokens) "[1k, C500↑ 2k↓]")))
  (let ((tokens '(:input 1500 :output 2500)))
    (should (equal (gptel--format-token-usage tokens) "[1.5k↑ 2.5k↓]")))
  (let ((tokens '(:input 1024 :output 1024)))
    (should (equal (gptel--format-token-usage tokens) "[1k↑ 1k↓]")))
  (let ((tokens '(:input 999)))
    (should (equal (gptel--format-token-usage tokens) "[999↑]"))))

(ert-deftest gptel-test-update-token-usage ()
  "Test gptel--update-token-usage calculation."
  (let ((gptel--token-usage nil)
        (gptel--token-usage-strings nil)
        (gptel-mode t))
    ;; First request
    (gptel--update-token-usage '(:input 1000 :output 500) '(:input 1000 :output 500))
    (should (equal gptel--token-usage '((:input 1000 :output 500) (:input 1000 :output 500))))

    ;; Second turn (tool use)
    (gptel--update-token-usage '(:input 500 :output 500) '(:input 1500 :output 1000))
    (should (equal gptel--token-usage '((:input 1500 :output 1000) (:input 1500 :output 1000))))

    ;; Subsequent request in same buffer
    (gptel--update-token-usage '(:input 1000 :output 500) '(:input 1000 :output 500))
    (should (equal gptel--token-usage '((:input 1000 :output 500) (:input 2500 :output 1500))))))

(ert-deftest gptel-test-update-token-usage-preserve-on-nil ()
  "Test that buffer total is preserved when a request turn returns nil."
  (let ((gptel--token-usage '((:input 100 :output 50) (:input 500 :output 250)))
        (gptel--token-usage-strings (list 0))
        (gptel-mode t))
    ;; Request turn returns nil usage (e.g. error)
    (gptel--update-token-usage nil nil)
    ;; Request usage and Buffer usage are both PRESERVED (no modification)
    (should (equal (car gptel--token-usage) '(:input 100 :output 50)))
    (should (equal (nth 1 gptel--token-usage) '(:input 500 :output 250)))))

(ert-deftest gptel-test-update-token-usage-nil-does-nothing ()
  "Test that gptel--update-token-usage does nothing when both arguments are nil."
  (let ((gptel--token-usage nil)
        (gptel--token-usage-strings nil)
        (gptel-mode t))
    (gptel--update-token-usage nil nil)
    (should (null gptel--token-usage))
    (should (null gptel--token-usage-strings))))

(provide 'gptel-token-tests)
;;; gptel-token-tests.el ends here
