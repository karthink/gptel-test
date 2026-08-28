;;; gptel-test-deps.el --- Ensure package dependencies for gptel tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Karthik Chikmagalur

;; Author: Henrik Ahlgren <pablo@iki.fi>
;; Keywords: tests

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; gptel depends on only a few packages, but they may be missing when
;; tests run in a clean environment such as CI.  These helper functions
;; can be invoked from the Makefile to install required dependencies,
;; along with optional packages used by certain tests when available.

;;; Code:

(defconst gptel-test-optional-packages
  '((markdown-mode . "2.7"))
  "Optional packages used by gptel.")

(defun gptel-test-get-requirements (file)
  "Extract package requirements from FILE's Package-Requires header.
Return a list of cons cells of the form (PACKAGE . VERSION), where
PACKAGE is a symbol and VERSION is a string or nil."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward "^;;?[ \t]*Package-Requires:[ \t]*\\(.*\\)$" nil t)
          (let* ((raw (match-string-no-properties 1))
                 (sexp (ignore-errors (car (read-from-string raw)))))
            (when (listp sexp)
              (delq
               nil
               (mapcar
                (lambda (entry)
                  (when (and (listp entry) (symbolp (car entry)))
                    (cons (car entry)
                          (let ((version (cadr entry)))
                            (cond
                             ((stringp version) version)
                             ((null version) nil)
                             (t (format "%s" version)))))))
                sexp))))
        nil))))

(defun gptel-test-install-deps (&optional optional)
  "Install packages required by gptel.el.

If OPTIONAL is non-nil, install also optional packages."
  (setq package-install-upgrade-built-in t)
  (dolist  (requirement (append
                         (gptel-test-get-requirements "../gptel.el")
                         (if optional gptel-test-optional-packages)))
    (let ((pkg (car requirement))
          (version (version-to-list (cdr requirement))))
      (unless (package-installed-p pkg version)
        (if (eq pkg 'emacs)
            (error "Emacs too old, %s required." version))
        (message "Installing %s..." pkg)
        (package-install pkg t)))))

(defun gptel-test-install-deps-optional ()
  "Install packages required by gptel.el, including optional."
  (gptel-test-install-deps t))

(provide 'gptel-test-deps)

;;; gptel-test-deps.el ends here
