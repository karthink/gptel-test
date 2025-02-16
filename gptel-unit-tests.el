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


(provide 'gptel-unit-tests)
;;; gptel-unit-tests.el ends here
