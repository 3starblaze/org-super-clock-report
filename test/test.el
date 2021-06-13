;;; test.el --- description -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'org-super-clock-report (expand-file-name "../org-super-clock-report.el"))

(defvar org-super-clock-report-test-org-file "test.org"
  "Org mode file used for testing org-super-clock-report features.")

(defvar org-super-clock-report-test-ast
  (org-super-clock-report--get-ast org-super-clock-report-test-org-file)
  "The AST representation of `org-super-clock-report-test-org-file'.")

(ert-deftest org-super-clock-report-test-get-ast ()
  ;; Full-file org-file AST start with org-data
  (should (eq (cl-first org-super-clock-report-test-ast) 'org-data)))

(ert-deftest org-super-clock-report-test-parse-ast ()
  "Test parse-ast by checking if the file contains 7 headlines."
  (let ((headline-counter 0))
    (org-super-clock-report--parse-ast
     org-super-clock-report-test-ast
     (lambda (this-ast)
       (when (eq (cl-first this-ast) 'headline)
         (cl-incf headline-counter))))
    (should (eq headline-counter 7))))

(ert-deftest org-super-clock-report-test-regexp-headlines ()
  (let ((asts (org-super-clock-report--regexp-headlines
               org-super-clock-report-test-ast
               "render")))
    (should (eq (length asts) 1))
    (should (equal (plist-get (cl-second (cl-first asts)) :raw-value)
                   "Wait for render to finish"))))

(ert-deftest org-super-clock-report-test-count-clock-duration ()
  (let ((headline-ast (cl-first (org-super-clock-report--regexp-headlines
                                 org-super-clock-report-test-ast
                                 "render"))))
    (should (equal (org-super-clock-report--count-clock-duration headline-ast)
                   "3:17"))))

(ert-deftest org-super-clock-report-query-from-regexp ()
  (with-current-buffer (find-file org-super-clock-report-test-org-file)
    (should (equal (org-super-clock-report--query-from-regexp "render")
                   '("Wait for render to finish" "3:17")))))

(provide 'test)
;;; test.el ends here
