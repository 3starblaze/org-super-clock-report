;;; test.el --- description -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'org-super-clock-report (expand-file-name "../org-super-clock-report.el"))

(defvar org-super-clock-report-test-org-file "test.org"
  "Org mode file used for testing org-super-clock-report features.")

(defvar org-super-clock-report-test-org-buffer
  (find-file (expand-file-name
              org-super-clock-report-test-org-file
              (file-name-directory buffer-file-name)))
  "`org-super-clock-report-test-org-file' but in a buffer.")

(defvar org-super-clock-report-test-ast
  (org-super-clock-report--get-ast org-super-clock-report-test-org-file)
  "The AST representation of `org-super-clock-report-test-org-file'.")

(defmacro org-super-clock-report-test--with-org-file-as-current-buffer
    (&rest body)
  "Use `org-super-clock-report-test-org-file' and feed BODY to `with-current-buffer'."
  `(with-current-buffer org-super-clock-report-test-org-buffer
     ,@body))

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

(ert-deftest org-super-clock-report-test-regexp-filter ()
  (org-super-clock-report-test--with-org-file-as-current-buffer
   (should (equal (org-super-clock-report--query
                   (org-super-clock-report--create-regexp-headline-filter
                    "render"))
                  '("Wait for render to finish" "3:17")))))

(ert-deftest org-super-clock-report-test-headline-list-filter ()
  (org-super-clock-report-test--with-org-file-as-current-buffer
   (should (equal (org-super-clock-report--query
                   (org-super-clock-report--create-list-headline-filter
                    '("Make a puzzle")))
                  '("Make a puzzle" "6:16")))))

(ert-deftest org-super-clock-report-test-from-timestamp-clock-filter ()
  (org-super-clock-report-test--with-org-file-as-current-buffer
   (should (equal (org-super-clock-report--query
                   (org-super-clock-report--create-regexp-headline-filter
                    "busy")
                   (org-super-clock-report--create-from-timestamp-clock-filter
                    "[2021-05-10]"))
                  '("Pretend to be busy" "2:29")))))

(provide 'test)
;;; test.el ends here
