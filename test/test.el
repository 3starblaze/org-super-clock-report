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

(ert-deftest org-super-clock-report-test-display-data-p ()
  (cl-macrolet ((should-be
                 (data expected)
                 `(should (equal
                           (org-super-clock-report--display-data-p ,data) ,expected)))
                (should-t (data) `(should-be ,data t))
                (should-nil (data) `(should-be ,data nil)))
    (should-nil 3)
    (should-t '())
    (should-nil '("foo"))
    (should-nil '("foo" "bar"))
    (should-t '("foo" "4:20"))
    (should-nil '("foo" "4:20" "bar"))
    (should-nil '("foo" "4:20" "bar" "baz"))
    (should-t '("foo" "4:20" "boo" "1d 3:20"))))

(ert-deftest org-super-clock-report-test-grouped-display-data-p ()
  (cl-macrolet ((should-be
                 (data expected)
                 `(should (equal
                           (org-super-clock-report--grouped-display-data-p ,data)
                           ,expected)))
                (should-t (data) `(should-be ,data t))
                (should-nil (data) `(should-be ,data nil)))
    (should-t nil)
    (should-nil '("foo"))
    (should-nil '("foo" "bar"))
    (should-nil '("foo" "4:20"))
    (should-nil '("foo" "4:20" "bar"))
    (should-nil '("foo" "4:20" "bar" "baz"))
    (should-nil '("foo" "4:20" "boo" "1d 3:20"))
    (should-t '("a headline" ("a group" "3:33"
                              "b group" "13:32")
                "b headline" ("a group" "7:32"
                              "b group" "23:59")))
    ;; Group should be matrix-like
    (should-nil '("headline a" ("a group" "0:20"
                                "b group" "4:30")
                  "headline b" ("b group" "2:22")))
    ;; Order matters
    (should-nil '("a headline" ("a group" "21:20"
                                "b group" "19:20")
                  "b headline" ("b group" "3:22"
                                "a group" "1:59")))
    ;; Content matters
    (should-nil '("a headline" ("a group" "0:39"
                                "b group" "13:00")
                  "b headline" ("a group" "3:22"
                                "c group" "1:33")))
    ;; Having just one group is fine
    (should-t '("good headline" ("one group" "15:50")
                "better headline" ("one group" "16:50")
                "best headline" ("one group" "17:50")))

    (should-t '("good headline" ("one group" "15:50"
                                 "two group" "15:51"
                                 "three group" "15:52")
                "better headline" ("one group" "16:50"
                                   "two group" "16:51"
                                   "three group" "16:52")
                "best headline" ("one group" "17:50"
                                 "two group" "17:51"
                                 "three group" "17:52")))))


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

(ert-deftest org-super-clock-report-test-filter-ast ()
  (should (eq (length (org-super-clock-report--filter-ast
                       (org-super-clock-report--get-ast org-super-clock-report-test-org-buffer)
                       (org-super-clock-report--create-regexp-headline-filter
                        "busy")))
              1)))

(ert-deftest org-super-clock-report-test-daily-clock-grouper ()
  (let ((ast-ht
         (ht-from-plist
          (cl-first
           (cl-mapcar
            (lambda (ast)
              (org-super-clock-report--grouper
               ast
               #'org-super-clock-report--daily-clock-grouper))
            (org-super-clock-report--filter-ast
             (org-super-clock-report--get-ast org-super-clock-report-test-org-buffer)
             (org-super-clock-report--create-list-headline-filter
              '("Wait for render to finish"))))))))

    (should (eq (length (ht-values ast-ht)) 6))
    (should (equal (mapcar #'length (ht-values ast-ht)) '(4 3 2 2 1 2)))))

(provide 'test)
;;; test.el ends here
