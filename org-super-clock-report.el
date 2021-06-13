;;; org-super-clock-report.el --- Supercharged clock reports -*- lexical-binding: t; -*-
;;
;; Version: 0.0.1
;; Keywords:
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; org-super-clock-report provides a bunch of features to make clock reports
;; more convenient.
;;
;;; Code:

(require 'cl-lib)
(require 'org-element)

(defun org-super-clock-report--get-ast (buffer-or-name)
  "Get AST of a buffer or its name BUFFER-OR-NAME."
  (with-current-buffer buffer-or-name
    (org-element-parse-buffer)))

(defun org-super-clock-report--parse-ast (ast fun)
  "Apply FUN for every element in AST."
    (when ast
      (apply fun (list ast))
      (dolist (child (cddr ast))
        (org-super-clock-report--parse-ast child fun))))

(defun org-super-clock-report--regexp-headlines (ast regexp)
  "Find headlines in AST that match REGEXP."
  (let ((headline-asts))
    (org-super-clock-report--parse-ast
     ast
     (lambda (this-ast)
       (when (and (eq (cl-first this-ast) 'headline)
                  (string-match-p regexp (plist-get (cl-second this-ast) :raw-value)))
         (setf headline-asts (append headline-asts (list this-ast))))))
    headline-asts))

(defun org-super-clock-report--count-clock-duration (ast)
  "For every clock in AST count minutes."
  (let ((total-duration 0))
    (org-super-clock-report--parse-ast
     ast
     (lambda (this-ast)
       (let ((tmp-duration (and (eq (cl-first this-ast) 'headline)
                                (plist-get (cl-second this-ast) :duration))))
         (when tmp-duration
           (cl-incf total-duration (org-duration-to-minutes tmp-duration))))))
    total-duration))

(defun org-super-clock-report-from-regexp (regexp)
  "Create clock report from REGEXP."
  (let* ((ast (org-super-clock-report--get-ast (current-buffer)))
         (target-asts (org-super-clock-report--regexp-headlines ast regexp))
         (headline-duration-plist))
    (dolist (this-ast target-asts)
      (plist-put
       headline-duration-plist
       (plist-get (cl-second this-ast) :raw-value)
       (org-super-clock-report--count-clock-duration this-ast)))
    (with-output-to-temp-buffer "*org-super-clock-report*"
      (org-mode)
      (print "| Headline | Duration |\n")
      (cl-do ((this-plist headline-duration-plist (cddr this-plist)))
          ((not this-plist) nil)
        (print (concat "|" (cl-first this-plist) "|" (cl-second this-plist) "|\n"))))))

(provide 'org-super-clock-report)
;;; org-super-clock-report.el ends here
