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

(defvar org-super-clock-report-buffer-name "*org-super-clock-report*"
  "The name of the buffer where the clock report is shown.")

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
       (let ((tmp-duration (and (eq (cl-first this-ast) 'clock)
                                (plist-get (cl-second this-ast) :duration))))
         (when tmp-duration
           (cl-incf total-duration (org-duration-to-minutes tmp-duration))))))
    (org-duration-from-minutes total-duration)))

(defun org-super-clock-report--query-from-regexp (regexp)
  "Collect clock report information given REGEXP meant for display."
  (let* ((ast (org-super-clock-report--get-ast (current-buffer)))
         (target-asts (org-super-clock-report--regexp-headlines ast regexp))
         (headline-duration-plist))
    (dolist (this-ast target-asts)
      (setf headline-duration-plist
            (plist-put
             headline-duration-plist
             (plist-get (cl-second this-ast) :raw-value)
             (org-super-clock-report--count-clock-duration this-ast))))
    headline-duration-plist))

(defun org-super-clock-report--display (display-data-plist)
  "Create clock report from DISPLAY-DATA-PLIST."
  (unless (eq major-mode 'org-mode)
    (error "Not in org-mode"))
  ;; Kill the bufffer so that we don't have to do the clean-up ourselves
  (when (get-buffer org-super-clock-report-buffer-name)
    (kill-buffer org-super-clock-report-buffer-name))
  (with-current-buffer (get-buffer-create org-super-clock-report-buffer-name)
    (org-mode)
    (insert "| Headline | Duration |\n")
    (insert "|-\n")
    (cl-do ((this-plist display-data-plist (cddr this-plist)))
        ((not this-plist) nil)
      (insert "|" (cl-first this-plist)
              "|" (cl-second this-plist) "|\n"))
    (switch-to-buffer org-super-clock-report-buffer-name)
    ;; Use org's C-c C-c to have a properly aligned table.
    ;; This is done at the second line because "|-" line needs to be C-c C-c
    ;; specifically. It updates the whole table as well.
    (save-excursion
      (goto-char (point-min))
      (forward-line)
      (org-ctrl-c-ctrl-c))
    (read-only-mode)))

(defun org-super-clock-report-from-regexp (regexp)
  "Display clock-report table for headlines which match REGEXP."
  (org-super-clock-report--display
   (org-super-clock-report--query-from-regexp regexp)))

(provide 'org-super-clock-report)
;;; org-super-clock-report.el ends here
