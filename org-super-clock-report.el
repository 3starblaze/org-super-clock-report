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
    (when (listp ast)
      (apply fun (list ast))
      (dolist (child (cddr ast))
        (org-super-clock-report--parse-ast child fun))))

(defun org-super-clock-report--display-data-p (data)
  "Verify that DATA is display-data."
  (and (listp data)
       (= (% (length data) 2) 0)
       (cl-do ((this-list data (cddr this-list)))
           ((or (not this-list)
                (not (stringp (cl-first this-list)))
                (not (org-duration-p (cl-second this-list))))
            (not this-list)))))

(defun org-super-clock-report--create-regexp-headline-filter (regexp)
  "Return a headline filter that will match a REGEXP."
  (lambda (ast)
    (and (eq (cl-first ast) 'headline)
         (string-match-p regexp (plist-get (cl-second ast) :raw-value)))))

(defun org-super-clock-report--create-list-headline-filter (headline-list)
  "Create a headline filter that will match a literal string in HEADLINE-LIST."
  (lambda (ast)
    (and (eq (cl-first ast) 'headline)
         (cl-find (plist-get (cl-second ast) :raw-value)
                  headline-list
                  :test 'equal))))

(defun org-super-clock-report--create-from-timestamp-clock-filter (timestamp)
  "Verify that current AST is a clock which has started at TIMESTAMP."
  (lambda (ast)
    (and (eq (cl-first ast) 'clock)
         (let ((expected-timestamp-object
                (cl-second (org-timestamp-from-string timestamp)))
               (actual-timestamp-object
                (cl-second (plist-get (cl-second ast) :value))))
           (cl-reduce
            ;; AND is a macro and can't be passed directly, thus the lambda
            (lambda (a b) (and a b))
            (append
             (cl-mapcar
             (lambda (prop)
                (>=
                 (plist-get actual-timestamp-object prop)
                 (plist-get expected-timestamp-object prop)))
              '(:year-start :month-start :day-start))
             (cl-mapcar
              (lambda (prop)
                (>=
                 (or (plist-get actual-timestamp-object prop) 0)
                 (or (plist-get expected-timestamp-object prop) 0)))
              '(:hour-start :minute-start :second-start))))))))

(defun org-super-clock-report--count-clock-duration (ast &optional clock-filter)
  "Count duration in AST clocks, optionally filtering with CLOCK-FILTER."
  (let ((total-duration 0)
        (filter (or clock-filter
                    (lambda (this-ast) (eq (cl-first this-ast) 'clock)))))
    (org-super-clock-report--parse-ast
     ast
     (lambda (this-ast)
       (let ((tmp-duration (and (funcall filter this-ast)
                                (plist-get (cl-second this-ast) :duration))))
         (when tmp-duration
           (cl-incf total-duration (org-duration-to-minutes tmp-duration))))))
    (org-duration-from-minutes total-duration)))

(defun org-super-clock-report--query (headline-filter &optional clock-filter)
  "Obtain display data given HEADLINE-FILTER and CLOCK-FILTER.

Filter functions are special functions that accept an ast and return a truthy
value if the the ast matches the requirements.

Return display-data - a plist containing headlines as keys and durations as
values - meant for display in a buffer (but is handy for verifying query
results)."
  (unless (eq major-mode 'org-mode)
    (error "Not in org-mode"))
  (let ((ast (org-super-clock-report--get-ast (current-buffer)))
        (target-asts)
        (headline-duration-plist))
    (org-super-clock-report--parse-ast
     ast
     (lambda (this-ast)
       (when (funcall headline-filter this-ast)
         (setf target-asts (append target-asts (list this-ast))))))
    (dolist (this-ast target-asts)
      (setf headline-duration-plist
            (plist-put
             headline-duration-plist
             (plist-get (cl-second this-ast) :raw-value)
             (org-super-clock-report--count-clock-duration
              this-ast clock-filter))))
    headline-duration-plist))

(defun org-super-clock-report--display (display-data-plist)
  "Create clock report from DISPLAY-DATA-PLIST."
  (unless (org-super-clock-report--display-data-p display-data-plist)
    (signal 'wrong-type-argument
            `(org-super-clock-report--display-data-p ,display-data-plist)))
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
   (org-super-clock-report--query
    (org-super-clock-report--create-regexp-headline-filter regexp))))

(defun org-super-clock-report-from-headline-list (headline-list)
  "Display clock-report table for headlines that are in HEADLINE-LIST."
  (org-super-clock-report--display
   (org-super-clock-report--query
    (org-super-clock-report--create-list-headline-filter headline-list))))

(provide 'org-super-clock-report)
;;; org-super-clock-report.el ends here
