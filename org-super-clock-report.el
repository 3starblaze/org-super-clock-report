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
(require 'dash)
(require 'ht)
(require 'org-element)

;; This shim makes nested ht comparable
(defun ht-equal? (table1 table2)
  "Return t if TABLE1 and TABLE2 have the same keys and values.
Does not compare equality predicates."
  (declare (side-effect-free t))
  (let ((keys1 (ht-keys table1))
        (keys2 (ht-keys table2))
        (sentinel (make-symbol "ht-sentinel")))
    (and (equal (length keys1) (length keys2))
         (--all?
          (if (ht-p (ht-get table1 it))
              (ht-equal-p (ht-get table1 it)
                          (ht-get table2 it))
            (equal (ht-get table1 it)
                 (ht-get table2 it sentinel)))
          keys1))))

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
  (or (eq data nil)
      (and (listp data)
           (= (% (length data) 2) 0)
           (ht-p (ht-from-plist data))
           (cl-reduce (lambda (a b) (and a b))
                      (ht-map (lambda (k v)
                                (and
                                 (stringp k)
                                 ;; To prevent returning 0
                                 (when (org-duration-p v) t)))
                              (ht-from-plist data))))))

(defun org-super-clock-report--grouped-display-data-p (data)
  "Verify that DATA is grouped display-data.

Grouped display-data is similar to normal data but instead is a 2d plist with
key being a headline and value - a plist with a key of a group and value of a
duration."
  (and (listp data)
       (= (% (length data) 2) 0)
       (cl-do ((this-list data (cddr this-list)))
           ((or (not this-list)
                (not (stringp (cl-first this-list)))
                (not (org-super-clock-report--display-data-p
                      (cl-second this-list))))
            (not this-list)))
       (or (eq nil data)
           (cl-do ((this-list (-slice data 1 nil 2) (cdr this-list)))
               ((or (= (length this-list) 1)
                    (not (equal (-slice (cl-first this-list) 0 nil 2)
                                (-slice (cl-second this-list) 0 nil 2))))
                (= (length this-list) 1))))))

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

(defun org-super-clock-report--today-clock-filter (ast)
  "Clock filter that takes all AST that are made today."
  (funcall (org-super-clock-report--create-from-timestamp-clock-filter
            (format-time-string "[%Y-%m-%d]"))
           ast))

(defun org-super-clock-report--this-month-clock-filter (ast)
  "Clock filter that takes all AST that are made this month."
  (funcall (org-super-clock-report--create-from-timestamp-clock-filter
            (format-time-string "[%Y-%m-01]"))
           ast))

(defun org-super-clock-report--daily-clock-grouper (clock-ast)
  "Given clock data CLOCK-AST create a daily group."
  (let ((timestamp-data (cl-second (plist-get (cl-second clock-ast) :value))))
    ;; org-timestamp-format seems to be broken so it's done manually
    (format "%04d-%02d-%02d"
            (plist-get timestamp-data :year-start)
            (plist-get timestamp-data :month-start)
            (plist-get timestamp-data :day-start))))

(defun org-super-clock-report--grouper (ast group-filter)
  "Group AST clock entries with GROUP-FILTER."
  (let ((groups (ht-create)))
    (org-super-clock-report--parse-ast
     ast
     (lambda (this-ast)
       (when (eq (cl-first this-ast) 'clock)
         (let ((group (funcall group-filter this-ast)))
           (ht-set! groups group
                    (push this-ast (ht-get groups group)))))))

    ;; Reverse the plist order
    (ht->plist (ht-from-alist (reverse (ht->alist groups))))))

(defun org-super-clock-report--count-clock-duration (ast-list)
  "Count toget all duration for each clock ast from AST-LIST."
  (org-duration-from-minutes
   (cl-reduce
    #'+
    (cl-mapcar (lambda (ast)
                 (and (eq (cl-first ast) 'clock)
                      (org-duration-to-minutes
                       (plist-get (cl-second ast) :duration))))
               ast-list))))

(defun org-super-clock-report--count-group-clock-duration (clock-data)
  "Given CLOCK-DATA count duration according to group.

CLOCK-DATA is a plist with group as a key and ast list as a value."
  (let ((display-data))
    (dolist (key (-slice clock-data 0 nil 2))
      (dolist (this-ast (plist-get clock-data key))
        (plist-put
         display-data
         key
         (+ (or (plist-get display-data key) 0)
            (plist-get (cl-second this-ast) :duration)))))
    display-data))

(defun org-super-clock-report--filter-ast (ast filter)
  "Recursively traverse AST and return a list to all asts that satisfy FILTER.

FILTER is a predicate that accepts an AST."
  (let ((ast-list))
    (org-super-clock-report--parse-ast
     ast
     (lambda (this-ast)
       (when (funcall filter this-ast)
         (setf ast-list (push this-ast ast-list)))))
    ast-list))

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
              (org-super-clock-report--filter-ast
               this-ast
               (or clock-filter
                   (lambda (this-ast) (eq (cl-first this-ast) 'clock))))))))
    headline-duration-plist))

(defun org-super-clock-report--query-grouped
    (headline-filter _clock-filter group-filter)
  "Query current buffer by group.

HEADLINE-FILTER and CLOCK-FILTER acts just like `org-super-clock-report--query'.
GROUP-FILTER generates a group name for a clock entry which is then used to sort
clock reports by group.

TODO Actually use CLOCK-FILTER."
  (unless (eq major-mode 'org-mode)
    (error "Not in org-mode"))
  (let* ((ast (org-super-clock-report--get-ast (current-buffer)))
         (headline-asts (org-super-clock-report--filter-ast ast headline-filter))
         (result-ht (ht-create)))
    (dolist (this-headline-ast headline-asts)
      (ht-set! result-ht
               (plist-get (cl-second this-headline-ast) :raw-value)
               (let ((group-duration-ht
                      (ht-from-plist
                       (org-super-clock-report--grouper
                        this-headline-ast group-filter))))
                 (ht-map
                  (lambda (group ast-list)
                    (ht-set! group-duration-ht group
                             (org-super-clock-report--count-clock-duration ast-list)))
                  group-duration-ht)
                 group-duration-ht)))
    result-ht))

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
    (ht-map (lambda (k v) (insert "|" k "|" v "|\n"))
     (ht-from-plist display-data-plist))
    (switch-to-buffer org-super-clock-report-buffer-name)
    ;; Use org's C-c C-c to have a properly aligned table.
    ;; This is done at the second line because "|-" line needs to be C-c C-c
    ;; specifically. It updates the whole table as well.
    (save-excursion
      (goto-char (point-min))
      (forward-line)
      (org-ctrl-c-ctrl-c))
    (read-only-mode)))

(defun org-super-clock-report--display-grouped (display-ht)
  "Use DISPLAY-HT to show grouped clock report."
  (when (get-buffer org-super-clock-report-buffer-name)
    (kill-buffer org-super-clock-report-buffer-name))
  (with-current-buffer (get-buffer-create org-super-clock-report-buffer-name)
    (org-mode)
    (insert "Grouped clock-report\n\n")
    (switch-to-buffer org-super-clock-report-buffer-name)
    (ht-map (lambda (headline groups)
              (insert "* " headline "\n")
              (insert "| Group | Duration |\n")
              (insert "|-\n")
              (ht-map (lambda (group duration)
                        (insert "|" group "|" duration "|\n"))
                      groups))
            display-ht)

    ;; Table updating procedure
    ;; TODO If heading is at `point-min' it's ignored and the table is not
    ;; updated. Handle that better.
    (goto-char (point-min))
    (condition-case nil
        (while t
          (org-next-visible-heading 1)
          (forward-line 2)
          (org-ctrl-c-ctrl-c))
      (user-error nil))

    (goto-char (point-min))
    (read-only-mode)
    (switch-to-buffer org-super-clock-report-buffer-name)))

(defvar org-super-clock-report-headline-filters
  (ht (nil (lambda (ast) (eq (cl-first ast) 'headline)))
      ("Regexp" #'org-super-clock-report--create-regexp-headline-filter)
      ("List" #'org-super-clock-report--create-list-headline-filter)))
(defvar org-super-clock-report-clock-filters
  (ht (nil (lambda (ast) (eq (cl-first ast) 'clock)))
      ("Today" #'org-super-clock-report--today-clock-filter)
      ("This month" #'org-super-clock-report--this-month-clock-filter)))
(defvar org-super-clock-report-clock-groupers
  (ht (nil (lambda (_clock-ast) t))
      ("Daily" #'org-super-clock-report--daily-clock-grouper)))

(defvar org-super-clock-report-current-headline-filter
  nil
  "Nil-able string key of a headline filter.

Possible options defined in `org-super-clock-report-headline-filters'.")
(defvar org-super-clock-report-current-clock-filter
  nil
  "Nil-able string key of a clock filter.

Possible options defined in `org-super-clock-report-clock-filters'.")
(defvar org-super-clock-report-current-clock-grouper
  nil
  "Nil-able string key of a clock grouper.

Possible options defined in `org-super-clock-report-clock-groupers'.")

(defun org-super-clock-report-from-regexp (regexp)
  "Display clock-report table for headlines which match REGEXP."
  (interactive "MRegexp: ")
  (if (equal org-super-clock-report-current-clock-grouper nil)
      (org-super-clock-report--display
       (org-super-clock-report--query
        (org-super-clock-report--create-regexp-headline-filter regexp)
        (ht-get org-super-clock-report-clock-filters
              org-super-clock-report-current-clock-filter)))
    (org-super-clock-report--display-grouped
     (org-super-clock-report--query-grouped
      (org-super-clock-report--create-regexp-headline-filter regexp)
      (ht-get org-super-clock-report-clock-filters
              org-super-clock-report-current-clock-filter)
      (ht-get org-super-clock-report-clock-groupers
              org-super-clock-report-current-clock-grouper)))))

(defun org-super-clock-report ()
  "Open buffer for org-super-clock-report creation."
  (interactive)
  (when (get-buffer org-super-clock-report-buffer-name)
    (kill-buffer org-super-clock-report-buffer-name))
  (with-current-buffer (get-buffer-create org-super-clock-report-buffer-name)
    (org-mode)
    (switch-to-buffer org-super-clock-report-buffer-name)
    (let ((definitions
            (ht ("hf" '("Headline filter"
                        org-super-clock-report-headline-filters
                        org-super-clock-report-current-headline-filter))
                ("cf" '("Clock filter"
                        org-super-clock-report-clock-filters
                        org-super-clock-report-current-clock-filter))
                ("cg" '("Clock grouper"
                        org-super-clock-report-clock-groupers
                        org-super-clock-report-current-clock-grouper))))
          (btn-lambdas (ht-create)))

      (ht-map
       (lambda (key val)
         (ht-set! btn-lambdas key
                  (lambda ()
                    (insert-button
                     (cl-first val)
                     'action (lambda (_button)
                               (set
                                (cl-third val)
                                (completing-read
                                 "> "
                                 (ht-keys (symbol-value (cl-second val)))))
                               ;; Refresh buffer
                               (org-super-clock-report))))))
       definitions)

      (cl-flet ((btn (button-id) (funcall (ht-get btn-lambdas button-id))))
        (btn "hf")
        (insert (format " %s\n"
                        org-super-clock-report-current-headline-filter))
        (btn "cf")
        (insert (format " %s\n"
                        org-super-clock-report-current-clock-filter))
        (btn "cg")
        (insert (format " %s\n"
                        org-super-clock-report-current-clock-grouper))))

    (read-only-mode)
    (org-show-all)
    (use-local-map (copy-keymap org-mode-map))

    (local-set-key "q" (lambda () (interactive)
                         (kill-buffer org-super-clock-report-buffer-name)))
    (local-set-key "p" #'previous-line)
    (local-set-key "n" #'next-line)))

(provide 'org-super-clock-report)
;;; org-super-clock-report.el ends here
