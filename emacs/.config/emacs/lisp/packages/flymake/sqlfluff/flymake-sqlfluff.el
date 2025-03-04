;;; flymake-sqlfluff --- Flymake backend for SQLFluff. -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Dan Sheikh

;; Author: Dan Sheikh <dan.sheikh@yahoo.com>
;; Version: 0.0.1
;; Keywords: flymake, sqlfluff

;;; Commentary:

;; A flymake backend for SQLFluff.

;;; Code:

(require 'cl-lib)

(defgroup flymake-sqlfluff nil
  "Customizable SQLFluff configuration."
  :group 'programming)

(defvar flymake-sqlfluff--dialects '("ansi" "athena" "bigquery" "clickhouse" "databricks" "db2" "duckdb" "exasol" "greenplum" "hive" "materialize" "mysql" "oracle" "postgresql" "redshift" "snowflake" "soql" "sparksql" "sqlite" "t-sql" "teradata" "trino")
  "List of supported SQLFluff dialects.")

(defcustom flymake-sqlfluff--dialect "ansi"
  "Flymake SQLFluff dialect."
  :group 'flymake-sqlfluff
  :type 'list
  :local t
  :options flymake-sqlfluff--dialects)

;;;###autoload
(defun flymake-sqlfluff--select-dialect ()
  "Select SQLFluff dialect."
  (interactive)
  (customize-set-variable 'flymake-sqlfluff--dialect (completing-read "Select dialect: " flymake-sqlfluff--dialects)))

(defvar-local flymake-proc--sqlfluff nil)

(defun flymake-sqlfluff (report-fn &rest _args)
  "Configure sqlfluff as a flymake backend using REPORT-FN."

  (unless (executable-find "sqlfluff")
    (error "Cannot find sqlfluff binary!"))

  (flymake-log :debug "Starting flymake sqlfluff backend.")

  (when (process-live-p flymake-proc--sqlfluff)
    (kill-process flymake-proc--sqlfluff))

  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq flymake-proc--sqlfluff
            (make-process
             :name "flymake-sqlfluff"
             :noquery t
             :connection-type 'pipe
             :buffer (generate-new-buffer "*flymake-sqlfluff*")
             :command (list "sqlfluff" "lint" "--dialect" (shell-quote-argument flymake-sqlfluff--dialect) "--format" "github-annotation-native")
             :sentinel (lambda (proc _event)
                         (when (memq (process-status proc) '(exit signal))
                           (unwind-protect
                               (if (with-current-buffer source (eq proc flymake-proc--sqlfluff))
                                   (with-current-buffer (process-buffer proc)
                                     (goto-char (point-min))
                                     (cl-loop while (search-forward-regexp "^::\\(.*\\)\\s-*title=\\(.*\\),file=\\(.*\\),line=\\([0-9]+\\),col=\\([0-9]+\\),endLine=\\([0-9]+\\),endColumn=\\([0-9]+\\)::\\(.*\\):\\s-*\\(.*\\)$" nil t)
                                              for type = (if (string-match "^error\\|failure" (match-string 1))
                                                             :error
                                                           :warning)
                                              for title = (match-string 2)
                                              for line = (string-to-number (match-string 4))
                                              for col = (string-to-number (match-string 5))
                                              for code = (match-string 8)
                                              for msg = (match-string 9)
                                              for (beg . end) = (flymake-diag-region source line col)
                                              when (and beg end)
                                              collect (flymake-make-diagnostic source
                                                                               beg
                                                                               end
                                                                               type
                                                                               (format "%s [%s] %s" title code msg))
                                              into diagnostics
                                              finally (funcall report-fn diagnostics)))
                                 (flymake-log :warning "Canceling obsolete check %s." proc))
                             (kill-buffer (process-buffer proc))))))))
    (process-send-region flymake-proc--sqlfluff (point-min) (point-max))
    (process-send-eof flymake-proc--sqlfluff)))

(defun setup-flymake-sqlfluff-backend ()
  "Setup sqlfluff as a flymake backend."
  (add-hook 'flymake-diagnostic-functions #'flymake-sqlfluff nil t))

(provide 'flymake-sqlfluff)
;;; flymake-sqlfluff.el ends here
