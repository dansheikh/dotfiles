;;; flymake-sqlfluff.el --- Flymake backend for SQLFluff -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Dan Sheikh

;; Author: Dan Sheikh <dan.sheikh@yahoo.com>
;; Version: 0.2.0
;; Keywords: flymake, sqlfluff, sql, linting
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; A flymake backend for SQLFluff, a SQL linter and formatter.
;;
;; SQLFluff supports linting and formatting for SQL dialects including:
;; - ANSI SQL (default)
;; - PostgreSQL, MySQL, SQLite
;; - BigQuery, Snowflake, Redshift
;; - Spark SQL, Databricks
;; - And many more (see `flymake-sqlfluff-dialects')
;;
;; Features:
;; - SQL linting with dialect-specific rules
;; - Interactive dialect selection
;; - Works standalone or alongside LSP
;; - Proper error handling and cleanup
;; - Customizable SQLFluff executable and arguments
;; - Project-aware configuration (respects .sqlfluff files)
;;
;; Usage:
;;   (add-hook 'sql-mode-hook #'flymake-sqlfluff-init)
;;
;; Set dialect per-project in .dir-locals.el:
;;   ((sql-mode . ((flymake-sqlfluff-dialect . "postgresql"))))
;;
;; Or set globally:
;;   (setq flymake-sqlfluff-dialect "postgresql")
;;
;; Or select interactively:
;;   M-x flymake-sqlfluff-select-dialect
;;
;; Requirements:
;;   - SQLFluff installed and in PATH
;;   - Install via: pip install sqlfluff
;;   - Or via: pipx install sqlfluff

;;; Code:

(require 'cl-lib)
(require 'flymake)

(defgroup flymake-sqlfluff nil
  "Flymake backend for SQLFluff SQL linter."
  :group 'flymake
  :group 'sql
  :prefix "flymake-sqlfluff-")

(defcustom flymake-sqlfluff-program "sqlfluff"
  "Path to the SQLFluff executable.
Can be an absolute path or a program name to search in PATH."
  :type 'string
  :group 'flymake-sqlfluff)

(defcustom flymake-sqlfluff-extra-args nil
  "Extra arguments to pass to SQLFluff.
Example: '(\"--exclude-rules\" \"L001,L002\")"
  :type '(repeat string)
  :group 'flymake-sqlfluff)

(defconst flymake-sqlfluff-dialects
  '("ansi" "athena" "bigquery" "clickhouse" "databricks" "db2" "duckdb"
    "exasol" "greenplum" "hive" "materialize" "mysql" "oracle" "postgresql"
    "redshift" "snowflake" "soql" "sparksql" "sqlite" "tsql" "teradata" "trino")
  "List of supported SQLFluff dialects.
See https://docs.sqlfluff.com/en/stable/dialects.html for details.")

(defcustom flymake-sqlfluff-dialect "ansi"
  "Default SQL dialect for SQLFluff linting.
Must be one of `flymake-sqlfluff-dialects'.
Can be overridden per-project using .dir-locals.el or .sqlfluff config file."
  :type '(choice (const :tag "ANSI SQL" "ansi")
                 (const :tag "PostgreSQL" "postgresql")
                 (const :tag "MySQL" "mysql")
                 (const :tag "SQLite" "sqlite")
                 (const :tag "BigQuery" "bigquery")
                 (const :tag "Snowflake" "snowflake")
                 (const :tag "Redshift" "redshift")
                 (const :tag "Spark SQL" "sparksql")
                 (const :tag "Databricks" "databricks")
                 (const :tag "T-SQL" "tsql")
                 (const :tag "Oracle" "oracle")
                 (const :tag "Athena" "athena")
                 (const :tag "ClickHouse" "clickhouse")
                 (const :tag "DB2" "db2")
                 (const :tag "DuckDB" "duckdb")
                 (const :tag "Exasol" "exasol")
                 (const :tag "Greenplum" "greenplum")
                 (const :tag "Hive" "hive")
                 (const :tag "Materialize" "materialize")
                 (const :tag "SOQL" "soql")
                 (const :tag "Teradata" "teradata")
                 (const :tag "Trino" "trino")
                 (string :tag "Other dialect"))
  :group 'flymake-sqlfluff
  :safe #'stringp)

(defvar-local flymake-sqlfluff--process nil
  "Buffer-local SQLFluff process for this buffer.")

(defun flymake-sqlfluff--ensure-executable ()
  "Check if SQLFluff executable is available, return path or nil."
  (executable-find flymake-sqlfluff-program))

;;;###autoload
(defun flymake-sqlfluff-select-dialect ()
  "Interactively select SQL dialect for current buffer.
Sets `flymake-sqlfluff-dialect' as a buffer-local variable."
  (interactive)
  (let ((dialect (completing-read "Select SQL dialect: " 
                                  flymake-sqlfluff-dialects 
                                  nil t nil nil 
                                  flymake-sqlfluff-dialect)))
    (setq-local flymake-sqlfluff-dialect dialect)
    (message "SQLFluff dialect set to: %s" dialect)
    ;; Restart flymake to use new dialect
    (when flymake-mode
      (flymake-start))))

(defun flymake-sqlfluff-backend (report-fn &rest _args)
  "Flymake backend for SQL using SQLFluff and REPORT-FN.
Lints SQL files using the configured dialect and reports diagnostics."
  
  ;; Confirm `sqlfluff' executable is available in PATH
  (unless (flymake-sqlfluff--ensure-executable)
    (funcall report-fn
             :panic
             :explanation (format "SQLFluff executable '%s' not found in PATH. Install via: pip install sqlfluff"
                                  flymake-sqlfluff-program))
    (cl-return-from flymake-sqlfluff-backend))

  ;; Validate dialect
  (unless (member flymake-sqlfluff-dialect flymake-sqlfluff-dialects)
    (funcall report-fn
             :panic
             :explanation (format "Invalid SQL dialect '%s'. Use M-x flymake-sqlfluff-select-dialect"
                                  flymake-sqlfluff-dialect))
    (cl-return-from flymake-sqlfluff-backend))

  ;; Kill any currently executing linting process
  (when (process-live-p flymake-sqlfluff--process)
    (kill-process flymake-sqlfluff--process))

  (let* ((source-buffer (current-buffer))
         (file-ext (or (file-name-extension (buffer-file-name (current-buffer))) "sql"))
         (temp-file (make-temp-file "flymake-sqlfluff" nil (concat "." file-ext))))

    ;; Save buffer contents to temp file
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) temp-file nil 'silent))

    ;; Start linting process
    (setq flymake-sqlfluff--process
          (make-process
           :name "flymake-sqlfluff"
           :noquery t
           :connection-type 'pipe
           :buffer (generate-new-buffer " *flymake-sqlfluff*")
           :command `(,flymake-sqlfluff-program
                      "lint"
                      "--dialect" ,flymake-sqlfluff-dialect
                      "--format" "github-annotation-native"
                      ,@flymake-sqlfluff-extra-args
                      ,temp-file)
           :sentinel
           (lambda (proc _event)
             (when (eq 'exit (process-status proc))
               (unwind-protect
                   (if (buffer-live-p source-buffer)
                       (with-current-buffer source-buffer
                         (cond
                          ;; Check if linting is obsolete
                          ((not (eq proc flymake-sqlfluff--process))
                           (flymake-log :warning "Cancelling obsolete SQLFluff check"))
                          
                          ;; Check if linting failed with error (status > 1)
                          ;; Note: SQLFluff returns 1 when it finds issues, which is normal
                          ((let ((status (process-exit-status proc)))
                             (and (integerp status) (> status 1)))
                           (funcall report-fn
                                    :panic
                                    :explanation (format "SQLFluff exited with status %d. Check SQLFluff configuration and dialect."
                                                         (process-exit-status proc))))
                          
                          ;; Parse results
                          (t
                           (with-current-buffer (process-buffer proc)
                             (goto-char (point-min))
                             (let ((diags
                                    (cl-loop
                                     while (search-forward-regexp
                                            "^::\\(error\\|warning\\|notice\\) title=\\([^,]+\\),file=\\([^,]+\\),line=\\([0-9]+\\).*,col=\\([0-9]+\\).*::\\(.*\\)$"
                                            nil t)
                                     for type-str = (match-string 1)
                                     for type = (pcase type-str
                                                  ("error" :error)
                                                  ("warning" :warning)
                                                  ("notice" :note)
                                                  (_ :warning))
                                     for title = (match-string 2)
                                     for line = (string-to-number (match-string 4))
                                     for col = (string-to-number (match-string 5))
                                     for msg = (match-string 6)
                                     for full-msg = (format "[%s] %s" title msg)
                                     for (beg . end) = (flymake-diag-region source-buffer line col)
                                     collect (flymake-make-diagnostic
                                              source-buffer beg end type full-msg))))
                               (funcall report-fn diags))))))
                     ;; Report empty diagnostics if buffer no longer exists
                     (funcall report-fn nil))
                 
                 ;; Cleanup
                 (ignore-errors
                   (kill-buffer (process-buffer proc)))
                 (ignore-errors
                   (delete-file temp-file)))))))))

;;;###autoload
(defun flymake-sqlfluff-init ()
  "Initialize Flymake SQLFluff backend for the current buffer.
This function should be added to `sql-mode-hook'.

The dialect can be configured via:
1. Buffer-local variable (e.g., in .dir-locals.el)
2. Project .sqlfluff configuration file
3. Global `flymake-sqlfluff-dialect' setting
4. Interactively with `flymake-sqlfluff-select-dialect'"
  (interactive)
  
  ;; Check if SQLFluff is available before adding the backend
  (if (flymake-sqlfluff--ensure-executable)
      (progn
        ;; Add SQLFluff backend to flymake diagnostics
        (add-hook 'flymake-diagnostic-functions #'flymake-sqlfluff-backend nil t)
        
        ;; Enable flymake-mode if not already enabled
        (unless flymake-mode
          (flymake-mode 1))
        
        ;; Log successful initialization
        (flymake-log :debug "SQLFluff backend initialized for %s (dialect: %s)" 
                     (buffer-name) flymake-sqlfluff-dialect))
    
    ;; Warn if SQLFluff is not available
    (message "Warning: SQLFluff executable '%s' not found. Install via: pip install sqlfluff"
             flymake-sqlfluff-program)))

;;;###autoload
(defun flymake-sqlfluff-disable ()
  "Disable Flymake SQLFluff backend for the current buffer."
  (interactive)
  (remove-hook 'flymake-diagnostic-functions #'flymake-sqlfluff-backend t)
  (message "SQLFluff backend disabled for %s" (buffer-name)))

(provide 'flymake-sqlfluff)
;;; flymake-sqlfluff.el ends here
