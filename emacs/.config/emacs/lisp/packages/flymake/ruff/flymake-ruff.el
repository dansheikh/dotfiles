;;; flymake-ruff.el --- Flymake backend for Ruff -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Dan Sheikh

;; Author: Dan Sheikh <dan.sheikh@yahoo.com>
;; Version: 0.2.0
;; Keywords: flymake, ruff, python, linting
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; A flymake backend for Ruff, an extremely fast Python linter and formatter.
;;
;; Ruff supports linting and formatting for Python files (.py, .pyi).
;; It checks for style issues, errors, and potential bugs in Python code.
;;
;; Features:
;; - Fast, incremental linting using Ruff
;; - Works alongside Eglot/LSP diagnostics (e.g., Pyright, Pylsp)
;; - Proper error handling and cleanup
;; - Customizable Ruff executable path and arguments
;; - Buffer-local process management
;;
;; This backend works alongside Eglot diagnostics, providing additional
;; linting checks from Ruff while LSP provides type checking and semantic
;; analysis.
;;
;; Usage:
;;   (add-hook 'python-ts-mode-hook #'flymake-ruff-init)
;;   (add-hook 'python-mode-hook #'flymake-ruff-init)
;;
;; Customization:
;;   (setq flymake-ruff-program "ruff")  ; Path to ruff executable
;;   (setq flymake-ruff-extra-args '("--select=E,F"))  ; Extra CLI args
;;
;; Requirements:
;;   - Ruff installed and in PATH
;;   - Install via: pip install ruff
;;   - Or via: pipx install ruff

;;; Code:

(require 'cl-lib)
(require 'flymake)

(defgroup flymake-ruff nil
  "Flymake backend for Ruff Python linter."
  :group 'flymake
  :group 'python
  :prefix "flymake-ruff-")

(defcustom flymake-ruff-program "ruff"
  "Path to the Ruff executable.
Can be an absolute path or a program name to search in PATH."
  :type 'string
  :group 'flymake-ruff)

(defcustom flymake-ruff-extra-args nil
  "Extra arguments to pass to Ruff.
Example: '(\"--select=E,F\" \"--ignore=E501\")"
  :type '(repeat string)
  :group 'flymake-ruff)

(defvar-local flymake-ruff--process nil
  "Buffer-local Ruff process for this buffer.")

(defun flymake-ruff--ensure-executable ()
  "Check if Ruff executable is available, return path or nil."
  (executable-find flymake-ruff-program))

(defun flymake-ruff-backend (report-fn &rest _args)
  "Flymake backend for Python using Ruff and REPORT-FN.
Lints Python files and reports diagnostics via REPORT-FN."
  
  ;; Confirm `ruff' executable is available in PATH
  (unless (flymake-ruff--ensure-executable)
    (funcall report-fn
             :panic
             :explanation (format "Ruff executable '%s' not found in PATH. Install via: pip install ruff"
                                  flymake-ruff-program))
    (cl-return-from flymake-ruff-backend))

  ;; Kill any currently executing linting process
  (when (process-live-p flymake-ruff--process)
    (kill-process flymake-ruff--process))

  (let* ((source-buffer (current-buffer))
         (file-ext (or (file-name-extension (buffer-file-name (current-buffer))) "py"))
         (temp-file (make-temp-file "flymake-ruff" nil (concat "." file-ext))))

    ;; Save buffer contents to temp file
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) temp-file nil 'silent))

    ;; Start linting process
    (setq flymake-ruff--process
          (make-process
           :name "flymake-ruff"
           :noquery t
           :connection-type 'pipe
           :buffer (generate-new-buffer " *flymake-ruff*")
           :command `(,flymake-ruff-program
                      "check"
                      "--quiet"
                      "--output-format" "github"
                      ,@flymake-ruff-extra-args
                      ,temp-file)
           :sentinel
           (lambda (proc _event)
             (when (eq 'exit (process-status proc))
               (unwind-protect
                   (if (buffer-live-p source-buffer)
                       (with-current-buffer source-buffer
                         (cond
                          ;; Check if linting is obsolete
                          ((not (eq proc flymake-ruff--process))
                           (flymake-log :warning "Cancelling obsolete Ruff check"))
                          
                          ;; Check if linting failed with error (status > 1)
                          ;; Note: Ruff returns 1 when it finds issues, which is normal
                          ((let ((status (process-exit-status proc)))
                             (and (integerp status) (> status 1)))
                           (funcall report-fn
                                    :panic
                                    :explanation (format "Ruff exited with status %d. Check Ruff configuration."
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
(defun flymake-ruff-init ()
  "Initialize Flymake Ruff backend for the current buffer.
This function should be added to `python-ts-mode-hook' or `python-mode-hook'.

The backend works alongside Eglot diagnostics (e.g., Pyright), so both
Ruff linting and LSP type checking diagnostics will be shown."
  (interactive)
  
  ;; Check if Ruff is available before adding the backend
  (if (flymake-ruff--ensure-executable)
      (progn
        ;; Add Ruff backend to flymake diagnostics
        ;; This works alongside any existing backends (e.g., Eglot)
        (add-hook 'flymake-diagnostic-functions #'flymake-ruff-backend nil t)
        
        ;; Enable flymake-mode if not already enabled
        (unless flymake-mode
          (flymake-mode 1))
        
        ;; Log successful initialization
        (flymake-log :debug "Ruff backend initialized for %s" (buffer-name)))
    
    ;; Warn if Ruff is not available
    (message "Warning: Ruff executable '%s' not found. Install via: pip install ruff"
             flymake-ruff-program)))

;;;###autoload
(defun flymake-ruff-disable ()
  "Disable Flymake Ruff backend for the current buffer."
  (interactive)
  (remove-hook 'flymake-diagnostic-functions #'flymake-ruff-backend t)
  (message "Ruff backend disabled for %s" (buffer-name)))

(provide 'flymake-ruff)
;;; flymake-ruff.el ends here
