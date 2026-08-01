;;; flymake-biome.el --- Flymake backend for Biome -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Dan Sheikh

;; Author: Dan Sheikh <dan.sheikh@yahoo.com>
;; Version: 0.1.0
;; Keywords: flymake, biome, javascript, typescript, json, css, graphql
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; A flymake backend for Biome linter and formatter.
;;
;; Biome supports linting and formatting for:
;; - JavaScript (.js, .mjs, .cjs)
;; - TypeScript (.ts)
;; - JSX (.jsx)
;; - TSX (.tsx)
;; - JSON (.json)
;; - CSS (.css)
;; - GraphQL (.graphql, .gql)
;;
;; This backend works alongside Eglot's LSP diagnostics, providing
;; additional linting and style checks from Biome.
;;
;; Usage:
;;   (add-hook 'js-ts-mode-hook #'flymake-biome-init)
;;   (add-hook 'typescript-ts-mode-hook #'flymake-biome-init)
;;   (add-hook 'tsx-ts-mode-hook #'flymake-biome-init)
;;   (add-hook 'json-ts-mode-hook #'flymake-biome-init)
;;   (add-hook 'css-ts-mode-hook #'flymake-biome-init)
;;   (add-hook 'graphql-mode-hook #'flymake-biome-init)

;;; Code:

(require 'cl-lib)
(require 'flymake)

(defvar-local flymake-biome--process nil
  "Buffer-local Biome process for this buffer.")

(defcustom flymake-biome-program "biome"
  "Path to the Biome executable."
  :type 'string
  :group 'flymake-biome)

(defcustom flymake-biome-extra-args nil
  "Extra arguments to pass to Biome."
  :type '(repeat string)
  :group 'flymake-biome)

(defun flymake-biome--ensure-executable ()
  "Check if Biome executable is available, return path or nil."
  (executable-find flymake-biome-program))

(defun flymake-biome-backend (report-fn &rest _args)
  "Flymake backend for Biome using REPORT-FN.
Lints JavaScript, TypeScript, JSX, TSX, JSON, CSS, and GraphQL files."
  
  ;; Confirm `biome' executable is available in PATH
  (unless (flymake-biome--ensure-executable)
    (funcall report-fn
             :panic
             :explanation (format "Biome executable '%s' not found in PATH. Install via: npm install -g @biomejs/biome"
                                  flymake-biome-program))
    (cl-return-from flymake-biome-backend))

  ;; Kill any currently executing linting process
  (when (process-live-p flymake-biome--process)
    (kill-process flymake-biome--process))

  (let* ((source-buffer (current-buffer))
         (file-ext (file-name-extension (or (buffer-file-name) "")))
         (temp-file (make-temp-file "flymake-biome" nil (concat "." file-ext))))

    ;; Save buffer contents to temp file
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) temp-file nil 'silent))

    ;; Start linting process
    (setq flymake-biome--process
          (make-process
           :name "flymake-biome"
           :noquery t
           :connection-type 'pipe
           :buffer (generate-new-buffer " *flymake-biome*")
           :command `(,flymake-biome-program
                      "lint"
                      "--files-ignore-unknown=true"
                      "--reporter=github"
                      ,@flymake-biome-extra-args
                      ,temp-file)
           :sentinel
           (lambda (proc _event)
             (when (eq 'exit (process-status proc))
               (unwind-protect
                   (if (buffer-live-p source-buffer)
                       (with-current-buffer source-buffer
                         (cond
                          ;; Check if linting is obsolete
                          ((not (eq proc flymake-biome--process))
                           (flymake-log :warning "Cancelling obsolete Biome check"))
                          
                          ;; Check if linting failed with error (status > 1)
                          ;; Note: Biome returns 1 when it finds issues, which is normal
                          ((let ((status (process-exit-status proc)))
                             (and (integerp status) (> status 1)))
                           (funcall report-fn
                                    :panic
                                    :explanation (format "Biome exited with status %d. Check Biome configuration."
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
                                                  (_ :note))
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

(defun flymake-biome-init ()
  "Initialize Flymake Biome backend for the current buffer.
This function should be added to mode hooks for supported file types.

The backend works alongside Eglot diagnostics, so both Biome and
LSP diagnostics will be shown. To disable LSP diagnostics in favor
of Biome only, remove `eglot-flymake-backend' from
`flymake-diagnostic-functions'."
  (interactive)
  
  ;; Check if Biome is available before adding the backend
  (if (flymake-biome--ensure-executable)
      (progn
        ;; Add Biome backend to flymake diagnostics
        ;; This works alongside any existing backends (e.g., Eglot)
        (add-hook 'flymake-diagnostic-functions #'flymake-biome-backend nil t)
        
        ;; Enable flymake-mode if not already enabled
        (unless flymake-mode
          (flymake-mode 1))
        
        ;; Log successful initialization
        (flymake-log :debug "Biome backend initialized for %s" (buffer-name)))
    
    ;; Warn if Biome is not available
    (message "Warning: Biome executable '%s' not found. Flymake-biome not initialized."
             flymake-biome-program)))

(defun flymake-biome-disable ()
  "Disable Flymake Biome backend for the current buffer."
  (interactive)
  (remove-hook 'flymake-diagnostic-functions #'flymake-biome-backend t)
  (message "Biome backend disabled for %s" (buffer-name)))

(provide 'flymake-biome)
;;; flymake-biome.el ends here
