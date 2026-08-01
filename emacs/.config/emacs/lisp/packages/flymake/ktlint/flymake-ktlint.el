;;; flymake-ktlint.el --- Flymake backend for ktlint -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Dan Sheikh

;; Author: Dan Sheikh <dan.sheikh@yahoo.com>
;; Keywords: languages, kotlin, lint
;; Package-Requires: ((emacs "29.1") (flymake "1.0"))

;;; Commentary:

;; Flymake backend for ktlint, the Kotlin linter and formatter.
;; Parses ktlint's output format:
;;
;;   <file>:<line>:<col>: <message> (<rule>)
;;
;; Usage:
;;
;;   (add-hook 'kotlin-ts-mode-hook #'flymake-ktlint-init)
;;
;; Requires `ktlint' to be on $PATH.
;; Reference: https://ktlint.github.io/

;;; Code:

(require 'flymake)

(defgroup flymake-ktlint nil
  "Flymake backend for ktlint."
  :group 'flymake
  :prefix "flymake-ktlint-")

(defcustom flymake-ktlint-executable "ktlint"
  "Path to the ktlint executable."
  :type 'string
  :group 'flymake-ktlint)

(defcustom flymake-ktlint-args '("--reporter=plain" "--stdin")
  "Arguments passed to ktlint when linting via stdin.
The filename hint is appended automatically via --stdin-file-path."
  :type '(repeat string)
  :group 'flymake-ktlint)

;; Internal process handle — one per buffer.
(defvar-local flymake-ktlint--process nil
  "Running ktlint process for the current buffer.")

(defun flymake-ktlint--make-diagnostics (source output)
  "Parse ktlint OUTPUT string and return a list of Flymake diagnostics for SOURCE."
  (let ((diagnostics '())
        ;; ktlint plain reporter: <file>:<line>:<col>: <message> (<rule-id>)
        (pattern (rx bol
                     (one-or-more (not (any ":"))) ":" ; filename (ignored)
                     (group (one-or-more digit)) ":"   ; line number
                     (group (one-or-more digit)) ": "  ; column number
                     (group (one-or-more nonl))         ; message (includes rule)
                     eol)))
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (while (re-search-forward pattern nil t)
        (let* ((line   (string-to-number (match-string 1)))
               (col    (string-to-number (match-string 2)))
               (msg    (match-string 3))
               ;; Map to buffer positions; fall back gracefully on out-of-range lines
               (region (flymake-diag-region source line col)))
          (push (flymake-make-diagnostic
                 source
                 (car region)
                 (cdr region)
                 :warning
                 (format "ktlint: %s" msg))
                diagnostics))))
    (nreverse diagnostics)))

(defun flymake-ktlint--backend (report-fn &rest _args)
  "Flymake backend function.  Calls REPORT-FN with ktlint diagnostics."
  (unless (executable-find flymake-ktlint-executable)
    (error "flymake-ktlint: executable '%s' not found on PATH"
           flymake-ktlint-executable))

  ;; Kill any stale process from a previous check.
  (when (process-live-p flymake-ktlint--process)
    (kill-process flymake-ktlint--process))

  (let* ((source  (current-buffer))
         (content (buffer-substring-no-properties (point-min) (point-max)))
         ;; Provide a filename hint so ktlint can apply the correct rule set.
         (fname   (or (buffer-file-name) "stdin.kt"))
         (args    (append flymake-ktlint-args
                          (list (format "--stdin-file-path=%s" fname)))))
    (setq flymake-ktlint--process
          (make-process
           :name     "flymake-ktlint"
           :noquery  t
           :connection-type 'pipe
           :buffer   (generate-new-buffer " *flymake-ktlint*")
           :command  (cons flymake-ktlint-executable args)
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (unwind-protect
                   (if (with-current-buffer source
                         (eq proc flymake-ktlint--process))
                       (with-current-buffer (process-buffer proc)
                         (funcall report-fn
                                  (flymake-ktlint--make-diagnostics
                                   source (buffer-string))))
                     ;; A newer process superseded this one — discard results.
                     (flymake-log :debug "flymake-ktlint: stale process result discarded"))
                 (kill-buffer (process-buffer proc)))))))
    ;; Send buffer contents to ktlint's stdin and close the pipe.
    (process-send-string flymake-ktlint--process content)
    (process-send-eof    flymake-ktlint--process)))

;;;###autoload
(defun flymake-ktlint-init ()
  "Enable the flymake-ktlint backend in the current buffer.
Add to `kotlin-ts-mode-hook'."
  (add-hook 'flymake-diagnostic-functions #'flymake-ktlint--backend nil t))

(provide 'flymake-ktlint)
;;; flymake-ktlint.el ends here