;;; flymake-ruff --- Flymake backend for Ruff. -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Dan Sheikh

;; Author: Dan Sheikh <dan.sheikh@yahoo.com>
;; Version: 0.0.1
;; Keywords: flymake, ruff

;;; Commentary:

;; A flymake backend for Ruff.

;;; Code:

(require 'cl-lib)

(defvar-local flymake-proc--ruff nil)

(defun flymake-ruff (report-fn &rest _args)
  "Configure ruff as a flymake backend using REPORT-FN."

  (unless (executable-find "ruff")
    (error "Cannot find ruff binary!"))

  (flymake-log :debug "Starting flymake ruff backend.")

  (when (process-live-p flymake-proc--ruff)
    (kill-process flymake-proc--ruff))

  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq flymake-proc--ruff
            (make-process
             :name "flymake-ruff"
             :noquery t
             :connection-type 'pipe
             :buffer (generate-new-buffer "*flymake-ruff*")
             :command (list "ruff" "check" "--quiet" "--output-format" "concise")
             :sentinel (lambda (proc _event)
                         (when (memq (process-status proc) '(exit signal))
                           (unwind-protect
                               (if (with-current-buffer source (eq proc flymake-proc--ruff))
                                   (with-current-buffer (process-buffer proc)
                                     (goto-char (point-min))
                                     (cl-loop while (search-forward-regexp "^\\(?:.*\\):\\([0-9]+\\):\\([0-9]+\\):[[:blank:]\|[:space:]]*\\(\\w\\{1\\}[0-9]+\\)[[:blank:]\|[:space:]]*\\(.*\\)$" nil t)
                                              for line = (string-to-number (match-string 1))
                                              for col = (string-to-number (match-string 2))
                                              for code = (match-string 3)
                                              for msg = (match-string 4)
                                              for (beg . end) = (flymake-diag-region source line col)
                                              for type = (if (string-match "^E.*" code)
                                                             :error
                                                           :warning)
                                              when (and beg end)
                                              collect (flymake-make-diagnostic source
                                                                               beg
                                                                               end
                                                                               type
                                                                               (format "Ruff [%s] %s" code msg))
                                              into diagnostics
                                              finally (funcall report-fn diagnostics)))
                                 (flymake-log :warning "Canceling obsolete check %s." proc))
                             (kill-buffer (process-buffer proc)))))))
      (process-send-region flymake-proc--ruff (point-min) (point-max))
      (process-send-eof flymake-proc--ruff))))

(defun setup-flymake-ruff-backend ()
  "Setup ruff as a flymake backend."
  (add-hook 'flymake-diagnostic-functions #'flymake-ruff nil t))

(provide 'flymake-ruff)
;;; flymake-ruff.el ends here
