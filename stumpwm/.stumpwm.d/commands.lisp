;;; commands.lisp --- StumpWM custom commands

;; Author: Dan Sheikh

;;; Commentary:

;; Custom StumpWM commands.

;;; Code:

(defcommand delete-window-and-frame () ()
  "Delete the current window and enclosing frame."
  (delete-window)
  (remove-split))

(defcommand firefox () ()
  "Run or raise Firefox."
  (run-or-raise "firefox" '(:class "Firefox ") t nil))

;;; commands.lisp ends here
