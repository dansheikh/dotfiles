(defun mini-eshell ()
    "Launch eshell in a mini-window."
  (interactive)
  (let ((w (split-window-below -10)))
    (select-window w)
    (eshell)))

(defun cleanup-eshell-window ()
  "Delete eshell window."
  (when (not (one-window-p))
    (delete-window)))

(advice-add 'eshell-life-is-too-much :after 'cleanup-eshell-window)

(defun conditionally-enable-lispy ()
  (when (eq this-command 'eval-expression)
    (lispy-mode 1)))

(provide 'config)

;;; config.el ends here
