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

