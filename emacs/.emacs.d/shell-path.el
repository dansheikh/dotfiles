;;; package --- Summary

;;; Code:
(defun set-shell-path ()
  "Use environment variable PATH to configure shell path."
  (with-temp-buffer
    (let* ((shell (getenv "SHELL"))
	   (shell-args (list "-i" "-c" "echo $PATH"))
	   (exit-code (apply #'call-process shell nil t nil shell-args)))
      (unless (zerop exit-code)
	(error "Non-zero exit code from shell %s invoked with args: %S\nOutput: %s" shell shell-args (buffer-substring (point-min) (point-max))))
      (setenv "PATH" (buffer-substring (point-min) (point-max)))
      (setq exec-path (split-string (getenv "PATH") path-separator)))))

(when (display-graphic-p)
    (set-shell-path))

(provide 'shell-path)
;;; shell-path.el ends here
