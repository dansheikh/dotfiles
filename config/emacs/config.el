;;; config.el --- Custom Emacs configurations. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(defun cleanup-eshell-window ()
  "Delete eshell window."
  (when (not (one-window-p))
    (delete-window)))

(defun danish--org-babel-tangle-config ()
  "Automatically tangle emacs.org on save."
  (when (string-equal (buffer-file-name) (expand-file-name "~/dotfiles/emacs/emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(defun danish--style-frame ()
  "Style Emacs frame."
  ;; Disable menu, scroll, & tool bars
  (menu-bar-mode -1)
  (when (display-graphic-p)
    (scroll-bar-mode -1)
    (tool-bar-mode -1))
  ;; Set default font
  (add-to-list 'default-frame-alist '(font . "VictorMono Nerd Font-16"))
  (set-frame-font "VictorMono Nerd Font-16")
  ;; Maximize frame
  (toggle-frame-maximized))

(defun danish--set-exec-path-by-shell ()
  "Set Emacs' `exec path` and `PATH` environment variable to match those in user's shell."
  (interactive)
  (let ((shell-path (if (string-match-p "fish" (getenv "SHELL"))
                        (replace-regexp-in-string
                         "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -c 'string join : $PATH'"))
                      (replace-regexp-in-string
                       "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -c 'echo $PATH'")))))
    (setenv "PATH" shell-path)
    (setq exec-path (split-string shell-path path-separator))))

(defun danish--add-local-node-bin-to-exec-path ()
  "Add `node_modules/.bin` directory to `exec-path`."
  (interactive)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (bin (and root
                   (expand-file-name "node_modules/.bin"
                                     root))))
    (when (and bin (file-directory-p bin))
      (setq-local exec-path (cons bin exec-path)))))

(defun mini-eshell ()
  "Launch eshell in a mini-window."
  (interactive)
  (let ((w (split-window-below -10)))
    (select-window w)
    (eshell)))

(advice-add 'eshell-life-is-too-much :after 'cleanup-eshell-window)

;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)
;;                 (with-selected-frame frame
;;                   (danish--style-frame))))
;;   (danish--style-frame))

(cond ((daemonp)
       (danish--style-frame)
       (add-hook 'after-make-frame-functions
                 (lambda (frame)
                   (with-selected-frame frame
                     (danish--style-frame)))))
      (t (danish--style-frame)))

(provide 'config)
;;; config.el ends here
