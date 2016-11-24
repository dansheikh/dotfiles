(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Load required packages list
;; (load "~/.emacs.d/required-packages.el")

;; Configure load path
;; (add-to-list 'load-path "~/.emacs.d/")

;; Start server
(server-start)

;; Set frame size
(setq default-frame-alist '((width . 150) (height . 30)))

;; Set coding preference
(set-default-coding-systems 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; Set default font
(set-face-attribute 'default nil :font "Inconsolata-11")

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Disable backup and auto-save
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Auto-reload buffers on disk file changes
(global-auto-revert-mode t)

;; Disable menu, scroll, & tool bars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Vertical window split default
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Enable windmove with default bindings
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

;; Set root directory
(setq root-dir (file-name-directory
		(or (buffer-file-name) load-file-name)))

;; Set theme
(require 'color-theme-sanityinc-tomorrow)
(color-theme-sanityinc-tomorrow--define-theme eighties)

;; Enable line numbers
(global-linum-mode t)

;; Set tab (space) width
(setq tab-width 4
      indent-tabs-mode nil)

(show-paren-mode 1)

;; Auto-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Set style
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "bsd")))

(setq c-basic-offset 4)

;; Customize keybindings
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-c C-m") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Load Tuareg
(load "~/.opam/4.03.0/share/emacs/site-lisp/tuareg-site-file")

;; Add opam emacs directory to the load-path
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

;; Load merlin-mode
(require 'merlin)

;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)

;; Enable Ido mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virutal-buffers t)

;; Enable magit
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

;; Enable flycheck and override defaults
(add-hook 'after-init-hook 'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save idle-change mode-enable)
      flycheck-idle-change-delay 1.0)
(provide 'init-flycheck)

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
  (flycheck-pos-tip-mode))

;; Enable global company mode
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-inf-ruby 'company-anaconda))

;; Enable python
(require 'python)
(setq python-shell-interpreter "ipython")
(add-hook 'python-mode-hook 'anaconda-mode)

;; Enable go
(require 'go-mode)

;; Enable Scala
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-mode)

;; Configure Clojure & ClojureScript
(require 'clojure-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojurescript-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojurescript-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; Enable F#
(require 'fsharp-mode)

;; Enable web development support
;; (require 'php-mode)
(require 'web-mode)
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck-color-mode-line flycheck-pos-tip color-theme-sanityinc-tomorrow groovy-mode slime zenburn-theme web-mode sass-mode rainbow-delimiters python-mode paredit magit helm go-mode fsharp-mode flycheck ensime company-anaconda cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
