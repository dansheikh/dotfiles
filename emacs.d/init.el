;;; Code:

(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Set shell path
(defun set-shell-path ()
  (let ((shell-path (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" shell-path)
    (setq exec-path (split-string shell-path path-separator))))

(if (display-graphic-p)
    (set-shell-path))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Configure Multi-term
(use-package multi-term
  :init
  (setq multi-term-program "/bin/zsh"))

;; Set frame size
(setq default-frame-alist '((width . 150) (height . 30)))

;; Set coding preference
(set-default-coding-systems 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; Set default font
(set-face-attribute 'default nil :font "Droid Sans Mono Slashed for Powerline-10")

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
(load-theme 'monokai t)
; (require 'color-theme-sanityinc-tomorrow)
; (color-theme-sanityinc-tomorrow--define-theme eighties)

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

;; Enable global buffer auto-revert.
(global-auto-revert-mode 1)
(setq auto-revert-check-vc-info t)

;; Enable Evil
(use-package evil
  :init
  (evil-mode t)
  :config
  (define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down)
  (define-key evil-visual-state-map (kbd "C-d") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-insert-state-map (kbd "C-u")
    (lambda ()
      (interactive)
      (evil-delete (point-at-bol) (point)))))

;; Enable powerline
(use-package powerline)
(use-package airline-themes
  :init
  (setq powerline-utf-8-separator-left        #xe0b0
        powerline-utf-8-separator-right       #xe0b2
        airline-utf-glyph-separator-left      #xe0b0
        airline-utf-glyph-separator-right     #xe0b2
        airline-utf-glyph-subseparator-left   #xe0b1
        airline-utf-glyph-subseparator-right  #xe0b3
        airline-utf-glyph-branch              #xe0a0
        airline-utf-glyph-readonly            #xe0a2
        airline-utf-glyph-linenumber          #xe0a1)
  :config
  (load-theme 'airline-molokai))

;; Enable flycheck and override defaults
(use-package flycheck
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enable)
        flycheck-idle-change-delay 1.0)
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (provide 'init-flycheck))

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
  (flycheck-pos-tip-mode))

;; Enable yasnippet
(use-package yasnippet
  :init
  (yas-global-mode t))

;; Enable Ido mode
(use-package ido
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t
      ido-use-virutal-buffers t))

;; Enable magit
(use-package magit
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

;; Enable global company mode
(use-package company
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-backends '((company-capf company-files company-elisp company-inf-ruby company-anaconda company-go company-irony company-clang company-cmake company-css company-yasnippet) (company-dabbrev company-dabbrev-code)))
  (add-hook 'after-init-hook 'global-company-mode))

;; Enable python
(use-package python
  :config
  (setq python-shell-interpreter (substring (shell-command-to-string "which ipython") 0 -1)
        python-shell-interpreter-args "--simple-prompt -i")
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

;; Enable irony
(use-package irony
  :config
  (defun custom-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'custom-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode))

;; Enable rust
(use-package rust-mode
  :init
  (setq rust-format-on-save t))

;; Enable go
(use-package go-mode)

;; Configure Clojure & ClojureScript
(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojurescript-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojurescript-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode))

;; Enable web development support
(use-package web-mode)
(use-package sass-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode)))

;; Enable F#
(use-package fsharp-mode)

;; Load Tuareg
(load "~/.opam/4.03.0/share/emacs/site-lisp/tuareg-site-file")

;; Add opam emacs directory to the load-path
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

;; Load OCaml plugins
(use-package ocp-indent)
(use-package merlin
  :config
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'caml-mode-hook 'merlin-mode t))

;; Enable Haskell
(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

;; Enable Perl 6
(use-package perl6-mode
  :defer t)

;; Enable Statistics
(use-package ess
  :defer t)

(provide 'init)

;;; init.el ends here
