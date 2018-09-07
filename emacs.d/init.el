;;; Code:

(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/")
		    ("emacs-pe" . "http://emacs-pe.github.io/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; (require 'shell-path "~/.emacs.d/shell-path.el")

;; Configure Multi-term
(use-package multi-term
  :init
  (setq multi-term-program "/bin/zsh"))

;; Set frame size
(setq default-frame-alist '((width . 160) (height . 40)))

;; Set coding preference
(set-default-coding-systems 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; Set theme
(use-package dracula-theme)

;; Set default font
(add-to-list 'default-frame-alist '(font . "Droid Sans Mono Slashed for Powerline-12"))
(set-frame-font "Droid Sans Mono Slashed for Powerline-12")

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

;; Enable line numbers
(global-linum-mode t)

;; Set tab (space) width
(setq-default tab-width 2
	      indent-tabs-mode nil)

(show-paren-mode 1)

;; Auto-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Set style
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "bsd")))

(setq c-basic-offset 2)
(setq sh-basic-offset 2)

;; Enable Ido mode
(use-package ido
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t
        ido-use-virutal-buffers t))

;; Enable and customize helm
(use-package helm
  :config
  ;; Customize keybindings
  (global-set-key (kbd "M-]") 'next-buffer)
  (global-set-key (kbd "M-[") 'previous-buffer)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-m") 'helm-M-x)
  (global-set-key (kbd "C-c C-m") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files))

(use-package helm-ag
  :init
  (setq helm-follow-mode-persistent t)
  (setq helm-ag-base-command "ack --nocolor --nogroup")
  :config
  (global-set-key (kbd "M-f") 'helm-do-ag)
  (global-set-key (kbd "M-s") 'helm-do-ag-this-file))

;; Enable global buffer auto-revert
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

(use-package flycheck-color-mode-line
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package flycheck-pos-tip)

;; Enable yasnippet
(use-package yasnippet
  :init
  (yas-global-mode t))

;; Enable company backends
(use-package company-anaconda)
(use-package company-tern)
(use-package company-irony)
(use-package company-go)
(use-package company-ghc
  :init
  (setq company-ghc-show-info t))

;; Enable company mode
(use-package company
  :init
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-backends '(company-capf company-dabbrev company-dabbrev-code company-yasnippet company-files company-keywords))
  (add-hook 'after-init-hook 'global-company-mode))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (add-to-list (make-local-variable 'company-backends) 'company-elisp)))
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(add-hook 'cmake-mode-hook
	  (lambda ()
	    (add-to-list (make-local-variable 'company-backends) 'company-cmake)))

;; Enable python
(use-package anaconda-mode)
(use-package python
  :config
  (setq python-shell-interpreter (substring (shell-command-to-string "which ipython") 0 -1)
        python-shell-interpreter-args "--simple-prompt -i")
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (add-hook 'python-mode-hook
	    (lambda ()
	      (add-to-list (make-local-variable 'company-backends) 'company-anaconda))))

;; Enable irony
(use-package irony
  :init
  (add-hook 'irony-mode-hook 'custom-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook
	    (lambda ()
	      (add-to-list (make-local-variable 'company-backends) 'company-irony)))
  :config
  (defun custom-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async)))

;; Enable rust
(use-package rust-mode
  :init
  (setq rust-format-on-save t))

;; Enable go
(use-package go-mode
  :init
  (add-hook 'go-mode-hook
	    (lambda ()
	      (add-to-list (make-local-variable 'company-backends) 'company-go))))

;; Configure Clojure & ClojureScript
(use-package paredit)
(use-package rainbow-delimiters)
(use-package cider
  :init
  (setq cider-show-error-buffer nil)
  (setq cider-repl-display-help-banner nil)
  (cider-auto-test-mode 1)
  (add-hook 'cider-mode-hook 'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-repl-mode-hook 'cider-company-enable-fuzzy-completion))
(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'clojurescript-mode-hook 'cider-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojurescript-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojurescript-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode))

;; Enable Elixir
(use-package elixir-mode
  :init
  (add-hook 'elixir-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'elixir-format nil t))))
(use-package alchemist
  :init
  (setq alchemist-key-command-prefix (kbd "C-c ,")))

;; Enable web development support
(use-package web-mode
  :init
  (add-hook 'web-mode-hook
	    (lambda ()
	      (add-to-list (make-local-variable 'company-backends) 'company-css))))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :interpreter ("node" . js2-mode)
  :init
  (add-hook 'js2-mode-hook
	    (lambda ()
	      (add-to-list (make-local-variable 'company-backends) 'company-tern)
          (when (executable-find "eslint")
            (flycheck-select-checker 'javascript-eslint)))))

(use-package json-mode)

(use-package sass-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode)))

;; Enable F#
(use-package fsharp-mode)

;; Add opam emacs directory to the load-path
(if (executable-find "opam")
    (progn
      (setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
      (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
      (load (concat opam-share "/emacs/site-lisp/tuareg-site-file"))
      ;; Configure UTop
      (setq utop-command "opam config exec -- utop -emacs")
      ;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
      (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
      ;; ## end of OPAM user-setup addition for emacs / base ## keep this line
      ))

;; Enable Haskell
(use-package haskell-mode
  :init
  (setq haskell-tags-on-save t)
  (setq haskell-interactive-mode-eval-mode 'haskell-mode)
  (setq ghc-report-errors nil)
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook
	    (lambda ()
	      (add-to-list (make-local-variable 'company-backends) 'company-ghc)))
  :config
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-mode-map (kbd "C-c .") 'haskell-mode-jump-to-def-or-tag)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal))

;; Enable PureScript
(use-package purescript-mode
  :pin emacs-pe
  :init
  (add-hook 'purescript-mode-hook #'haskell-indentation-mode))
(use-package psci
  :pin emacs-pe)

;; Enable Groovy
(use-package groovy-mode)

;; Enable Ensime
(use-package ensime
  :pin melpa-stable)

;; Enable Perl 6
(use-package perl6-mode)

;; Enable Statistics
(use-package ess)

;; Enable magit
(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

(provide 'init)

;;; init.el ends here
