;;; init.el -- Emacs configuration file

;; Author: Dan Sheikh

;;; Commentary:

;; Custom Emacs configuration.

;;; Code:

(require 'package)
(setq
 package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                    ("org" . "https://orgmode.org/elpa/")
                    ("melpa" . "https://melpa.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/"))
 package-archive-priorities '(("gnu" . 9)
                              ("org" . 9)
                              ("melpa-stable" . 10)
                              ("melpa" . 0)))

(package-initialize)
(package-refresh-contents)

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(setq config-file "~/.emacs.d/config.el")
(load config-file 'noerror)

;; Disable bell
(setq ring-bell-function 'ignore)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Set execution path
; (require 'shell-path "~/.emacs.d/shell-path.el")

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;; Configure multi-term
(use-package multi-term
  :init
  (setq multi-term-program "/bin/zsh"))

;; Set frame size
(setq default-frame-alist '((width . 200) (height . 50)))

;; Set cursor type
(setq-default cursor-type 'hbar)

;; Set coding preference
(set-default-coding-systems 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; Set theme
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-soft t))

;; Set default font
(add-to-list 'default-frame-alist '(font . "Dank Mono-12"))
(set-frame-font "Dank Mono-12")

;; Set character table
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Disable backup and auto-save
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Enable (explicit) auto-save
(auto-save-visited-mode 1)
(setq auto-save-visited-interval 3)

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

;; Globally prettify symbols
(global-prettify-symbols-mode 1)
(defun configure-prettify-symbols-alist ()
  "Set prettify symbols alist."
  (setq prettify-symbols-alist '(("map" . ?↦)
                                 ("&&" . ?∧)
                                 ("||" . ?∨)
                                 ("not" . ?¬))))

;; Enable ido mode
(use-package ido
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t
        ido-use-virutal-buffers t))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        enable-recursive-minibuffers t)
  :init
  (ivy-mode 1))

(use-package swiper)

(use-package counsel)

(use-package ranger
  :init
  (setq ranger-override-dired 'ranger
        ranger-cleanup-eagerly t
        ranger-modify-header t
        ranger-header-func 'ranger-header-line
        ranger-parent-header-func 'ranger-parent-header-line
        ranger-preview-header-func 'ranger-preview-header-line
        ranger-hide-cursor nil
        ranger-footer-delay 0.2
        ranger-preview-delay 0.2
        ranger-parent-depth 2
        ranger-preview-file t
        ranger-width-preview 0.5
        ranger-dont-show-binary t
        ranger-excluded-extensions '("iso" "mkv" "mp3" "mp4")))

;; Enable which-key
(use-package which-key
  :config
  (which-key-mode 1)
  :init
  (setq which-key-idle-delay 0.5)
  (setq which-key-prefix-prefix "+"))

;; Enable general
(use-package general
  :after which-key
  :config
  (general-override-mode 1)
  (general-create-definer benevolent-dictator
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  (benevolent-dictator
    ";" (general-simulate-key ";" :which-key ";")
    "c" (general-simulate-key "C-c" :which-key "C-c")
    "h" (general-simulate-key "C-h" :which-key "C-h")
    "x" (general-simulate-key "C-x" :which-key "C-x")
    "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
    "SPC" '(counsel-M-x :which-key "M-x")
    "/"   '(counsel-ag :which-key "silver searcher")
    ;; Buffer management
    "b"  '(:ignore t :which-key "buffer")
    "bl" '(ivy-switch-buffer :which-key "buffer list")
    "bn" '(next-buffer :which-key "next buffer")
    "bp" '(previous-buffer :which-key "previous buffer")
    "br" '(counsel-recentf :which-key "recent files")
    "bs" '(save-some-buffers :which-key "buffer save")
    "bx" '(ido-kill-buffer :which-key "buffer delete")
    "bq" '(save-buffers-kill-terminal :which-key "buffer quit")
    ;; File management
    "f"  '(:ignore t :which-key "file")
    "f." '(counsel-find-file :which-key "file search")
    "fr"  '(ranger :which-key "ranger")
    "."  '(counsel-find-file :which-key "file search")
    ;; Git management
    "g"  '(:ignore t :which-key "git")
    "gs" '(magit-status :which-key "git status")
    "gd" '(magit-dispatch-popup :which-key "git dispatch")
    ;; Interface management
    "i"  '(:ignore t :which-key "interface")
    "ie" '(eshell :which-key "open eshell")
    "im" '(mini-eshell :which-key "open mini-eshell")
    ;; Search management
    "s"  '(:ignore t :which-key "search")
    "ss" '(swiper :which-key "swiper")
    ;; Tree management
    "t"  '(:ignore t :which-key "tree")
    "tf" '(treemacs-find-file :which-key "treemacs find file")
    "tt" '(treemacs :which-key "treemacs")
    "tw" '(treemacs-select-window :which-key "treemacs window")
    ;; Window management
    "w"  '(:ignore t :which-key "window")
    "wl" '(windmove-right :which-key "move right")
    "wh" '(windmove-left :which-key "move left")
    "wk" '(windmove-up :which-key "move up")
    "wj" '(windmove-down :which-key "move down")
    "w+" '(split-window-right :which-key "split right")
    "w-" '(split-window-below :which-key "split below")
    "wo" '(delete-other-windows :which-key "delete other window")
    "wx" '(delete-window :which-key "delete window"))
  :init
  (setq which-key-idle-delay 0.5))

;; Enable global buffer auto-revert
(global-auto-revert-mode 1)
(setq auto-revert-check-vc-info t)

;; Enable evil
(use-package evil
  :init
  (evil-mode t)
  :config
  (evil-set-initial-state 'term-mode 'emacs)
  (setq evil-default-cursor 'hbar
        evil-emacs-state-cursor 'hbar
        evil-normal-state-cursor 'hbar
        evil-motion-state-cursor 'hbar
        evil-visual-state-cursor 'hbar
        evil-insert-state-cursor 'hbar
        evil-replace-state-cursor 'hbar
        evil-operator-state-cursor 'hbar)
  (define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down)
  (define-key evil-visual-state-map (kbd "C-d") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-insert-state-map (kbd "C-u")
    (lambda ()
      (interactive)
      (evil-delete (point-at-bol) (point)))))

;; Enable evil surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; Enable evil embrace
(use-package evil-embrace
  :config
  (evil-embrace-enable-evil-surround-integration))

;; Enable evil easymotion
(use-package evil-easymotion
  :config
  (evilem-default-keybindings ";"))

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

;; Enable projectile
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;; Enable org mode
(use-package org
  :config
  (visual-line-mode 1)
  :init
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN PROGRESS(i)" "|" "CANCELLED(c)" "DONE(d)")))
	(setq org-log-done 'time)
	(setq org-agenda-files
        (append (file-expand-wildcards "~/org/agendas/*.org"))))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :requires org)

(use-package org-journal
  :requires org)

(use-package org-sticky-header
  :config
  (add-hook 'org-mode-hook (lambda () (org-sticky-header-mode)))
  :requires org)

(use-package org-projectile
  :after org
  :after projectile
  :config
  (setq org-projectile-projects-file "~/.org/projects/todos.org"
        org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  (push (org-projectile-project-todo-entry) org-capture-templates))

;; Enable treemacs
(use-package treemacs
  :defer t
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  :bind
  (:map global-map
        ("s-t" . treemacs)
        ("s-f" . treemacs-find-file)))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)

;; Enable YASnippet
(use-package yasnippet
  :init
  (yas-global-mode t))

;; Enable LSP
(use-package lsp-mode
  :commands lsp
  :hook
  ((clojure-mode . lsp-mode)
   (scala-mode . lsp-mode)
   (go-mode . lsp-mode)
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :init
  (setq lsp-ui-sideline-enable nil)
  :requires
  lsp-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  (lsp-metals-treeview-enable t)
  (setq lsp-metals-treeview-show-when-views-received t))

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
  (setq company-backends '(company-capf company-dabbrev-code company-dabbrev company-yasnippet company-files company-keywords))
  (add-hook 'after-init-hook 'global-company-mode))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends) 'company-elisp)))
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(add-hook 'cmake-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends) 'company-cmake)))

(use-package company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

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

;; Configure clojure & clojurescript
(use-package paredit)
(use-package rainbow-delimiters)
(use-package cider
  :init
  (setq cider-show-error-buffer nil)
  (setq cider-repl-display-help-banner nil)
  (setq cider-repl-shortcut-dispatch-char ?\;)
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

;; Enable scala and sbt
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands sbt-start sbt-command)

;; Enable elixir
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
              (add-to-list (make-local-variable 'company-backends) 'company-cs))))

(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :interpreter ("node" . js2-mode)
  :init
  (add-hook 'js2-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends) 'company-tern))))

(use-package prettier-js
  :init
  (add-hook 'j2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode))

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

;; Enable haskell
(use-package haskell-mode
  :init
  (setq haskell-tags-on-save t)
  (setq haskell-interactive-mode-eval-mode 'haskell-mode)
  (setq ghc-report-errors nil)
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook 'configure-prettify-symbols-alist)
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

;; Enable elm
(use-package elm-mode
  :init
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-hook 'elm-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends) 'company-elm)))
  :config
  (setq elm-format-on-save t))

;; Enable groovy
(use-package groovy-mode)

;; Enable perl 6
(use-package perl6-mode)

;; Enable statistics
(use-package ess)

;; Enable magit
(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

;; Enable docker
(use-package dockerfile-mode
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(provide 'init)

;;; init.el ends here
