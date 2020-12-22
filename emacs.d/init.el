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
                              ("melpa-stable" . 8)
                              ("melpa" . 10)))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(setq config-file "~/.emacs.d/config.el")
(load config-file 'noerror)

;; Default directory
(setq default-directory "~/")

;; Space width
(setq preferred-tab-width 2)

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
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; Set frame size
(setq default-frame-alist '((width . 225) (height . 65)))

;; Set cursor type
(setq-default cursor-type 'box)

;; Set coding preference
(set-default-coding-systems 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; Enable icons
(use-package all-the-icons)

;; Set theme
(use-package nord-theme
  :config
  (load-theme 'nord t))

;; Enable powerline
(use-package powerline)

;; Enable airline
(use-package airline-themes
  :init
  (setq powerline-default-separator           'utf-8
        powerline-utf-8-separator-left        #xe0b0
        powerline-utf-8-separator-right       #xe0b2
        airline-utf-glyph-separator-left      #xe0b0
        airline-utf-glyph-separator-right     #xe0b2
        airline-utf-glyph-subseparator-left   #xe0b1
        airline-utf-glyph-subseparator-right  #xe0b3
        airline-utf-glyph-branch              #xe0a0
        airline-utf-glyph-readonly            #xe0a2
        airline-utf-glyph-linenumber          #xe0a1
        airline-cursor-colors                 t
        airline-display-directory             'airline-directory-shortened)
  :config
  (load-theme 'airline-base16_nord t))

;; Set default font
(add-to-list 'default-frame-alist '(font . "Source Code Pro for Powerline-12"))
(set-frame-font "Source Code Pro for Powerline-12")

;; Set character table
;; (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
;;                (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
;;                (36 . ".\\(?:>\\)")
;;                (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
;;                (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
;;                (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
;;                (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
;;                (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
;;                (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
;;                (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
;;                (48 . ".\\(?:x[a-zA-Z]\\)")
;;                (58 . ".\\(?:::\\|[:=]\\)")
;;                (59 . ".\\(?:;;\\|;\\)")
;;                (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
;;                (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
;;                (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
;;                (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
;;                (91 . ".\\(?:]\\)")
;;                (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
;;                (94 . ".\\(?:=\\)")
;;                (119 . ".\\(?:ww\\)")
;;                (123 . ".\\(?:-\\)")
;;                (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
;;                (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
;;   (dolist (char-regexp alist)
;;     (set-char-table-range composition-function-table (car char-regexp)
;;                           `([,(cdr char-regexp) 0 font-shape-gstring]))))

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
(global-display-line-numbers-mode t)
(setq display-line-numbers t)

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

(use-package swiper
  :after ivy
  :bind
  (("C-s" . swiper)))

(use-package counsel
  :config
  (defun counsel-fzf-dir (arg)
    (counsel-fzf ivy-text (read-directory-name
                           (concat
                            (car (split-string counsel-fzf-cmd)) " in directory: "))))
  (ivy-add-actions
   'counsel-fzf
   '(("s" counsel-fzf-dir "search directory")))
  :init
  (setq counsel-fzf-cmd "fd --type f | fzf -f \"%s\""))

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
  (setq which-key-idle-delay 0.2)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-side-window-max-width 0.33)
  (setq which-key-add-column-padding 4)
  (setq which-key-max-display-columns 6)
  (setq which-key-separator " » ")
  (setq which-key-prefix-prefix "+")
  (setq which-key-show-remaining-keys t)
  (setq which-key-allow-evil-operators t))

;; Enable general
(use-package general
  :after which-key
  :config
  (general-override-mode 1)
  (general-create-definer benevolent-dictator
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  (defun shrink-horizontally ()
    (interactive)
    (shrink-window-horizontally 10))
  (defun enlarge-horizontally ()
    (interactive)
    (enlarge-window-horizontally 10))
  (defun shrink-vertically ()
    (interactive)
    (shrink-window 10))
  (defun enlarge-vertically ()
    (interactive)
    (enlarge-window 10))
  (benevolent-dictator
    ";" (general-simulate-key ";" :which-key ";")
    "c" (general-simulate-key "C-c" :which-key "C-c")
    "h" (general-simulate-key "C-h" :which-key "C-h")
    "x" (general-simulate-key "C-x" :which-key "C-x")
    "TAB" '(ivy-switch-buffer :which-key "switch buffer")
    "SPC" '(counsel-M-x :which-key "M-x")
    "/"   '(counsel-rg :which-key "ripgrep")
    ;; Buffer management
    "b"  '(:ignore t :which-key "buffer")
    "bl" '(ivy-switch-buffer :which-key "buffer list")
    "bn" '(next-buffer :which-key "next buffer")
    "bp" '(previous-buffer :which-key "previous buffer")
    "br" '(counsel-recentf :which-key "recent files")
    "bS" '(save-some-buffers :which-key "buffer any save")
    "bs" '(save-buffer :which-key "buffer save")
    "bk" '(ido-kill-buffer :which-key "buffer kill")
    ;; Describe
    "d"  '(:ignore t :which-key "describe")
    "df" '(counsel-describe-function :which-key "describe function")
    "dv" '(counsel-describe-variable :which-key "describe variable")
    ;; File management
    "f"  '(:ignore t :which-key "file")
    "f." '(counsel-find-file :which-key "file search")
    "ff" '(counsel-fzf :which-key "file fuzzy search")
    "fr" '(ranger :which-key "ranger")
    "."  '(counsel-find-file :which-key "file search")
    ;; Git management
    "g"  '(:ignore t :which-key "git")
    "gc" '(counsel-git :which-key "git counsel")
    "gs" '(magit-status :which-key "git status")
    "gd" '(magit-dispatch-popup :which-key "git dispatch")
    ;; Interface management
    "i"  '(:ignore t :which-key "interface")
    "ie" '(eshell :which-key "open eshell")
    "im" '(mini-eshell :which-key "open mini-eshell")
    ;; Org management
    "o"  '(:ignore t :which-key "org")
    "od" '(org-deadline :which-key "deadline")
    "ot" '(org-time-stamp :which-key "timestamp")
    ;; Project management
    "p"  '(:ignore t :which-key "project")
    "pp" '(projectile-switch-project :which-key "switch project")
    ;; Quit
    "q"  '(:ignore t :which-key "quit")
    "qq" '(save-buffers-kill-terminal :which-key "save & quit")
    "qQ" '(kill-emacs :which-key "quit")
    ;; Search management
    "s"  '(:ignore t :which-key "search")
    "ss" '(swiper :which-key "swiper")
    ;; Tree management
    "t"  '(:ignore t :which-key "tree")
    "tt" '(neotree-toggle :which-key "neotree-toggle")
    ;; Window management
    "w"  '(:ignore t :which-key "window")
    "wl" '(windmove-right :which-key "move right")
    "wh" '(windmove-left :which-key "move left")
    "wk" '(windmove-up :which-key "move up")
    "wj" '(windmove-down :which-key "move down")
    "w+" '(split-window-right :which-key "split right")
    "w-" '(split-window-below :which-key "split below")
    "wo" '(delete-other-windows :which-key "delete other window")
    "wx" '(delete-window :which-key "delete window")
    "w=" '(balance-windows :which-key "balance")
    "w<" '(shrink-horizontally :which-key "shrink horizontally")
    "w>" '(enlarge-horizontally :which-key "enlarge horizontally")
    "w_" '(shrink-vertically :which-key "shrink vertically")
    "w^" '(enlarge-vertically :which-key "enlarge vertically")))

;; Enable global buffer auto-revert
(global-auto-revert-mode 1)
(setq auto-revert-check-vc-info t)

;; Enable evil
(use-package evil
  :init
  (evil-mode t)
  :config
  (evil-set-initial-state 'term-mode 'emacs)
  (setq evil-default-cursor 'box
        evil-emacs-state-cursor 'box
        evil-normal-state-cursor 'box
        evil-motion-state-cursor 'box
        evil-visual-state-cursor 'box
        evil-insert-state-cursor 'box
        evil-replace-state-cursor 'box
        evil-operator-state-cursor 'box)
  (define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-s") 'swiper)
  (define-key evil-visual-state-map (kbd "C-d") 'evil-scroll-down)
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
  (evilem-default-keybindings ","))

;; Enable flycheck
(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (provide 'init-flycheck)
  :init
  (setq flycheck-check-syntax-automatically '(mode-enabled idle-buffer-switch idle-change save)
        flycheck-idle-buffer-switch-delay 1.0
        flycheck-idle-change-delay 3.0))

(use-package flycheck-color-mode-line
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package flycheck-pos-tip)

;; Enable projectile
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  :init
  (setq projectile-project-search-path '("~/Workspace")
        projectile-completion-system 'ivy
        projectile-switch-project-action 'neotree-projectile-action))

;; Enable markdown
(use-package markdown-mode
  :commands
  (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

;; Enable markdown preview
(use-package markdown-preview-mode
  :requires markdown-mode)

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
  :after (org projectile)
  :config
  (setq org-projectile-projects-file "~/.org/projects/todos.org"
        org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  (push (org-projectile-project-todo-entry) org-capture-templates))

;; Enable neotree
(use-package neotree
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-smart-open t
        neo-autorefresh nil))

;; Enable YASnippet
(use-package yasnippet
  :init
  (yas-global-mode t))

;; Enable LSP
(use-package lsp-mode
  :commands lsp
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (setq lsp-clojure-custom-server-command '("bash" "-c" "~/.emacs.d/.cache/lsp/clojure/clojure-lsp")
        lsp-enable-indentation nil)
  :hook
  ((clojure-mode . lsp)
   (clojurec-mode . lsp)
   (clojurescript-mode . lsp)
   (python-mode . lsp)
   (scala-mode . lsp)
   (go-mode . lsp)
   (terraform-mode . lsp)
   (lsp-mode . (lambda ()
                 (let ((lsp-keymap-prefix "C-c l"))
                   (lsp-enable-which-key-integration))))))

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

(use-package dap-mode)

(use-package lsp-python-ms
  :init
  (setq lsp-python-ms-auto-install-server t))

;; Enable company backends
(use-package company-go)
(use-package company-ghc
  :init
  (setq company-ghc-show-info t))
(use-package company-irony)

;; Enable company mode
(use-package company
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  (add-hook 'after-init-hook #'company-tng-mode)
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-backends '(company-capf company-dabbrev-code company-dabbrev company-yasnippet company-files company-keywords)
        company-tooltip-align-annotations t))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends) 'company-elisp)))
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(add-hook 'cmake-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends) 'company-cmake)))

;; Enable python
(use-package python
  :config
  (setq python-shell-interpreter (substring (shell-command-to-string "which ipython") 0 -1)
        python-shell-interpreter-args "--simple-prompt -i"))

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
(use-package lispy
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy))

(use-package lispyville
  :config
  (lispyville-set-key-theme '(operators c-w additional slurp/barf-cp))
  :init
  (add-hook 'emacs-lisp-mode-hook #'lispyville-mode)
  (add-hook 'lispy-mode-hook #'lispyville-mode))

(use-package paredit)

(use-package rainbow-delimiters
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

;; (use-package cider
;;   :init
;;   (setq cider-show-error-buffer nil)
;;   (setq cider-repl-display-help-banner nil)
;;   (setq cider-repl-shortcut-dispatch-char ?\;)
;;   (cider-auto-test-mode 1)
;;   (add-hook 'cider-mode-hook 'cider-company-enable-fuzzy-completion)
;;   (add-hook 'cider-repl-mode-hook 'cider-company-enable-fuzzy-completion))

(use-package inf-clojure
  :init
  (setq inf-clojure-prompt-read-only nil
        inf-clojure-custom-repl-type "clj"
        inf-clojure-custom-startup "clj -A:compliment")
  (add-hook 'inf-clojure-mode-hook #'eldoc-mode)
  (add-hook 'inf-clojure-mode-hook (lambda ()
                                     (setq completion-at-point-functions nil))))

(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
  (add-hook 'clojurescript-mode-hook #'inf-clojure-minor-mode)
  ;; (add-hook 'clojure-mode-hook 'cider-mode)
  ;; (add-hook 'clojurescript-mode-hook 'cider-mode)
  ;; (add-hook 'cider-repl-mode-hook 'lispy-mode)
  ;; (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'eldoc-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojurescript-mode-hook #'rainbow-delimiters-mode))

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
(use-package web-mode)

(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :init
  (setq js-indent-level preferred-tab-width)
  :interpreter ("node" . js2-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (eldoc-mode +1))

(use-package tide
  :config
  (setq js-indent-level preferred-tab-width
        tide-completion-detailed t
        tide-always-show-documentation t
        tide-server-max-response-length 524288))

(use-package typescript-mode
  :hook ((typescript-mode . rainbow-delimiters-mode)
         (typescript-mode . setup-tide-mode))
  :init
  (setq typescript-indent-level preferred-tab-width))

(use-package prettier-js
  :init
  (setq prettier-js-args '("--arrow-parens" "avoid"))
  (add-hook 'j2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode))

(use-package json-mode
  :init
  (add-hook 'json-mode-hook 'prettier-js-mode))

(use-package sass-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode)))

;; Enable psc-ide
(use-package psc-ide
  :init
  (setq psc-ide-use-npm-bin t))

;; Enable purescript
(use-package purescript-mode
  :init
  (add-hook 'purescript-mode-hook (lambda ()
                                    (psc-ide-mode)
                                    (turn-on-purescript-indentation)))
  :requires psc-ide)

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
(use-package raku-mode)

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

(use-package hcl-mode
  :init
  (setq hcl-indent-level 2))

(use-package terraform-mode
  :init
  (setq terraform-indent-level 2)
  :requires (hcl-mode))

(provide 'init)

;;; init.el ends here
