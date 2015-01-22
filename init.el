(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Configure load path
(add-to-list 'load-path "~/.emacs.d/")

;; Set frame size
;;(setq default-frame-alist '((width . 120) (height . 30)))

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Disable backup and auto-save
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Disable menu, scroll, & tool bars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Vertical window split default
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Set root directory
(setq root-dir (file-name-directory
		(or (buffer-file-name) load-file-name)))

;; Add themes to path
(load-theme 'solarized-dark t)

;; Enable line numbers
(global-linum-mode t)

;; Set tab (space) width
(setq tab-width 4
      indent-tabs-mode nil)

(show-paren-mode 1)

;; Set style
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "bsd")))

(setq c-basic-offset 4)

;; Enable Ido mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virutal-buffers t)

;; Enable magit
(require 'magit)

;; Enable flycheck and override defaults
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save idle-change mode-enable)
      flycheck-idle-change-delay 1.0)
(provide 'init-flycheck)

;; Enable global company mode
(add-hook 'after-init-hook #'global-company-mode)

;; Enable python
(require 'python)
(setq python-shell-interpreter "ipython")
(add-hook 'python-mode-hook 'anaconda-mode)
;;(add-to-list 'company-backends 'company-anaconda)

;; Enable fsharp
(require 'fsharp-mode)

;; Configure Clojure
(require 'clojure-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; Enable go
(require 'go-mode)
(require 'go-eldoc)
(require 'company)
(require 'company-go)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) '(company-go))))

;; Enable web development support
(require 'php-mode)
(require 'web-mode)
(require 'sass-mode)

(provide 'init)
;;; init.el ends here
