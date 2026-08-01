;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Executed prior to `package.el' and GUI/TUI frame initialization.
;; Configures GC thresholds, native compilation, Truecolor daemon environment,
;; Evil pre-autoload flags, `package-quickstart' paths, and frame defaults.

;;; Code:

;; Defer garbage collection during startup; restore a balanced threshold once ready.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024)
                  gc-cons-percentage 0.1)))

;; Ensure daemon TTY clients negotiate 24-bit Truecolor (16,777,216 colors)
;; before terminal initialization runs.
(unless (getenv "COLORTERM")
  (setenv "COLORTERM" "truecolor"))

;; Native compilation settings (Emacs 31+)
(when (featurep 'native-compile)
  (setq native-comp-jit-compilation t
        native-comp-async-report-warnings-errors 'silent
        native-comp-speed 2))

;; Must be set before `package-quickstart' activates Evil autoloads and hooks.
(defvar evil-mode-buffers nil)
(setq evil-want-integration t
      evil-want-keybinding nil)

;; Align quickstart cache path with `no-littering' (`var/package-quickstart.el').
(setq package-quickstart-file
      (expand-file-name "var/package-quickstart.el" user-emacs-directory)
      package-enable-at-startup t
      package-quickstart t)

;; Suppress GUI toolbars/scrollbars and set Catppuccin Macchiato defaults early.
(setq frame-inhibit-implied-resize t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(font . "Maple Mono NF-14") default-frame-alist)
(push '(background-color . "#24273a") default-frame-alist)
(push '(foreground-color . "#cad3f5") default-frame-alist)

(provide 'early-init)
;;; early-init.el ends here
