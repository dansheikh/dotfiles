;; [[file:emacs.org::*Early Init Performance][Early Init Performance:1]]
;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Dan Sheikh

;; Author: Dan Sheikh <dan.sheikh@yahoo.com>

;;; Commentary:

;; Performance optimizations loaded before package initialization.
;; These settings are applied before the GUI is rendered and before
;; package.el is initialized, providing the earliest possible optimization.
;;
;; Key optimizations:
;; - Defer garbage collection during startup
;; - Disable UI elements before frame creation
;; - Configure native compilation
;; - Disable package.el in favor of elpaca

;;; Code:

;; Defer garbage collection during startup for faster load time.
;; The threshold will be reset after startup completes.
;; Reference: https://www.gnu.org/software/emacs/manual/html_node/elisp/Garbage-Collection.html
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent premature redisplays during startup.
;; This avoids flickering as packages load and configure.
(setq-default inhibit-redisplay t
              inhibit-message t)

;; Reset performance settings after startup completes
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB - balanced for interactive use
                  gc-cons-percentage 0.1
                  inhibit-redisplay nil
                  inhibit-message nil)))

;; Native compilation settings for Emacs 28+
;; Reference: https://www.emacswiki.org/emacs/GccEmacs
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil  ; Don't pop up warnings
        native-comp-deferred-compilation t            ; Compile in background
        native-comp-speed 2))                         ; Optimization level (0-3)

;; Disable package.el in favor of elpaca package manager.
;; This must be set before package.el would normally initialize.
(setq package-enable-at-startup nil)

;; Prevent frame resizing based on font during startup.
;; This speeds up initial frame creation.
(setq frame-inhibit-implied-resize t)

;; Disable unnecessary UI elements early, before frame creation.
;; This is faster than disabling them after the frame exists.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(provide 'early-init)
;;; early-init.el ends here
;; Early Init Performance:1 ends here
