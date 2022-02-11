;;; init.lisp --- StumpWM init configuration

;; Author: Dan Sheikh

;;; Commentary:

;; StumpWM init configuration file.

;;; Code:

(in-package :stumpwm)

(setf *default-package* :stumpwm)
(setf *startup-message* "Welcome!")
;(setf *mouse-focus-policy* :click
;      *float-window-modifier :super)

(set-module-dir "~/.stumpwm.d/modules")

(run-shell-command "xsetroot -cursor_name left_ptr")

(mapc #'load-module '("beckon"
                      "end-session"
                      "globalwindows"
                      "stump-backlight"
                      "swm-gaps"
                      "ttf-fonts"
                      "urgentwindows"))

(mapc #'load '("~/.stumpwm.d/commands.lisp"
               "~/.stumpwm.d/groups.lisp"
               "~/.stumpwm.d/keybindings.lisp"
               "~/.stumpwm.d/theme.lisp"))

;;; init.lisp ends here
