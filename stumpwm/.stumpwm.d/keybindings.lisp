;;; keybindings.lisp --- StumpWM keybinding configuration

;; Author: Dan Sheikh

;;; Commentary:

;; Custom StumpWM keybinding configuration.

;;; Code:

;; Prefix key.
(set-prefix-key (kbd "C-z"))

;; Group navigation.
(define-key *root-map* (kbd ".") "gnext")
(define-key *root-map* (kbd ",") "gprev")
(define-key *top-map* (kbd "s-Up") "gnext")
(define-key *top-map* (kbd "s-Down") "gprev")

;; Split management.
(define-key *root-map* (kbd "S") "vsplit")
(define-key *root-map* (kbd "s") "hsplit")

;; Window navigation.
(define-key *root-map* (kbd "s-Right") "gnext-with-window")
(define-key *root-map* (kbd "s-Left") "gprev-with-window")
(define-key *top-map* (kbd "s-Right") "pull-hidden-next")
(define-key *top-map* (kbd "s-Left") "pull-hidden-previous")

;;; keybindings.lisp ends here
