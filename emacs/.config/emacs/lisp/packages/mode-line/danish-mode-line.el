;;; danish-mode-line --- Custom mode line. -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Dan Sheikh

;; Author: Dan Sheikh <dan.sheikh@yahoo.com>
;; Version: 0.0.1
;; Keywords: mode-line

;;; Commentary:

;; A custom mode line.

;;; Code:

(defvar-local danish-mode-line--evil-state-symbols
    '((emacs . " 󰯸  ")
      (insert . " 󰰄  ")
      (motion . " 󰰐  ")
      (normal . " 󰰓  ")
      (operator . " 󰲞  ")
      (replace . " 󰰟  ")
      (visual . " 󰰫  "))
  "Custom Evil state symbols.")

(defvar-local danish-mode-line--evil-state-colors
    '((danish-mode-line--evil-emacs "medium purple" "Evil emacs state face.")
      (danish-mode-line--evil-insert "dodger blue" "Evil insert state face.")
      (danish-mode-line--evil-motion "light sea green" "Evil motion state face.")
      (danish-mode-line--evil-normal "medium sea green" "Evil normal state face.")
      (danish-mode-line--evil-operator "medium spring green" "Evil normal state face.")
      (danish-mode-line--evil-replace "magenta" "Evil replace state face.")
      (danish-mode-line--evil-visual "gold" "Evil visual state face."))
  "Names, colors, and docstrings for Evil state faces.")

(dolist (evil-state-color danish-mode-line--evil-state-colors)
  (eval `(defface ,(nth 0 evil-state-color)
           '((t :foreground ,(nth 1 evil-state-color) :inherit bold))
           ,(nth 2 evil-state-color)
           :group 'danish-mode-line)))

(defvar-local danish-mode-line--evil-state-faces
    '((emacs . danish-mode-line--evil-emacs)
      (insert . danish-mode-line--evil-insert)
      (motion . danish-mode-line--evil-motion)
      (normal . danish-mode-line--evil-normal)
      (operator . danish-mode-line--evil-operator)
      (replace . danish-mode-line--evil-replace)
      (visual . danish-mode-line--evil-visual))
  "Custom Evil state faces.")

(defun danish-mode-line--evil-state-symbol (state)
  "Get evil symbol given STATE."
  (alist-get state danish-mode-line--evil-state-symbols "   "))

(defun danish-mode-line--evil-state-face (state)
  "Set evil face given STATE."
  (assq state danish-mode-line--evil-state-faces))

(defvar-local danish-mode-line--modal-state
    '(:eval
      (propertize (danish-mode-line--evil-state-symbol evil-state)
                  'face (danish-mode-line--evil-state-face evil-state)))
  "Custom mode line modal state display.")

(defvar-local danish-mode-line--buffer-name
    '(:eval
      (propertize (buffer-name) 'face 'bold))
  "Custom mode line buffer name display.")

(declare-function flymake--severity "flymake" (type))
(declare-function flymake-diagnostic-type "flymake" (diag))

(defun danish-mode-line--flymake-counter (type)
  "Return count of TYPE diagnostics."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (cl-plusp count)
      (number-to-string count))))

(defvar danish-mode-line--flymake-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'flymake-show-buffer-diagnostics)
    (define-key map [mode-line down-mouse-2] 'flymake-show-project-diagnostics)
    map)
  "Flymake indicator keymap display.")

(defmacro danish-mode-line--flymake-type (type indicator &optional face)
  "Return a function to handle flymake TYPE with stylistic INDICATOR and FACE."
  `(defun ,(intern (format "danish-mode-line--flymake-%s" type)) ()
     (when-let ((count (danish-mode-line--flymake-counter ,(intern (format ":%s" type)))))
       (concat
        (propertize ,indicator 'face 'shadow)
        (propertize count 'face ',(or face type) 'mouse-face 'mode-line-highlight 'local-map danish-mode-line--flymake-map 'help-echo "mouse-1: buffer diagnostics\nmouse-2: project diagnostics")))))

(danish-mode-line--flymake-type error " 󰈸 ")
(danish-mode-line--flymake-type warning "   ")
(danish-mode-line--flymake-type note "   " success)

(defvar-local danish-mode-line--flymake
    '(:eval
      (when (and (bound-and-true-p flymake-mode)
                 (mode-line-window-selected-p))
        (list
         '(:eval (danish-mode-line--flymake-error))
         '(:eval (danish-mode-line--flymake-warning))
         '(:eval (danish-mode-line--flymake-note)))))
  "Custom mode line flymake display.")

(defvar-local danish-mode-line--eglot
    '(:eval
      (when (and (featurep 'eglot)
                 (mode-line-window-selected-p))
        '(eglot--managed-mode eglot--mode-line-format)))
  "Custom mode line eglot display.")

(defvar-local danish-mode-line--major-mode
    '(:eval
      (when (mode-line-window-selected-p)
        (list
         (propertize "λ" 'face 'shadow)
         " "
         (propertize (symbol-name major-mode) 'face 'italic))))
  "Custom mode line major mode display.")

(defvar-local danish-mode-line--display-time
    '(:eval
      (when (mode-line-window-selected-p)
        (list
         (propertize " " 'display
                     `((space :align-to (- (+ right right-fringe right-margin)
                                           ,(+ 1 (string-width display-time-string))))))
         'display-time-string)))
  "Custom mode line time display.")

(dolist (construct '(danish-mode-line--modal-state
                     danish-mode-line--buffer-name
                     danish-mode-line--flymake
                     danish-mode-line--eglot
                     danish-mode-line--major-mode
                     danish-mode-line--display-time))
  (put construct 'risky-local-variable t))

(setq display-time-format "%A, %d %B %Y %H:%M (%Z)")
(setq display-time-interval 60)
(setq display-time-string-forms '((propertize
                                   (format-time-string display-time-format now)
                                   'face 'bold
                                   'help-echo (format-time-string "%A, %d %B %Y" now))
                                  " "))
(setq display-time-default-load-average nil)
(setq display-time-mail-directory nil)
(setq display-time-mail-function nil)
(setq display-time-use-mail-icon nil)
(setq display-time-mail-string nil)
(setq display-time-mail-face nil)
(setq mode-line-right-align-edge 'right-margin)
(display-time-mode 1)

(set-face-attribute 'mode-line nil
                    :background "#5C6380"
                    :foreground "#FFFFFF"
                    :box '(:line-width 8 :color "#5C6380")
                    :height 160
                    :overline nil
                    :underline nil)
(set-face-attribute 'mode-line-inactive nil
                    :background "#494E65"
                    :foreground "#FFFFFF"
                    :box '(:line-width 8 :color "#494E65")
                    :height 160
                    :overline nil
                    :underline nil)

(setq-default mode-line-format
              '("%e"
                danish-mode-line--modal-state
                danish-mode-line--buffer-name
                danish-mode-line--flymake
                " "
                danish-mode-line--eglot
                (vc-mode vc-mode)
                " "
                danish-mode-line--major-mode
                danish-mode-line--display-time))

(provide 'danish-mode-line)
;;; danish-mode-line.el ends here
