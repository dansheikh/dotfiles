;;; package -- Summary:
;;; Commentary:
;;; Code:
(require 'cl)

(defvar required-packages
  '(cider
    clojure-mode    
    company
    company-anaconda
    flycheck
    fsharp-mode
    go-mode
    magit
    python-mode
    sass-mode
    web-mode
    zenburn-theme) "List of required packages.")

(defun packages-installed-p ()
  (loop for pack in required-packages
	when (not (package-installed-p pack)) do (return nil)
	finally (return t)))

(unless (packages-installed-p)
  (message "%s" "Refreshing Emacs package database...")
  (package-refresh-contents)
  (message "%s" "Installing required packages")
  (dolist (pack required-packages)
    (when (not (package-installed-p pack))
      (package-install pack))))

(provide 'required-packages)
;;; required-packages.el ends here
