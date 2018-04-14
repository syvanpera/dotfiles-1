;;; Package --- Summary

;;; Commentary:
;; The main entry point into Emacs.
;; It either loads the pre-compiledconfiguration file or tangles and loads the
;; literate org configuration file.

;;; Code:

;; Reduce the frequency of garbage collection during startup
(setq gc-cons-threshold 100000000)
;; restore after start-up
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))


(let ((file-name-handler-alist nil))
  (if (file-exists-p (expand-file-name "ts-init.elc" user-emacs-directory))
      (load-file (expand-file-name "ts-init.elc" user-emacs-directory))
    (require 'org)
    (org-babel-load-file (expand-file-name "ts-init.org" user-emacs-directory))))

;;; init.el ends here
