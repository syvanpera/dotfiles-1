;;; config.el --- ts-config layer config file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Tuomo Syvänperä <tinimini@coruscant.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(with-current-buffer "*scratch*"  (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*" (emacs-lock-mode 'kill))

(setq prettify-symbols-unprettify-at-point t
      powerline-image-apple-rgb t
      neo-theme 'icons)

(setq-default indent-tabs-mode nil
              standard-indent 2
              tab-width 2
              js-indent-level 2
              js2-basic-offset 2
              web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-indent-style 2)

(setq split-height-threshold 0
      split-width-threshold 0)

(setq evil-want-C-i-jump nil)
(setq treemacs-no-png-images nil)

;; (global-prettify-symbols-mode t)

(add-hook 'evil-visual-state-entry-hook (lambda () (global-hl-line-mode -1)))
(add-hook 'evil-visual-state-exit-hook (lambda () (global-hl-line-mode +1)))

; (add-hook 'prog-mode-hook
;           (lambda ()
;             (push '("<="  . ?≤) prettify-symbols-alist)
;             (push '(">="  . ?≥) prettify-symbols-alist)
;             (push '("=="  . ?≡) prettify-symbols-alist)
;             (push '("===" . ?≣) prettify-symbols-alist)
;             (push '("!="  . ?≠) prettify-symbols-alist)
;             (push '("!==" . ?≢) prettify-symbols-alist)
;             (push '("->"  . ?→) prettify-symbols-alist)
;             (push '("<-"  . ?←) prettify-symbols-alist)
;             (push '("=>"  . ?⇒) prettify-symbols-alist)))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'electric-indent-mode)

(add-hook 'js-mode-hook (lambda () (setq js2-strict-missing-semi-warning nil)))

;;; config.el ends here
