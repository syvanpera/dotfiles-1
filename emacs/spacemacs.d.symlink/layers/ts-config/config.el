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

(setq mu4e-maildir "~/Mail"
      mu4e-sent-folder "/Gmail/Sent Mail"
      mu4e-drafts-folder "/Gmail/Drafts"
      mu4e-trash-folder "/Gmail/Trash"
      mu4e-sent-messages-behavior 'delete)
(setq mu4e-alert-interesting-mail-query
      (concat
       "flag:unread maildir:/Houston/INBOX "
       "OR "
       "flag:unread maildir:/Gmail/INBOX"))

(global-prettify-symbols-mode t)
;; (golden-ratio-mode t)

(add-hook 'evil-visual-state-entry-hook (lambda () (global-hl-line-mode -1)))
(add-hook 'evil-visual-state-exit-hook (lambda () (global-hl-line-mode +1)))

(add-hook 'prog-mode-hook
          (lambda ()
            (push '("<="  . ?≤) prettify-symbols-alist)
            (push '(">="  . ?≥) prettify-symbols-alist)
            (push '("=="  . ?≡) prettify-symbols-alist)
            (push '("===" . ?≣) prettify-symbols-alist)
            (push '("!="  . ?≠) prettify-symbols-alist)
            (push '("!==" . ?≢) prettify-symbols-alist)
            (push '("->"  . ?→) prettify-symbols-alist)
            (push '("<-"  . ?←) prettify-symbols-alist)
            (push '("=>"  . ?⇒) prettify-symbols-alist)))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'electric-indent-mode)
(add-hook 'prog-mode-hook
          (lambda () (progn
                  (add-hook 'evil-insert-state-entry-hook 'ts/line-numbers-absolute nil t)
                  (add-hook 'evil-insert-state-exit-hook 'ts/line-numbers-relative nil t))))

(add-hook 'js-mode-hook (lambda () (setq js2-strict-missing-semi-warning nil)))

;; (add-hook 'coffee-mode-hook 'highlight-indentation-mode)
;; (add-hook 'coffee-mode-hook 'highlight-indentation-current-column-mode)

;;; config.el ends here
