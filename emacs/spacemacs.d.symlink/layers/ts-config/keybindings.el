;;; keybindings.el --- ts-config layer keybindings file for Spacemacs.
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

;; Disable kill-line as I don't use it and accidentally keep hitting it
(global-set-key (kbd "C-k") nil)

;; Map M-h to C-h before we override C-h with window movement command
(global-set-key (kbd "M-h") (lookup-key global-map (kbd "C-h")))

;; (global-set-key (kbd "M-e") 'neotree-toggle)
(global-set-key (kbd "M-e") 'treemacs-projectile)
(global-set-key (kbd "M-r") 'spacemacs/helm-jump-in-buffer)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-/") 'iedit-mode)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-c") 'ns-copy-including-secondary)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-P") 'helm-M-x)
(global-set-key (kbd "M-a") 'mark-whole-buffer)
(global-set-key (kbd "M-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "M-f") 'avy-goto-word-1)
(global-set-key (kbd "M-J") 'move-text-line-down)
(global-set-key (kbd "M-K") 'move-text-line-up)
(global-set-key (kbd "M-b") 'helm-mini)
(global-set-key (kbd "M-p") 'helm-projectile-find-file)
(global-set-key (kbd "M-f") 'spacemacs/helm-project-do-ag-region-or-symbol)
(global-set-key (kbd "∫") 'spacemacs/helm-project-do-ag-region-or-symbol)
(global-set-key (kbd "") 'move-text-line-up)

(define-key evil-motion-state-map (kbd "C-w C-h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "C-w C-j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "C-w C-k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "C-w C-l") 'evil-window-right)
(define-key evil-motion-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "C-l") 'evil-window-right)

(add-hook 'js-mode-hook
          (lambda ()
            (define-key js-mode-map (kbd "M-J") 'js2r-move-line-down)
            (define-key js-mode-map (kbd "M-K") 'js2r-move-line-up)))

(add-hook 'vc-annotate-mode-hook
          (lambda ()
            (define-key vc-annotate-mode-map (kbd "C-w C-l")  'evil-window-right)
            (define-key vc-annotate-mode-map (kbd "C-w C-h")  'evil-window-left)
            (define-key vc-annotate-mode-map (kbd "C-w C-j")  'evil-window-down)
            (define-key vc-annotate-mode-map (kbd "C-w C-k")  'evil-window-up)
            (define-key vc-annotate-mode-map (kbd "C-l")      'evil-window-right)
            (define-key vc-annotate-mode-map (kbd "C-h")      'evil-window-left)
            (define-key vc-annotate-mode-map (kbd "C-j")      'evil-window-down)
            (define-key vc-annotate-mode-map (kbd "C-k")      'evil-window-up)))

(with-eval-after-load 'org
  (add-hook 'org-mode-hook
            (lambda ()
              (define-key org-mode-map (kbd "M-e") 'treemacs-toggle)
              ;; (define-key org-mode-map (kbd "M-e") 'neotree-toggle)
              (define-key org-mode-map (kbd "M-a") 'mark-whole-buffer))))

(with-eval-after-load 'eshell
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key evil-insert-state-map (kbd "C-d")     'ts/eshell-quit-or-delete-char)
              (define-key evil-insert-state-map (kbd "C-a")     'eshell-bol)
              (define-key evil-insert-state-map (kbd "C-e")     'end-of-line)
              (define-key evil-insert-state-map (kbd "C-r")     'spacemacs/helm-eshell-history)
              (define-key evil-insert-state-map (kbd "C-n")     'eshell-next-matching-input-from-input)
              (define-key evil-insert-state-map (kbd "C-p")     'eshell-previous-matching-input-from-input)

              (define-key evil-motion-state-map (kbd "C-w C-l") 'evil-window-right)
              (define-key evil-motion-state-map (kbd "C-w C-h") 'evil-window-left)
              (define-key evil-motion-state-map (kbd "C-w C-j") 'evil-window-down)
              (define-key evil-motion-state-map (kbd "C-w C-k") 'evil-window-up))))

(with-eval-after-load 'treemacs
  (add-hook 'treemacs-mode-hook
            (lambda ()
              (define-key treemacs-mode-map (kbd "M-e") 'treemacs-toggle))))

(with-eval-after-load 'neotree
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key neotree-mode-map (kbd "h")       'evil-backward-char)
              (define-key neotree-mode-map (kbd "l")       'evil-forward-char)
              (define-key neotree-mode-map (kbd "o")       'neotree-enter)
              (define-key neotree-mode-map (kbd "O")       'neotree-enter-vertical-split)
              (define-key neotree-mode-map (kbd "C-w C-l") 'evil-window-right)
              (define-key neotree-mode-map (kbd "C-w C-h") 'evil-window-left)
              (define-key neotree-mode-map (kbd "C-w C-j") 'evil-window-down)
              (define-key neotree-mode-map (kbd "C-w C-k") 'evil-window-up)
              (define-key neotree-mode-map (kbd "C-l")     'evil-window-right)
              (define-key neotree-mode-map (kbd "C-h")     'evil-window-left)
              (define-key neotree-mode-map (kbd "C-j")     'evil-window-down)
              (define-key neotree-mode-map (kbd "C-k")     'evil-window-up))))

(with-eval-after-load 'company
  (add-hook 'company-mode-hook
            (lambda ()
              (define-key company-active-map [tab]       'company-complete)
              (define-key company-active-map (kbd "C-f") 'company-next-page)
              (define-key company-active-map (kbd "C-b") 'company-previous-page))))

(define-key evil-normal-state-map (kbd "g f") 'helm-projectile-find-file-dwim)

(define-key helm-map (kbd "C-f") 'helm-next-page)
(define-key helm-map (kbd "C-b") 'helm-previous-page)
(define-key helm-map (kbd "M-v") 'yank)

(spacemacs/set-leader-keys "bS" 'multi-scratch-new)

(spacemacs/declare-prefix "o" "org")
(spacemacs/set-leader-keys "oc" 'org-capture)
(spacemacs/set-leader-keys "oi" 'ts/open-org-inbox)
(spacemacs/set-leader-keys "ob" 'org-switchb)
(spacemacs/set-leader-keys "of" 'ts/open-org-dir)
(spacemacs/set-leader-keys "o/" 'helm-org-rifle-org-directory)
(spacemacs/set-leader-keys "bo" 'org-switchb)
(spacemacs/set-leader-keys "fo" 'ts/open-org-dir)


;;; keybindings.el ends here
