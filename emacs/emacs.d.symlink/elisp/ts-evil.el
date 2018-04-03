(use-package evil-leader
  :ensure t
  :defer t
  :config
  (global-evil-leader-mode))

(use-package evil-collection
  :ensure t
  :defer t
  :config
  (evil-collection-init))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)

  :config
  (evil-mode 1)

  (require 'evil-collection)
  (require 'evil-leader)

  (evil-leader/set-leader ",")

  (define-key evil-normal-state-map (kbd "M-s") #'save-buffer)
  (define-key evil-normal-state-map (kbd "M-a") #'mark-whole-buffer)
  (define-key evil-normal-state-map (kbd "M-q") #'evil-quit-all)
  (define-key evil-normal-state-map (kbd "M-c") #'evil-yank)
  (define-key evil-normal-state-map (kbd "M-v") #'clipboard-yank)
  (define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)
  (define-key evil-insert-state-map (kbd "M-v") #'clipboard-yank)

  ;; Ensure ESC quits in all modes: http://stackoverflow.com/a/10166400/61435
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'ts/minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'ts/minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'ts/minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'ts/minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'ts/minibuffer-keyboard-quit)

  (evil-leader/set-key
    "TAB"    'ts/alternate-buffer
    "q"      'ts/kill-window-or-buffer)
  )

(provide 'ts-evil)
