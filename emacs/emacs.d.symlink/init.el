;; Unclutter the interface immediately
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(when (fboundp 'mouse-wheel-mode) (mouse-wheel-mode 1))

(require 'package)
(setq package-enable-at-startup nil
      package-archives
      '(("melpa"        . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("gnu"          . "http://elpa.gnu.org/packages/")
        ("marmalade"    . "http://marmalade-repo.org/packages/")))

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'ts-funcs)

(defvar ts/backup-directory (concat user-emacs-directory "backups"))
(defvar ts/theme-directory (concat user-emacs-directory "themes"))

(if (not (file-exists-p ts/backup-directory))
    (make-directory ts/backup-directory t))

(if (not (file-exists-p ts/theme-directory))
    (make-directory ts/theme-directory t))

(setq backup-directory-alist `(("." . ,ts/backup-directory)))

(setq make-backup-files nil
      backup-by-copying t
      version-control nil
      delete-old-versions t
      delete-by-moving-to-trash nil
      create-lockfiles nil
      kept-old-versions 1
      kept-new-versions 1)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil
      ring-bell-function 'ignore)

(add-to-list 'custom-theme-load-path ts/theme-directory)

(fset 'yes-or-no-p 'y-or-n-p)

(setq mac-command-modifier 'meta
      mac-option-modifier  'alt
      mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll t
      ns-alternate-modifier 'none)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq-default indent-tabs-mode nil
              show-trailing-whitespace t
              tab-width 4)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq display-line-numbers-type 'relative
      display-time-24hr-format t
      custom-safe-themes t
      show-paren-when-point-inside-paren t)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)

;; (global-display-line-numbers-mode)
;; (global-hl-line-mode t)
(show-paren-mode t)
(electric-pair-mode t)
(electric-indent-mode t)
;; (display-time-mode t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)

  :config
  (evil-mode 1)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")

    (evil-leader/set-key
      "TAB"    'ts/alternate-buffer
      "'"      'shell-pop
      "q"      'ts/kill-window-or-buffer
      "h k"    'describe-key
      "h v"    'describe-variable
      "f f"    'helm-find-files
      "f r"    'helm-recentf
      "r"      'helm-recentf
      "b"      'helm-mini
      "p p"    'helm-projectile-switch-project
      "p f"    'helm-projectile-find-file
      "p r"    'helm-projectile-recentf
      "v"      'ts/edit-configuration
      "u"      'ts/load-configuration
      "e"      'neotree-toggle))

  ;;(use-package evil-collection
  ;;  :ensure t
  ;;  :defer t
  ;;  :config
  ;;  (evil-collection-init))

  (define-key evil-normal-state-map (kbd "M-s") #'save-buffer)
  (define-key evil-normal-state-map (kbd "M-a") #'mark-whole-buffer)
  (define-key evil-normal-state-map (kbd "M-q") #'evil-quit-all)
  (define-key evil-normal-state-map (kbd "M-c") #'evil-yank)
  (define-key evil-normal-state-map (kbd "M-v") #'clipboard-yank)
  (define-key evil-insert-state-map (kbd "M-v") #'clipboard-yank)
  (define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)
  (define-key evil-normal-state-map (kbd "u")   #'undo-tree-undo)
  (define-key evil-normal-state-map (kbd "C-r") #'undo-tree-redo)
  (define-key evil-normal-state-map (kbd "g f") #'helm-projectile-find-file-dwim)
  (define-key evil-normal-state-map (kbd "g c") #'comment-line)

  ;; Ensure ESC quits in all modes: http://stackoverflow.com/a/10166400/61435
  (global-set-key [escape] 'evil-exit-emacs-state)
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'ts/minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'ts/minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'ts/minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'ts/minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'ts/minibuffer-keyboard-quit)

  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "o") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "c") 'neotree-create-node)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter))

(use-package diminish
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode)
  (add-hook 'js2-mode-hook 'rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
;; (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'js2-mode-hook 'rainbow-delimiters-mode))

(use-package all-the-icons
  :ensure t)

;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t)
;; (load-theme 'sanityinc-tomorrow-eighties)
;; (load-theme 'ts-overrides)

(use-package oceanic-theme
  :ensure t)
(load-theme 'oceanic)
(load-theme 'ts-overrides)

;; (use-package dracula-theme
;;   :ensure t)
;; (load-theme 'dracula)
;; (load-theme 'dracula-overrides)

;; (use-package doom-themes
;;   :ensure t)
;; (load-theme 'doom-one)

(use-package neotree
  :ensure t
  :init
  (setq neo-smart-open t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("M-P" . helm-M-x)
         ("M-p" . helm-find-files)
         ("M-e" . helm-recentf))
  :init
  (setq helm-buffers-fuzzy-matching t
        helm-autoresize-mode t
        helm-split-window-inside-p t
        helm-display-buffer-default-height 15)

  :config
  (helm-mode 1)

  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-h") 'helm-next-source)
  (define-key helm-map (kbd "C-S-h") 'describe-key)
  (define-key helm-map (kbd "C-l") (kbd "RET"))
  (define-key helm-map [escape] 'helm-keyboard-quit)
  (dolist (keymap (list helm-find-files-map helm-read-file-map))
    (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
    (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
    (define-key keymap (kbd "C-S-h") 'describe-key))

  (add-hook 'helm-after-initialize-hook 'ts/hide-cursor-in-helm-buffer))

(use-package helm-ag
  :ensure t)

(use-package helm-projectile
  :ensure t)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t)

  :config
  (projectile-global-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-declare-prefixes ", h" "help")
  (which-key-declare-prefixes ", p" "project")
  (which-key-declare-prefixes ", f" "files")
  (which-key-declare-prefixes ", b" "buffers"))

(use-package powerline
  :ensure t
  :init
  (setq powerline-image-apple-rgb t)

  :config
  ;;  (use-package telephone-line
  ;;    :ensure t
  ;;    :init
  ;;    (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
  ;;          telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
  ;;          telephone-line-primary-right-separator 'telephone-line-cubed-right
  ;;          telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  ;;    (setq telephone-line-height 24
  ;;          telephone-line-evil-use-short-tag t)
  ;;    :config
  ;;    (telephone-line-mode t)))
  )

(use-package spaceline
  :ensure t
  :after powerline)
;; :init
;; (setq-default
;;  powerline-height 20
;;  powerline-default-separator 'wave
;;  spaceline-flycheck-bullet "❖ %s"
;;  spaceline-separator-dir-left '(right . right)
;;  spaceline-separator-dir-right '(left . left)))

(use-package spaceline-config
  :ensure spaceline
  :after spaceline
  :init
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  :config
  ;; (spaceline-spacemacs-theme)
  (spaceline-helm-mode 1))
;; (spaceline-toggle-minor-modes-off))

(use-package spaceline-all-the-icons
  :after spaceline
  :init
  (setq spaceline-all-the-icons-highlight-file-name t
        spaceline-all-the-icons-separator-type 'cup)
  :config
  (spaceline-all-the-icons-theme)
  (spaceline-toggle-all-the-icons-buffer-position-on)
  (spaceline-toggle-all-the-icons-region-info-on)
  )

(use-package company
  :ensure t
  :diminish company-mode
  :config
  ;;  (setq company-idle-delay nil)

  (global-company-mode +1)

  (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-f") 'company-next-page)
  (define-key company-active-map (kbd "C-b") 'company-previous-page))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package rjsx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-hook 'rjsx-mode-hook (lambda () (setq js2-strict-missing-semi-warning nil))))

(use-package js2-refactor
  :ensure t
  :config
  (add-hook 'rjsx-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package expand-region
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "C-0") #'er/expand-region))

(use-package shell-pop
  :ensure t
  :init
  (setq shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell shell-pop-term-shell))))))

(use-package eshell-git-prompt
  :ensure t
  :config
  (eshell-git-prompt-use-theme 'powerline))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(markdown-mode company spaceline powerline which-key helm-projectile helm-ag helm neotree oceanic-theme all-the-icons rainbow-delimiters rainbow-mode evil-leader evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
