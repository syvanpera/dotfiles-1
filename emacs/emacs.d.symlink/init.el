;;; init.el --- My magnificent Emacs configuration

;;; Code:

;; Unclutter the interface immediately
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(when (fboundp 'mouse-wheel-mode) (mouse-wheel-mode 1))

(setq gc-cons-threshold 64000000)
;; restore after start-up
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

(setq warning-minimum-level :emergency)

(require 'package)

(setq package-enable-at-startup nil
      use-package-always-ensure t
      package-archives
      '(("melpa"        . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("org"          . "http://orgmode.org/elpa/")
        ("marmalade"    . "http://marmalade-repo.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp/vendor" user-emacs-directory))

(require 'sensible-defaults)
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

(setq user-full-name "Tuomo Syvänperä"
      user-mail-address "tuomo.syvanpera@gmail.com")

(require 'ts-funcs)

(defconst ts/theme-directory (concat user-emacs-directory "themes"))

(setq make-backup-files nil
      backup-by-copying t
      version-control nil
      delete-old-versions t
      delete-by-moving-to-trash nil
      create-lockfiles nil
      kept-old-versions 1
      kept-new-versions 1
      ediff-window-setup-function 'ediff-setup-windows-plain
      help-window-select t
      eshell-scroll-to-bottom-on-input t
      save-interprogram-paste-before-kill t
      x-select-enable-clipboard nil
      save-place-mode t
      recentf-mode nil)

(setq inhibit-startup-echo-area-message user-login-name
      ring-bell-function 'ignore)

(add-to-list 'custom-theme-load-path ts/theme-directory)

(setq mac-command-modifier 'meta
      mac-option-modifier  'alt
      mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll t
      ns-alternate-modifier 'none)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq-default indent-tabs-mode nil
              standard-indent 2
              tab-width 2
              indent-tabs-mode nil
              js-indent-level 2
              js2-basic-offset 2
              web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-indent-style 2)

(setq display-line-numbers-type 'relative
      display-time-24hr-format t
      custom-safe-themes t
      show-paren-when-point-inside-paren t
      prettify-symbols-unprettify-at-point t
      default-frame-alist '((font . "Hack-12")))

(add-hook 'prog-mode-hook (lambda () (setq-local show-trailing-whitespace t)))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; (add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)

;; (global-display-line-numbers-mode)
;; (global-hl-line-mode t)
(electric-pair-mode t)
(electric-indent-mode t)
(global-prettify-symbols-mode t)

(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

(with-current-buffer "*Messages*"
  (emacs-lock-mode 'kill))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq show-paren-style 'parenthesis)
                                  (push '("<=" . ?≤) prettify-symbols-alist)
                                  (push '(">=" . ?≥) prettify-symbols-alist)
                                  (push '("==" . ?≡) prettify-symbols-alist)
                                  (push '("===" . ?≣) prettify-symbols-alist)
                                  (push '("!=" . ?≠) prettify-symbols-alist)
                                  (push '("!==" . ?≢) prettify-symbols-alist)
                                  (push '("->" . ?→) prettify-symbols-alist)
                                  (push '("<-" . ?←) prettify-symbols-alist)
                                  (push '("=>" . ?⇒) prettify-symbols-alist)))

(defun display-startup-echo-area-message ()
  "Startup message."
  (message "Another Visitor! Stay awhile! Stay FOREVER!!!!!!!!!!!!"))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package evil
  :init
  (setq evil-want-integration nil)

  :config
  (evil-mode t)

  ;; Some commands are just not meant to be repeated
  (mapc 'evil-declare-not-repeat
        '(undo-tree-undo
          undo-tree-redo))

  (add-to-list 'evil-normal-state-modes 'Custom-mode)

  ;; Ensure ESC quits in all modes: http://stackoverflow.com/a/10166400/61435
  (global-set-key [escape] 'evil-exit-emacs-state)
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'ts/minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'ts/minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'ts/minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'ts/minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'ts/minibuffer-keyboard-quit)

  (define-key evil-normal-state-map   (kbd "C-g") 'ts/evil-keyboard-quit)
  (define-key evil-motion-state-map   (kbd "C-g") 'ts/evil-keyboard-quit)
  (define-key evil-insert-state-map   (kbd "C-g") 'ts/evil-keyboard-quit)
  (define-key evil-window-map         (kbd "C-g") 'ts/evil-keyboard-quit)
  (define-key evil-operator-state-map (kbd "C-g") 'ts/evil-keyboard-quit)

  (define-key evil-normal-state-map (kbd "M-s") 'save-buffer)
  (define-key evil-normal-state-map (kbd "M-a") 'mark-whole-buffer)
  (define-key evil-normal-state-map (kbd "M-q") 'evil-quit-all)
  (define-key evil-normal-state-map (kbd "M-c") 'evil-yank)
  (define-key evil-normal-state-map (kbd "M-v") 'clipboard-yank)
  (define-key evil-insert-state-map (kbd "M-v") 'clipboard-yank)
  (define-key evil-normal-state-map (kbd "M-y") 'helm-show-kill-ring)
  (define-key evil-insert-state-map (kbd "M-y") 'helm-show-kill-ring)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "u")   'undo-tree-undo)
  (define-key evil-normal-state-map (kbd "M-u") 'undo-tree-visualize)
  (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
  (define-key evil-normal-state-map (kbd "gf") 'projectile-find-file-dwim)
  (define-key evil-normal-state-map (kbd "gF") 'projectile-find-file-dwim-other-window)
  (define-key evil-normal-state-map (kbd "gc") 'sensible-defaults/comment-or-uncomment-region-or-line)
  (define-key evil-normal-state-map (kbd "M-√") (lambda () (interactive) (scroll-other-window 1)))
  (define-key evil-normal-state-map (kbd "M-ª") (lambda () (interactive) (scroll-other-window-down 1)))
  (define-key evil-normal-state-map (kbd "M-ƒ") 'scroll-other-window)
  (define-key evil-normal-state-map (kbd "M-›") 'scroll-other-window-down)
  (define-key evil-normal-state-map (kbd "M-k") 'move-line-up)
  (define-key evil-normal-state-map (kbd "M-j") 'move-line-down)

  (evil-define-key 'normal org-mode-map (kbd "M-h") 'org-metaleft)
  (evil-define-key 'normal org-mode-map (kbd "M-l") 'org-metaright)
  (evil-define-key 'normal org-mode-map (kbd "M-k") 'org-metaup)
  (evil-define-key 'normal org-mode-map (kbd "M-j") 'org-metadown)
  (evil-define-key 'normal org-mode-map (kbd "gh") 'org-backward-element)
  (evil-define-key 'normal org-mode-map (kbd "gl") 'org-forward-element)
  (evil-define-key 'normal org-mode-map (kbd "gk") 'org-up-element)
  (evil-define-key 'normal org-mode-map (kbd "gj") 'org-down-element)
  (evil-define-key 'normal org-mode-map (kbd "H")   'org-shiftleft)
  (evil-define-key 'normal org-mode-map (kbd "L")   'org-shiftright)

  (evil-define-key 'normal magit-status-mode-map (kbd "M-n") 'magit-section-forward-sibling)
  (evil-define-key 'normal magit-status-mode-map (kbd "M-p") 'magit-section-backward-sibling)
  (evil-define-key 'normal magit-status-mode-map (kbd "j")   'magit-section-forward)
  (evil-define-key 'normal magit-status-mode-map (kbd "k")   'magit-section-backward)

  (evil-define-key 'normal paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)

  (evil-define-key 'normal js-mode-map (kbd "M-k") 'js2r-move-line-up)
  (evil-define-key 'normal js-mode-map (kbd "M-j") 'js2r-move-line-down)

  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "o")   'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "O")   'neotree-enter-horizontal-split)
  (evil-define-key 'normal neotree-mode-map (kbd "q")   'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "c")   'neotree-create-node)
  (evil-define-key 'normal neotree-mode-map (kbd "r")   'neotree-rename-node)
  (evil-define-key 'normal neotree-mode-map (kbd "d")   'neotree-delete-node)
  (evil-define-key 'normal neotree-mode-map (kbd "H")   'neotree-hidden-file-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "z")   'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "R")   'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)

  (evil-define-key 'motion undo-tree-visualizer-mode-map (kbd "j")   'undo-tree-visualize-redo)
  (evil-define-key 'motion undo-tree-visualizer-mode-map (kbd "k")   'undo-tree-visualize-undo)
  (evil-define-key 'motion undo-tree-visualizer-mode-map (kbd "l")   'undo-tree-visualize-switch-branch-right)
  (evil-define-key 'motion undo-tree-visualizer-mode-map (kbd "h")   'undo-tree-visualize-switch-branch-left)
  (evil-define-key 'motion undo-tree-visualizer-mode-map (kbd "C-h") 'evil-window-left)
  (evil-define-key 'motion undo-tree-visualizer-mode-map (kbd "C-j") 'evil-window-down)
  (evil-define-key 'motion undo-tree-visualizer-mode-map (kbd "C-k") 'evil-window-up)
  (evil-define-key 'motion undo-tree-visualizer-mode-map (kbd "C-l") 'evil-window-right)

  (evil-define-key 'normal flycheck-error-list-mode-map (kbd "j")   'flycheck-error-list-next-error)
  (evil-define-key 'normal flycheck-error-list-mode-map (kbd "k")   'flycheck-error-list-previous-error)
  (evil-define-key 'normal flycheck-error-list-mode-map (kbd "q")   'kill-buffer-and-window)
  (evil-define-key 'normal flycheck-error-list-mode-map (kbd "RET") 'flycheck-error-list-goto-error)

  (evil-define-key 'normal helm-ag-mode-map (kbd "o")   'helm-ag-mode-jump)
  (evil-define-key 'normal helm-ag-mode-map (kbd "O")   'helm-ag-mode-jump-other-window)

  (evil-define-key 'normal custom-mode-map (kbd "q")   'Custom-buffer-done)

  (evil-define-key 'normal eshell-mode-map (kbd "i")   'ts/eshell-evil-input-mode)
  (evil-define-key 'normal eshell-mode-map (kbd "q")   'shell-pop)
  (evil-define-key 'insert eshell-mode-map (kbd "C-a") 'eshell-bol)
  (evil-define-key 'insert eshell-mode-map (kbd "C-p") 'eshell-previous-matching-input-from-input)
  (evil-define-key 'insert eshell-mode-map (kbd "C-n") 'eshell-next-matching-input-from-input)
  (evil-define-key 'insert eshell-mode-map (kbd "C-r") 'helm-eshell-history))

(use-package evil-leader
  :after evil
  :init
  (setq evil-leader/in-all-states t)

  :config
  (evil-leader/set-leader ",")
  (evil-mode nil)
  (global-evil-leader-mode)
  (evil-mode t)

  (evil-leader/set-key
    "TAB"   'ts/alternate-buffer
    "'"     'shell-pop
    "s"     'ts/contextual-shell-pop
    "q"     'ts/kill-window-or-buffer
    "hk"    'describe-key
    "hv"    'describe-variable
    "hf"    'describe-function
    "hw"    'where-is
    "ff"    'ts/contextual-helm-find-files
    "fo"    'ts/helm-find-org-files
    "fs"    'ts/open-create-scratch-buffer
    "fr"    'helm-recentf
    "xl"    'flycheck-list-errors
    "xv"    'flycheck-verify-setup
    "xn"    'next-error
    "xp"    'previous-error
    "r"     'ts/contextual-helm-recentf
    "b"     'helm-mini
    "pp"    'helm-projectile-switch-project
    "pf"    'ts/contextual-helm-find-files
    "pr"    'projectile-run-project
    "pt"    'projectile-test-project
    "ps"    'ts/projectile-shell-pop
    "po"    (lambda () (interactive) (ts/load-project-org "veikkaus"))
    "v"     'ts/edit-configuration
    "u"     'ts/load-configuration
    "e"     'ts/contextual-neotree-toggle
    "gs"    'magit-status
    "gl"    'magit-log-current
    "gb"    'magit-blame
    "ol"    'org-store-link
    "of"    'ts/helm-find-org-files
    "oa"    'ts/org-agenda-show-agenda-and-todo
    "oc"    'org-capture
    "op"    (lambda () (interactive) (ts/load-project-org "veikkaus"))
    "ob"    'org-switchb))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package rainbow-mode
  :hook ((emacs-lisp-mode css-mode html-mode js-mode rjsx-mode) . rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons)

;; (use-package color-theme-sanityinc-tomorrow
;;   :config
;;   (color-theme-sanityinc-tomorrow-eighties)
;;   (load-theme 'tomorrow-overrides))

;; (use-package oceanic-theme
;;   :config
;;   (load-theme 'oceanic)
;;   (load-theme 'oceanic-overrides))

(load-theme 'misterioso)
(load-theme 'misterioso-overrides)

;; (use-package doom-themes
;;   :init
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t)

;;   :config
;;   (load-theme 'doom-one t)
;;   (doom-themes-visual-bell-config)
;;   (doom-themes-neotree-config)
;;   (doom-themes-org-config))

(use-package neotree
  :init
  (setq neo-smart-open t
        ;; projectile-switch-project-action 'neotree-projectile-action
        neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("M-P" . helm-M-x)
         ("M-r" . helm-recentf)
         :map helm-map
         ("C-j"   . helm-next-line)
         ("C-k"   . helm-previous-line)
         ("C-f"   . helm-next-page)
         ("C-b"   . helm-previous-page)
         ("C-h"   . helm-next-source)
         ("C-v"   . helm-toggle-visible-mark)
         ("C-p"   . helm-copy-to-buffer)
         ("C-S-h" . describe-key)
         ("C-l"   . helm-confirm-and-exit-minibuffer)
         ("ESC"   . helm-keyboard-quit)
         :map helm-find-files-map
         ("C-l"   . helm-execute-persistent-action)
         ("C-h"   . helm-find-files-up-one-level)
         ("C-S-h" . describe-key)
         :map helm-read-file-map
         ("C-l"   . helm-execute-persistent-action)
         ("C-h"   . helm-find-files-up-one-level)
         ("C-S-h" . describe-key))
  :init
  (setq helm-buffers-fuzzy-matching t
        helm-autoresize-mode t
        helm-split-window-inside-p t
        helm-commands-using-frame nil
        helm-display-buffer-default-height 15)

  :config
  (helm-mode add)

  (t-hook 'helm-after-initialize-hook 'ts/hide-cursor-in-helm-buffer))

(use-package helm-ag
  :bind ("∫" . ts/contextual-helm-ag)
  :init
  (setq helm-ag-base-command "ag --nocolor --nogroup"
        ;; helm-ag-command-option "-C2"
        helm-ag-insert-at-point 'symbol))

(use-package helm-projectile
  :init
  (setq helm-projectile-fuzzy-match t))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package projectile
  :init
  (setq projectile-enable-caching t)

  :config
  (projectile-global-mode))

(use-package which-key
  :config
  (which-key-mode)
  (which-key-declare-prefixes ", h" "help")
  (which-key-declare-prefixes ", p" "project")
  (which-key-declare-prefixes ", f" "files")
  (which-key-declare-prefixes ", b" "buffers")
  (which-key-declare-prefixes ", g" "git")
  (which-key-declare-prefixes ", o" "org")
  (which-key-declare-prefixes ", x" "errors")
  (which-key-declare-prefixes ", t" "toggle"))

(use-package powerline
  :init
  (setq powerline-image-apple-rgb t))

(use-package spaceline
  :after powerline)

(use-package spaceline-config
  :ensure spaceline
  :after spaceline
  :init
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  :config
  (spaceline-helm-mode t))

(use-package spaceline-all-the-icons
  :after spaceline
  :init
  (setq spaceline-all-the-icons-highlight-file-name t
        spaceline-all-the-icons-separator-type 'cup)
  :config
  (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-git-ahead)
  (spaceline-toggle-all-the-icons-buffer-position-on)
  (spaceline-toggle-all-the-icons-region-info-on))

(use-package company
  :config
  (global-company-mode t)

  (define-key company-active-map [tab]       'company-complete)
  (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-f") 'company-next-page)
  (define-key company-active-map (kbd "C-b") 'company-previous-page))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package rjsx-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-hook 'js-mode-hook (lambda () (setq js2-strict-missing-semi-warning nil))))

(use-package js2-refactor
  :hook (rjsx-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package expand-region
  :config
  (define-key evil-normal-state-map (kbd "M-+") #'er/expand-region)
  (define-key evil-normal-state-map (kbd "M--") #'er/contract-region))

(use-package shell-pop
  :init
  (setq shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell shell-pop-term-shell))))))

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package eshell-z)

(use-package magit
  :init
  (setq git-commit-summary-max-length 50)
  :config
  (magit-define-popup-switch 'magit-pull-popup ?r "Rebase" "--rebase"))

(use-package evil-magit
  :after magit)

(use-package git-timemachine
  :config
  (eval-after-load 'git-timemachine
    '(progn
       (evil-make-overriding-map git-timemachine-mode-map 'normal)
       ;; force update evil keymaps after git-timemachine-mode loaded
       (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))))

(use-package yasnippet
  :config
  (yas-global-mode t))

(use-package org
  :config
  (setq org-directory "~/Documents/org"
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-ellipsis "…"
        org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
                            (sequence "⚑ WAITING(w@)" "|")
                            (sequence "|" "✘ CANCELED(c@)"))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "◎" "⚫" "○" "►" "◇")))

(use-package htmlize)

(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook                  'enable-paredit-mode)
  (add-hook 'lisp-mode-hook                        'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook                        'enable-paredit-mode)
  (add-hook 'lisp-mode-hook                        'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook            'enable-paredit-mode)
  (add-hook 'scheme-mode-hook                      'enable-paredit-mode)
  (add-hook 'js-mode-hook                          'ts/paredit-nonlisp))

(use-package evil-paredit)

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package flycheck
  :init
  :config
  (define-fringe-bitmap 'my-flycheck-fringe-indicator
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00011100
            #b00111110
            #b00111110
            #b00111110
            #b00011100
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))

  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-info)

  (global-flycheck-mode))

(use-package coffee-mode
  :init
  (setq coffee-tab-width 2))

(use-package engine-mode
  :config
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")

  (engine-mode t))

(use-package highlight-indent-guides
  :hook (coffee-mode . highlight-indent-guides-mode)
  :config (setq-default highlight-indent-guides-method 'character))

;; (use-package evil-org
;;   :after (evil org)
;;   :config
;;   (add-hook 'org-mode-hook 'evil-org-mode)
;;   (add-hook 'evil-org-mode-hook
;;             (lambda ()
;;               (evil-org-set-key-theme (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading)))))
;;   (require 'evil-org-agenda)
;;   (evil-org-agenda-set-keys))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(coffee-mode highlight-indent-guides engine-mode htmlize flycheck git-timemachine eshell-z markdown-mode company spaceline powerline which-key helm-projectile helm-ag helm neotree oceanic-theme all-the-icons rainbow-delimiters rainbow-mode evil-leader evil use-package))
 '(safe-local-variable-values
   '((projectile-project-run-cmd . "yarn start")
     (projectile-project-run-cmd . "y start")
     (projectile-project-test-cmd . "curl -s -i -X POST -u \"exthousyvtu:74c5eb9f9788478a2d64efbb4e6e43c4\" \"http://makemv01t.tst.veikkaus.fi:8080/job/web-test-revision/buildWithParameters?delay=0sec&revision=$(git rev-parse --symbolic --abbrev-ref HEAD)\"")
     (projectile-project-run-cmd . "BUILD_SPEC=0 ./gulp --buildPages")
     (projectile-project-test-cmd . "curl -i -X POST -u \"exthousyvtu:74c5eb9f9788478a2d64efbb4e6e43c4\" \"http://makemv01t.tst.veikkaus.fi:8080/job/web-test-revision/buildWithParameters?delay=0sec&revision=$(git rev-parse --symbolic --abbrev-ref HEAD)\"")
     (projectile-project-run-cmd . "echo Run")
     (projectile-project-compilation-cmd . "echo Compile"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)

;;; init.el ends here
