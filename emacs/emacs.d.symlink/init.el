;;; init.el --- My magnificent Emacs configuration

;;; Code:

;; Unclutter the interface immediately
;; (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(when (fboundp 'mouse-wheel-mode) (mouse-wheel-mode 1))

(setq gc-cons-threshold 64000000)
;; restore after start-up
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
;; (setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(setq warning-minimum-level :emergency)

(require 'package)

(setq package-enable-at-startup nil
      use-package-always-ensure t
      package-archives
      '(("melpa"        . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("gnu"          . "http://elpa.gnu.org/packages/")
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
      align-default-spacing 0
      recentf-mode nil
      compilation-window-height 30)

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
      show-paren-mode t
      prettify-symbols-unprettify-at-point t
      superword-mode t
      default-frame-alist '((font . "Cousine-12")))

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

(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.3)))

(defun display-startup-echo-area-message ()
  "Startup message."
  (message "Another Visitor! Stay awhile! Stay FOREVER!!!!!!!!!!!!"))

(use-package dashboard
  :init
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5))
        dashboard-startup-banner 'official)
  :config
  (dashboard-setup-startup-hook))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package evil
  :init
  (setq evil-want-integration nil)

  :config
  (progn
    (use-package evil-leader
      :config
      (evil-leader/set-leader ",")
      (global-evil-leader-mode 1)

      (evil-leader/set-key
        "TAB"   'ts/alternate-buffer
        "'"     'shell-pop
        "s"     'ts/contextual-shell-pop
        "q"     'ts/kill-window-or-buffer
        "tl"    'display-line-numbers-mode
        "tr"    'ts/toggle-relative-line-numbers
        "jw"    'avy-goto-word-1
        "jc"    'avy-goto-char
        "jl"    'avy-goto-line
        "hk"    'describe-key
        "hv"    'describe-variable
        "hf"    'describe-function
        "hw"    'where-is
        "fe"    'neotree-toggle
        "ff"    'helm-find-files
        "fo"    'ts/helm-find-org-files
        "fs"    'ts/open-create-scratch-buffer
        "fr"    'helm-recentf
        "xx"    'flycheck-mode
        "xl"    'flycheck-list-errors
        "xv"    'flycheck-verify-setup
        "xn"    'next-error
        "xp"    'previous-error
        "r"     'ts/contextual-helm-recentf
        "bb"    'helm-buffers-list
        "bk"    'kill-buffer
        "br"    'rename-buffer
        "pp"    'helm-projectile-switch-project
        "pf"    'ts/contextual-helm-find-files
        "pr"    'projectile-run-project
        "pt"    'projectile-test-project
        "ps"    'ts/projectile-shell-pop
        "po"    (lambda () (interactive) (ts/load-project-org "veikkaus"))
        "v"     'ts/edit-configuration
        "u"     'ts/load-configuration
        "e"     'ts/contextual-neotree-toggle
        ;; "e"     'ts/contextual-treemacs-toggle
        "gs"    'magit-status
        "gl"    'magit-log-current
        "gb"    'magit-blame
        "gn"    'git-gutter+-next-hunk
        "gp"    'git-gutter+-previous-hunk
        "gv"    'git-gutter+-show-hunk
        "gr"    'git-gutter+-revert-hunk
        "of"    'ts/helm-find-org-files
        "oa"    'ts/org-agenda-show-agenda-and-todo
        "oc"    'org-capture
        "oi"    'ts/open-org-inbox
        "oh"    'helm-org-agenda-files-headings
        "or"    'org-refile
        "op"    (lambda () (interactive) (ts/load-project-org "veikkaus"))
        "ob"    'org-switchb))

    (evil-mode t)

    ;; Some commands are just not meant to be repeated
    (mapc 'evil-declare-not-repeat '(undo-tree-undo undo-tree-redo))

    (add-to-list 'evil-normal-state-modes 'Custom-mode)

    (define-key global-map "\C-cy"        'clipboard-yank)
    (define-key global-map "\C-cs"        'ts/contextual-shell-pop)

    (define-key global-map (kbd "M-v")         'clipboard-yank)
    (define-key global-map (kbd "M-S-<left>")  'shrink-window-horizontally)
    (define-key global-map (kbd "M-S-<right>") 'enlarge-window-horizontally)
    (define-key global-map (kbd "M-S-<down>")  'shrink-window)
    (define-key global-map (kbd "M-S-<up>")    'enlarge-window)

    ;; Ensure ESC quits in all modes: http://stackoverflow.com/a/10166400/61435
    (global-set-key [escape]                             'evil-exit-emacs-state)
    (define-key evil-normal-state-map [escape]           'keyboard-quit)
    (define-key evil-visual-state-map [escape]           'keyboard-quit)
    (define-key minibuffer-local-map [escape]            'ts/minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape]         'ts/minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'ts/minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'ts/minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape]    'ts/minibuffer-keyboard-quit)

    (define-key evil-normal-state-map   (kbd "C-g") 'ts/evil-keyboard-quit)
    (define-key evil-motion-state-map   (kbd "C-g") 'ts/evil-keyboard-quit)
    (define-key evil-insert-state-map   (kbd "C-g") 'ts/evil-keyboard-quit)
    (define-key evil-window-map         (kbd "C-g") 'ts/evil-keyboard-quit)
    (define-key evil-operator-state-map (kbd "C-g") 'ts/evil-keyboard-quit)

    (define-key evil-normal-state-map (kbd "C-s") 'save-buffer)
    (define-key evil-normal-state-map (kbd "M-s") 'save-buffer)
    (define-key evil-normal-state-map (kbd "M-a") 'mark-whole-buffer)
    (define-key evil-normal-state-map (kbd "M-q") 'evil-quit-all)
    (define-key evil-normal-state-map (kbd "M-c") 'evil-yank)
    ;; (define-key evil-normal-state-map (kbd "M-v") 'clipboard-yank)
    ;; (define-key evil-insert-state-map (kbd "M-v") 'clipboard-yank)
    (define-key evil-normal-state-map (kbd "M-y") 'helm-show-kill-ring)
    (define-key evil-insert-state-map (kbd "M-y") 'helm-show-kill-ring)
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
    (define-key evil-normal-state-map (kbd "u")   'undo-tree-undo)
    (define-key evil-normal-state-map (kbd "M-u") 'undo-tree-visualize)
    (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
    (define-key evil-normal-state-map (kbd "go")  'find-file-at-point)
    (define-key evil-normal-state-map (kbd "gf")  'projectile-find-file-dwim)
    (define-key evil-normal-state-map (kbd "gF")  'projectile-find-file-dwim-other-window)
    (define-key evil-normal-state-map (kbd "gc")  'sensible-defaults/comment-or-uncomment-region-or-line)
    (define-key evil-normal-state-map (kbd "M-√") (lambda () (interactive) (scroll-other-window 1)))
    (define-key evil-normal-state-map (kbd "M-ª") (lambda () (interactive) (scroll-other-window-down 1)))
    (define-key evil-normal-state-map (kbd "M-ƒ") 'scroll-other-window)
    (define-key evil-normal-state-map (kbd "M-›") 'scroll-other-window-down)
    (define-key evil-normal-state-map (kbd "M-k") 'move-line-up)
    (define-key evil-normal-state-map (kbd "M-j") 'move-line-down)

    ;; (define-key compilation-mode-map (kbd "C-h") 'evil-window-left)
    ;; (define-key compilation-mode-map (kbd "C-h") 'evil-window-left)
    ;; (define-key compilation-mode-map (kbd "C-l") 'evil-window-right)
    ;; (define-key compilation-mode-map (kbd "C-k") 'evil-window-up)
    ;; (define-key compilation-mode-map (kbd "C-j") 'evil-window-down)

    (evil-define-key 'normal eww-mode-map (kbd "q") 'quit-window)
    (evil-define-key 'normal diff-mode-map (kbd "q") 'quit-window)

    (evil-define-key 'normal dashboard-mode-map (kbd "C-k") 'widget-backward)

    ;; (evil-define-key 'normal paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)

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

    ;; (evil-define-key 'normal evil-treemacs-state-map (kbd "C-l") 'evil-window-right)

    (evil-define-key 'motion undo-tree-visualizer-mode-map (kbd "j")   'undo-tree-visualize-redo)
    (evil-define-key 'motion undo-tree-visualizer-mode-map (kbd "k")   'undo-tree-visualize-undo)
    (evil-define-key 'motion undo-tree-visualizer-mode-map (kbd "l")   'undo-tree-visualize-switch-branch-right)
    (evil-define-key 'motion undo-tree-visualizer-mode-map (kbd "h")   'undo-tree-visualize-switch-branch-left)
    (evil-define-key 'motion undo-tree-visualizer-mode-map (kbd "C-h") 'evil-window-left)
    (evil-define-key 'motion undo-tree-visualizer-mode-map (kbd "C-j") 'evil-window-down)
    (evil-define-key 'motion undo-tree-visualizer-mode-map (kbd "C-k") 'evil-window-up)
    (evil-define-key 'motion undo-tree-visualizer-mode-map (kbd "C-l") 'evil-window-right)

    (evil-define-key 'motion help-mode-map (kbd "C-h") 'evil-window-left)
    (evil-define-key 'motion help-mode-map (kbd "C-j") 'evil-window-down)
    (evil-define-key 'motion help-mode-map (kbd "C-k") 'evil-window-up)
    (evil-define-key 'motion help-mode-map (kbd "C-l") 'evil-window-right)

    (evil-define-key 'normal dashboard-mode-map (kbd "j") 'widget-forward)
    (evil-define-key 'normal dashboard-mode-map (kbd "k") 'widget-backward)

    (evil-define-key 'normal flycheck-error-list-mode-map (kbd "j")   'flycheck-error-list-next-error)
    (evil-define-key 'normal flycheck-error-list-mode-map (kbd "k")   'flycheck-error-list-previous-error)
    (evil-define-key 'normal flycheck-error-list-mode-map (kbd "q")   'kill-buffer-and-window)
    (evil-define-key 'normal flycheck-error-list-mode-map (kbd "RET") 'flycheck-error-list-goto-error)

    (evil-define-key 'normal helm-ag-mode-map (kbd "o") 'helm-ag-mode-jump)
    (evil-define-key 'normal helm-ag-mode-map (kbd "O") 'helm-ag-mode-jump-other-window)

    (evil-define-key 'normal custom-mode-map (kbd "q") 'Custom-buffer-done)

    (evil-define-key 'motion tide-mode-map
      (kbd "gd")  'tide-jump-to-definition
      (kbd "C-o") 'tide-jump-back
      (kbd "C-t") 'tide-jump-back
      (kbd "K")   'tide-documentation-at-point)

    (evil-define-key 'normal tide-references-mode-map
      (kbd "gj")       'tide-find-next-reference
      (kbd "gk")       'tide-find-previous-reference
      (kbd "C-j")      'tide-find-next-reference
      (kbd "C-k")      'tide-find-previous-reference
      (kbd "C-l")      'tide-goto-reference
      (kbd "<return>") 'tide-goto-reference
      (kbd "q")        'quit-window)

    (evil-define-key 'normal tide-project-errors-mode-map
      (kbd "gj")       'tide-find-next-error
      (kbd "gk")       'tide-find-previous-error
      (kbd "C-j")      'tide-find-next-error
      (kbd "C-k")      'tide-find-previous-error
      (kbd "C-l")      'tide-goto-error
      (kbd "<return>") 'tide-goto-error
      (kbd "q")        'quit-window)

    (evil-define-key 'normal eshell-mode-map (kbd "i")   'ts/eshell-evil-input-mode)
    (evil-define-key 'normal eshell-mode-map (kbd "q")   'shell-pop)
    (evil-define-key 'insert eshell-mode-map (kbd "C-a") 'eshell-bol)
    (evil-define-key 'insert eshell-mode-map (kbd "C-p") 'eshell-previous-matching-input-from-input)
    (evil-define-key 'insert eshell-mode-map (kbd "C-n") 'eshell-next-matching-input-from-input)
    (evil-define-key 'insert eshell-mode-map (kbd "C-r") 'helm-eshell-history)))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package rainbow-mode
  :defer t
  :hook ((emacs-lisp-mode css-mode html-mode js-mode rjsx-mode) . rainbow-mode))

(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons
  :defer t)

;; (use-package color-theme-sanityinc-tomorrow
;;   :config
;;   (progn
;;     (color-theme-sanityinc-tomorrow-eighties)
;;     (load-theme 'tomorrow-overrides)))

;; (use-package oceanic-theme
;;   :config
;;   (progn
;;     (load-theme 'oceanic)
;;     (load-theme 'oceanic-overrides)))

;; (load-theme 'misterioso)
;; (load-theme 'misterioso-overrides)

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-vibrant-brighter-modeline t)

  :config
  (progn
    (load-theme 'doom-vibrant t)
    (doom-themes-visual-bell-config)
    (doom-themes-neotree-config)
    (doom-themes-org-config)
    (load-theme 'doom-overrides)))

(use-package neotree
  :defer t
  :init
  (setq neo-smart-open t
        neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :config
;;   (progn
;;     (use-package treemacs-evil
;;       :ensure t
;;       :demand t)
;;     (setq treemacs-change-root-without-asking nil
;;           treemacs-collapse-dirs              (if (executable-find "python") 3 0)
;;           treemacs-file-event-delay           5000
;;           treemacs-follow-after-init          t
;;           treemacs-follow-recenter-distance   0.1
;;           treemacs-goto-tag-strategy          'refetch-index
;;           treemacs-indentation                2
;;           treemacs-indentation-string         " "
;;           treemacs-is-never-other-window      nil
;;           treemacs-never-persist              nil
;;           treemacs-no-png-images              nil
;;           treemacs-recenter-after-file-follow nil
;;           treemacs-recenter-after-tag-follow  nil
;;           treemacs-show-hidden-files          t
;;           treemacs-silent-filewatch           nil
;;           treemacs-silent-refresh             nil
;;           treemacs-sorting                    'alphabetic-desc
;;           treemacs-tag-follow-cleanup         t
;;           treemacs-tag-follow-delay           1.5
;;           treemacs-width                      35)

;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (pcase (cons (not (null (executable-find "git")))
;;                  (not (null (executable-find "python3"))))
;;       (`(t . t)
;;        (treemacs-git-mode 'extended))
;;       (`(t . _)
;;        (treemacs-git-mode 'simple))))
;;   :bind
;;   (:map global-map
;;         ([f8]         . treemacs-toggle)
;;         ("M-0"        . treemacs-select-window)
;;         ("C-c 1"      . treemacs-delete-other-windows)
;;         ("M-m ft"     . treemacs-toggle)
;;         ("M-m fT"     . treemacs)
;;         ("M-m fB"     . treemacs-bookmark)
;;         ("M-m f C-t"  . treemacs-find-file)
;;         ("M-m f M-t"  . treemacs-find-tag)))

;; (use-package treemacs-projectile
;;   :defer t
;;   :ensure t
;;   :config
;;   (setq treemacs-header-function #'treemacs-projectile-create-header)
;;   :bind (:map global-map
;;               ("M-m fP" . treemacs-projectile)
;;               ("M-m fp" . treemacs-projectile-toggle)))

(use-package helm
  :defer t
  :bind (("M-x" . helm-M-x)
         ("C-x C-b" . helm-mini)
         ("M-P" . helm-M-x)
         ("M-r" . helm-recentf)
         :map helm-map
         ("C-j" . helm-next-line)
         ("C-k" . helm-previous-line)
         ("C-f" . helm-next-page)
         ("C-b" . helm-previous-page)
         ("C-h" . helm-next-source)
         ("C-v" . helm-toggle-visible-mark)
         ("C-p" . helm-copy-to-buffer)
         ("C-l" . helm-confirm-and-exit-minibuffer)
         ("ESC" . helm-keyboard-quit)
         ("C-S-H" . describe-key)
         ("C-S-V" . clipboard-yank)
         :map helm-find-files-map
         ("C-l" . helm-execute-persistent-action)
         ("C-h" . helm-find-files-up-one-level)
         :map helm-buffer-map
         ("C-d" . helm-buffer-run-kill-buffers)
         :map helm-read-file-map
         ("C-l" . helm-execute-persistent-action)
         ("C-h" . helm-find-files-up-one-level)
         ("C-S-H" . describe-key))
  :init
  (setq helm-buffers-fuzzy-matching t
        helm-autoresize-mode t
        helm-split-window-inside-p t
        helm-commands-using-frame nil
        helm-display-buffer-default-height 15)

  :config
  (progn
    (helm-mode t)
    (add-hook 'helm-after-initialize-hook 'ts/hide-cursor-in-helm-buffer)))

(use-package helm-ag
  :defer t
  :bind ("∫" . ts/contextual-helm-ag)
  :init
  (setq helm-ag-base-command "ag --nocolor --nogroup"
        ;; helm-ag-command-option "-C2"
        helm-ag-insert-at-point 'symbol))

(use-package helm-projectile
  :defer t
  :init
  (setq helm-projectile-fuzzy-match t))

(use-package helm-google
  :defer t)

(use-package helm-dash
  :defer t
  :init
  (setq helm-dash-browser-func 'eww))

(use-package helm-descbinds
  :defer t)

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
  (progn
    (which-key-mode)
    (which-key-declare-prefixes ", h" "help")
    (which-key-declare-prefixes ", p" "project")
    (which-key-declare-prefixes ", f" "files")
    (which-key-declare-prefixes ", b" "buffers")
    (which-key-declare-prefixes ", g" "git")
    (which-key-declare-prefixes ", o" "org")
    (which-key-declare-prefixes ", x" "errors")
    (which-key-declare-prefixes ", t" "toggle")))

(use-package mu4e
  :bind ("C-c m" . mu4e)
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
  :init
  (setq mu4e-maildir "~/Mail"
        mu4e-sent-folder "/Gmail/Sent Mail"
        mu4e-drafts-folder "/Gmail/Drafts"
        mu4e-trash-folder "/Gmail/Trash"
        mu4e-sent-messages-behavior 'delete)
  :config
  (require 'org-mu4e)
  (setq org-mu4e-link-query-in-headers-mode nil))
;;       mu4e-get-mail-command "offlineimap"
;;       mu4e-update-interval 600
;;       mu4e-view-show-images t
;;       mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")
;; ;; configuration for sending mail
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-stream-type 'starttls
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

(use-package mu4e-alert
  :after mu4e
  :init
  (setq mu4e-alert-style 'osx-notifier
        mu4e-alert-interesting-mail-query
        (concat
         "flag:unread maildir:/Houston/INBOX "
         "OR "
         "flag:unread maildir:/Gmail/INBOX"
         ))
  (mu4e-alert-enable-mode-line-display)
  ;; (defun gjstein-refresh-mu4e-alert-mode-line ()
  ;;   (interactive)
  ;;   (mu4e~proc-kill)
  ;;   (mu4e-alert-enable-mode-line-display)
  ;;   )
  ;; (run-with-timer 0 60 'gjstein-refresh-mu4e-alert-mode-line)
  )

(use-package evil-mu4e
  :after mu4e)

(use-package hide-mode-line
  :hook ((dashboard-mode fundamental-mode) . hide-mode-line-mode))

(use-package powerline
  :init
  (setq powerline-image-apple-rgb t
        powerline-text-scale-factor 1))

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
  :load-path "site-lisp/spaceline-all-the-icons.el"
  :init
  (setq spaceline-all-the-icons-highlight-file-name t
        spaceline-all-the-icons-separator-type 'wave)
  :config
  (progn
    (spaceline-all-the-icons-theme)
    (spaceline-all-the-icons--setup-git-ahead)
    (spaceline-all-the-icons--setup-package-updates)
    ;; (spaceline-all-the-icons-flycheck-alternate t)
    (spaceline-toggle-all-the-icons-buffer-position-on)
    (spaceline-toggle-all-the-icons-region-info-on)
    (spaceline-toggle-all-the-icons-sunrise-off)
    (spaceline-toggle-all-the-icons-sunset-off)))

(use-package company
  :config
  (progn
    (global-company-mode t)

    (define-key company-active-map [tab]       'company-complete)
    (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
    (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort)
    (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
    (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
    (define-key company-active-map (kbd "C-f") 'company-next-page)
    (define-key company-active-map (kbd "C-b") 'company-previous-page)))

(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package lua-mode
  :defer t
  :init
  (setq lua-indent-level 2)
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
    (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
    ))

(use-package web-mode
  :defer t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))))

(use-package rjsx-mode
  :defer t
  :hook (js-mode . rjsx-mode)
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))
    (add-hook 'js-mode-hook (lambda () (setq js2-strict-missing-semi-warning nil)))))

(use-package js2-refactor
  :defer t
  :hook (js-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package expand-region
  :defer t
  :bind (("M-+" . 'er/expand-region)
         ("M--" . 'er/contract-region)))

(use-package shell-pop
  :init
  (setq shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell shell-pop-term-shell))))))

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package eshell-z)

(use-package magit
  ;; :bind (:map magit-status-mode-map
  ;;             ("C-h" . evil-window-left)
  ;;             ("C-j" . evil-window-down)
  ;;             ("C-k" . evil-window-up)
  ;;             ("C-l" . evil-window-right)
  ;;             ("j"   . magit-section-forward)
  ;;             ("k"   . magit-section-backward))
    ;; (evil-define-key 'normal magit-status-mode-map (kbd "M-j") 'magit-section-forward-sibling)
    ;; (evil-define-key 'normal magit-status-mode-map (kbd "M-k") 'magit-section-backward-sibling)
    ;; (evil-define-key 'normal magit-status-mode-map (kbd "C-n") 'magit-section-forward-sibling)
    ;; (evil-define-key 'normal magit-status-mode-map (kbd "C-p") 'magit-section-backward-sibling)


  :init
  (setq git-commit-summary-max-length 50)
  :config
  (magit-define-popup-switch 'magit-pull-popup ?r "Rebase" "--rebase"))

;; (use-package evil-magit
;;   :after magit)

(use-package git-timemachine
  :defer t
  :config
  (eval-after-load 'git-timemachine
    '(progn
       (evil-make-overriding-map git-timemachine-mode-map 'normal)
       ;; force update evil keymaps after git-timemachine-mode loaded
       (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))))

(use-package git-gutter+
  :config
  (global-git-gutter+-mode))

(use-package yasnippet
  :config
  (yas-global-mode t))

(use-package org
  :defer t
  :bind (("C-c l"       . org-store-link)
         ("C-c a"       . org-agenda)
         ("C-c c"       . org-capture)
         ("C-c C-x C-i" . org-clock-in)
         ("C-c C-x C-o" . org-clock-out)
         :map org-mode-map
         ("C-h"   . evil-window-left)
         ("C-h"   . evil-window-left)
         ("C-l"   . evil-window-right)
         ("C-k"   . evil-window-up)
         ("C-j"   . evil-window-down)
         ("M-h"   . org-metaleft)
         ("M-l"   . org-metaright)
         ("M-k"   . org-metaup)
         ("M-j"   . org-metadown)
         ("M-H"   . org-shiftmetaleft)
         ("M-L"   . org-shiftmetaright)
         ("M-K"   . org-shiftmetaup)
         ("M-J"   . org-shiftmetadown)
         ("C-S-H" . org-shiftcontrolleft)
         ("C-S-L" . org-shiftcontrolright)
         ("C-S-K" . org-shiftcontrolup)
         ("C-S-J" . org-shiftcontroldown)
         ("gh"    . org-shiftleft)
         ("gl"    . org-shiftright)
         ("gk"    . org-shiftup)
         ("gj"    . org-shiftdown))
  :config
  (progn
    (setq org-directory "~/Google Drive/org"
          org-default-notes-file (concat org-directory "/inbox.org")
          org-snippets-file (concat org-directory "/snippets.org")
          org-agenda-files (quote ("~/Google Drive/org"
                                   "~/Google Drive/org/work"))
          org-refile-targets (quote ((nil :maxlevel . 1)
                                     (org-agenda-files :maxlevel . 1)))
          org-agenda-custom-commands '((" " "Agenda"
                                        ((agenda "" nil)
                                         (tags-todo "REFILE"
                                                    ((org-agenda-overriding-header "Tasks to Refile")
                                                     (org-tags-match-list-sublevels nil))))))
          ;; org-agenda-window-setup 'only-window
          org-agenda-window-setup 'reorganize-frame
          org-blank-before-new-entry (quote ((heading) (plain-list-item)))
          ;; org-refile-use-outline-path 'file
          ;; org-outline-path-complete-in-steps nil
          org-refile-allow-creating-parent-nodes 'confirm
          org-set-startup-visibility 'content
          org-pretty-entities t
          org-src-fontify-natively t
          org-fontify-done-headline t
          org-src-tab-acts-natively t
          org-log-done 'time
          org-treat-S-cursor-todo-selection-as-state-change nil
          org-startup-indented 'indent
          org-ellipsis "…")
    ;; org-ellipsis "⤵")
    ;; org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
    ;;                     (sequence "⚑ WAITING(w@)" "|")
    ;;                     (sequence "|" "✘ CANCELED(c@)")))
    (setq org-capture-templates
          '(("t" "Task" entry
             (file+headline org-default-notes-file "Refile")
             "* TODO %^{Task}\n"
             :immediate-finish t :kill-buffer t)
            ("b" "Bill" entry
             (file+headline "bills.org" "Bills")
             "* TODO %^{Description}\n%a\nDUE DATE: %^{Deadline}t\n"
             :immediate-finish t :kill-buffer t)
            ("s" "Snippet" entry
             (file+headline org-snippets-file "Snippets")
             "* %^{Title}\t%^g\n#+BEGIN_SRC %^{Language|javascript|emacs-lisp}\n%i%?\n#+END_SRC\n")
            ("i" "Interrupting task" entry
             (file+headline org-default-notes-file "Inbox")
             "* STARTED %^{Task}"
             :clock-in :clock-resume :kill-buffer t)
            ("e" "Emacs task" entry
             (file+headline "emacs/tasks.org" "Emacs tasks")
             "* TODO %^{Task}\n\n"
             :immediate-finish t :kill-buffer t)))))

(use-package org-bullets
  :defer t
  :after org
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "◎" "⚫" "○" "►" "◇")))

(use-package ox-twbs
  :defer t)

(use-package htmlize
  :defer t)

;; (use-package smartparens
;;   :defer t
;;   :hook ((prog-mode . show-smartparens-mode)
;;          (prog-mode . smartparens-mode)))

;; (use-package paredit
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook                  'enable-paredit-mode)
;;   (add-hook 'lisp-mode-hook                        'enable-paredit-mode)
;;   (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
;;   (add-hook 'ielm-mode-hook                        'enable-paredit-mode)
;;   (add-hook 'lisp-mode-hook                        'enable-paredit-mode)
;;   (add-hook 'lisp-interaction-mode-hook            'enable-paredit-mode)
;;   (add-hook 'scheme-mode-hook                      'enable-paredit-mode)
;;   (add-hook 'js-mode-hook                          'ts/paredit-nonlisp))

;; (use-package evil-paredit
;;   :hook (paredit-mode . evil-paredit-mode))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode)
  :init
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  :config
  (progn
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

    (flycheck-add-mode 'javascript-eslint 'web-mode)))

(use-package coffee-mode
  :defer t
  :init
  (setq coffee-tab-width 2))

(use-package engine-mode
  :defer t
  :config
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")

  (engine-mode t))

(use-package highlight-indent-guides
  :defer t
  :hook (coffee-mode . highlight-indent-guides-mode)
  :config
  (setq-default highlight-indent-guides-method 'character))

(use-package indium
  :defer t
  :hook (js-mode-hook . indium-interaction-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :defer t
  :hook ((js2-mode rjsx-mode) . setup-tide-mode)
  :init
  (setq tide-tsserver-executable "/usr/local/bin/tsserver")
  :config
  (progn
    (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
    (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)))

(use-package avy
  :init
  (setq avy-all-windows t))

(use-package yahoo-weather
  :init
  (setq yahoo-weather-location "Vantaa"
        yahoo-weather-format "[%(weather) %(temperature)℃]")
  :config
  (yahoo-weather-mode t))

(use-package solaire-mode
  :config
  (progn
    (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
    (add-hook 'focus-in-hook #'solaire-mode-reset)
    (add-hook 'org-capture-mode-hook #'turn-on-solaire-mode)
    (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)))

(use-package vi-tilde-fringe
  :hook ((prog-mode) . vi-tilde-fringe-mode))

(use-package try)

;; (load "~/.ercrc.el")
;; (use-package erc
;;   :commands erc
;;   :init
;;   (progn
;;     (setq erc-server "irc.freenode.net"
;;           erc-port 6667
;;           erc-nick "tinimini"
;;           erc-away-nickname "tinimini_AWAY"
;;           erc-user-full-name "Tuomo Syvänperä"
;;           erc-prompt-for-password nil
;;           erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#emacs-beginners" "#evil-mode")))
;;     (setq erc-prompt-for-nickserv-password nil
;;           erc-nickserv-passwords '((freenode     (("tinimini" . ,erc-password))))
;;           erc-prompt (lambda () (concat "[" (buffer-name) "]"))))
;;   :config
;;   (erc-services-mode 1))

;; (use-package evil-org
;;   :after (evil org)
;;   :config
;;   (progn
;;     (add-hook 'org-mode-hook 'evil-org-mode)
;;     (add-hook 'evil-org-mode-hook
;;               (lambda ()
;;                 (evil-org-set-key-theme (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading)))))
;;     (require 'evil-org-agenda)
;;     (evil-org-agenda-set-keys)))

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(try vi-tilde-fringe solaire-mode yahoo-weather avy tide indium highlight-indent-guides engine-mode coffee-mode flycheck persistent-scratch smartparens htmlize ox-twbs org-bullets git-gutter+ git-timemachine magit eshell-z eshell-git-prompt shell-pop expand-region js2-refactor rjsx-mode web-mode lua-mode markdown-mode company spaceline-all-the-icons spaceline powerline hide-mode-line evil-mu4e mu4e-alert which-key helm-descbinds helm-dash helm-google helm-projectile helm-ag helm neotree doom-themes all-the-icons rainbow-delimiters rainbow-mode evil-surround evil-leader evil exec-path-from-shell dashboard use-package))
 '(powerline-height 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
