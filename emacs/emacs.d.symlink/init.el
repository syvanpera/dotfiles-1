;;; init.el --- My magnificent Emacs configuration

;;; Code:

;; Unclutter the interface immediately
;; (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(when (fboundp 'mouse-wheel-mode) (mouse-wheel-mode 1))

;; Reduce the frequency of garbage collection during startup
(setq gc-cons-threshold 100000000)
;; restore after start-up
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;; warn when opening files bigger than 100MB and stop nagging about everything
(setq large-file-warning-threshold 100000000
      warning-minimum-level :emergency)

(require 'package)

(setq package-enable-at-startup nil
      package-archives
      '(("org"          . "http://orgmode.org/elpa/")
        ("melpa"        . "http://melpa.org/packages/")
        ("gnu"          . "http://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp/vendor" user-emacs-directory))

(require 'sensible-defaults)
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

(setq user-full-name "Tuomo Syvänperä"
      user-mail-address "tuomo.syvanpera@gmail.com")

(require 'ts-funcs)
(require 'ts-buffer)
(require 'ts-org)
(require 'ts-redefs)

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
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      compilation-window-height 30
      scroll-conservatively 100)

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
      desktop-save-mode t
      save-place-mode t
      default-frame-alist '((font . "Source Code Pro-12")))

(add-hook 'prog-mode-hook
          (lambda () (progn
                  (setq-local show-trailing-whitespace t)
                  (hl-line-mode)
                  (display-line-numbers-mode)
                  (electric-pair-mode t)
                  (electric-indent-mode t)
                  (add-hook 'evil-insert-state-entry-hook 'ts/line-numbers-absolute nil t)
                  (add-hook 'evil-insert-state-exit-hook 'ts/line-numbers-relative nil t))))

(add-hook 'js2-mode-hook 'js2-refactor-mode)

(global-prettify-symbols-mode t)

(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*"
  (emacs-lock-mode 'kill))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq show-paren-style 'parenthesis)
                                  (push '("<="  . ?≤) prettify-symbols-alist)
                                  (push '(">="  . ?≥) prettify-symbols-alist)
                                  (push '("=="  . ?≡) prettify-symbols-alist)
                                  (push '("===" . ?≣) prettify-symbols-alist)
                                  (push '("!="  . ?≠) prettify-symbols-alist)
                                  (push '("!==" . ?≢) prettify-symbols-alist)
                                  (push '("->"  . ?→) prettify-symbols-alist)
                                  (push '("<-"  . ?←) prettify-symbols-alist)
                                  (push '("=>"  . ?⇒) prettify-symbols-alist)))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local lisp-indent-function #'Fuco1/lisp-indent-function)))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.3)))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side . bottom)
               (reusable-frames . visible)
               (window-height . 0.3)))

(defun display-startup-echo-area-message ()
  "Startup message."
  (message "Another Visitor! Stay awhile! Stay FOREVER!!!!!!!!!!!!"))

(defvar ts-leader          ",")
(defvar ts-local-leader    (concat ts-leader " ,"))

(defvar ts-buffer-prefix   (concat ts-leader " b"))
(defvar ts-file-prefix     (concat ts-leader " f"))
(defvar ts-help-prefix     (concat ts-leader " h"))
(defvar ts-project-prefix  (concat ts-leader " p"))
(defvar ts-git-prefix      (concat ts-leader " g"))
(defvar ts-window-prefix   (concat ts-leader " w"))
(defvar ts-org-prefix      (concat ts-leader " o"))
(defvar ts-jump-prefix     (concat ts-leader " j"))
(defvar ts-error-prefix    (concat ts-leader " x"))
(defvar ts-toggle-prefix   (concat ts-leader " t"))
(defvar ts-bookmark-prefix (concat ts-leader " m"))

;; Generic elisp library packages
(use-package s)
(use-package f)
(use-package dash)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package general
  :config
  (general-create-definer ts-leader-def          :prefix ts-leader)
  (general-create-definer ts-local-leader-def    :prefix ts-local-leader)

  (general-create-definer ts-buffer-prefix-def   :prefix ts-buffer-prefix)
  (general-create-definer ts-file-prefix-def     :prefix ts-file-prefix)
  (general-create-definer ts-help-prefix-def     :prefix ts-help-prefix)
  (general-create-definer ts-project-prefix-def  :prefix ts-project-prefix)
  (general-create-definer ts-git-prefix-def      :prefix ts-git-prefix)
  (general-create-definer ts-window-prefix-def   :prefix ts-window-prefix)
  (general-create-definer ts-org-prefix-def      :prefix ts-org-prefix)
  (general-create-definer ts-jump-prefix-def     :prefix ts-jump-prefix)
  (general-create-definer ts-error-prefix-def    :prefix ts-error-prefix)
  (general-create-definer ts-toggle-prefix-def   :prefix ts-toggle-prefix)
  (general-create-definer ts-bookmark-prefix-def :prefix ts-bookmark-prefix)

  (ts-local-leader-def    'normal "" '(nil :which-key "mode-local"))
  (ts-buffer-prefix-def   'normal "" '(nil :which-key "buffer"))
  (ts-file-prefix-def     'normal "" '(nil :which-key "file"))
  (ts-help-prefix-def     'normal "" '(nil :which-key "help"))
  (ts-project-prefix-def  'normal "" '(nil :which-key "project"))
  (ts-git-prefix-def      'normal "" '(nil :which-key "git"))
  (ts-window-prefix-def   'normal "" '(nil :which-key "window"))
  (ts-org-prefix-def      'normal "" '(nil :which-key "org"))
  (ts-jump-prefix-def     'normal "" '(nil :which-key "jump"))
  (ts-error-prefix-def    'normal "" '(nil :which-key "error"))
  (ts-toggle-prefix-def   'normal "" '(nil :which-key "toggle"))
  (ts-bookmark-prefix-def 'normal "" '(nil :which-key "bookmarks"))

  (ts-leader-def 'normal
    "TAB" 'ts/alternate-buffer
    "'"   'shell-pop
    "s"   'ts/contextual-shell-pop
    "S"   'eshell
    "q"   'ts/kill-window-or-buffer
    "v"   'ts/edit-configuration
    "u"   'ts/load-configuration)

  (ts-toggle-prefix-def 'normal
    "l"  'display-line-numbers-mode
    "r"  'ts/toggle-relative-line-numbers)

  (ts-error-prefix-def 'normal
    "n"  'next-error
    "p"  'previous-error)

  (ts-buffer-prefix-def 'normal
    "k"  'kill-buffer
    "r"  'rename-buffer
    "e"  '((lambda () (interactive) (ielm)) :which-key "elisp-repl")
    "n"  '(ts/create-empty-buffer :which-key "new-empty")
    "s"  '(ts/open-create-scratch-buffer :which-key "scratch")
    "S"  '(ts/create-scratch-buffer :which-key "new-scratch")
    "m"  '((lambda () (interactive) (switch-to-buffer "*Messages*")) :which-key "messages")
    "w"  '((lambda () (interactive) (switch-to-buffer "*Warnings*")) :which-key "warnings"))

  (ts-help-prefix-def 'normal
    "a"  'helm-apropos
    "k"  'describe-key
    "v"  'describe-variable
    "f"  'describe-function
    "w"  'where-is)

  (ts-bookmark-prefix-def 'normal
    "s" 'bookmark-set
    "l" 'list-bookmarks
    "j" 'bookmark-jump)

  ;; Misc global keybindings
  (general-define-key
   "M-h"         (lookup-key global-map (kbd "C-h"))
   "M-v"         'clipboard-yank
   "M-S-<left>"  'shrink-window-horizontally
   "M-S-<right>" 'enlarge-window-horizontally
   "M-S-<down>"  'shrink-window
   "M-S-<up>"    'enlarge-window
   "M-="         'ts/indent-buffer
   "M-e"         'eval-region
   "M-E"         'eval-buffer
   "M-r"         'imenu)

  ;; Ensure ESC quits in all modes: http://stackoverflow.com/a/10166400/61435
  (general-define-key [escape] 'evil-exit-emacs-state)
  (general-define-key :states 'motion [escape] 'keyboard-quit)
  (general-define-key
   :keymaps '(minibuffer-local-map
              minibuffer-local-ns-map
              minibuffer-local-completion-map
              minibuffer-local-must-match-map
              minibuffer-local-isearch-map)
   [escape] 'ts/minibuffer-keyboard-quit)

  (general-define-key
   :states '(normal visual insert) "C-g" 'ts/evil-keyboard-quit)

  (general-define-key
   :prefix "C-c"
   "y" 'clipboard-yank
   "s" 'ts/contextual-shell-pop)

  (general-define-key
   :states '(insert normal visual)
   "M-s" 'save-buffer
   "M-a" 'mark-whole-buffer
   "M-q" 'evil-quit-all
   "M-c" 'evil-yank
   "M-√" (lambda () (interactive) (scroll-other-window 1)) ; meta-shift-j
   "M-ª" (lambda () (interactive) (scroll-other-window-down 1)) ; meta-shift-k
   "M-ƒ" 'scroll-other-window ; meta-shift-f
   "M-›" 'scroll-other-window-down ; meta-shift-b
   ;; TODO These don't actually work anymore
   "M-K" 'move-line-up
   "M-J" 'move-line-down)

  (general-define-key
   :states 'normal
   "C-h" 'evil-window-left
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up
   "C-l" 'evil-window-right)

  (general-define-key
   :states 'insert
   "C-h"   'evil-backward-char
   "C-j"   'evil-next-line
   "C-k"   'evil-previous-line
   "C-l"   'evil-forward-char
   "M-RET" 'comment-indent-new-line)

  (general-define-key
   :states '(normal visual)
   "go"  'find-file-at-point
   "gf"  'projectile-find-file-dwim
   "gF"  'projectile-find-file-dwim-other-window
   "gc"  'sensible-defaults/comment-or-uncomment-region-or-line)

  (general-define-key
   :states 'normal
   :keymaps '(eww-mode-map diff-mode-map debugger-mode-map)
   "q" 'quit-window)

  (general-define-key
   :states 'normal
   :keymaps 'custom-mode-map
   "q" 'Custom-buffer-done)

  (general-define-key
   :states '(normal visual)
   :keymaps 'js-mode-map
   "M-K" 'js2r-move-line-up
   "M-J" 'js2r-move-line-down)

  (general-define-key
   :states 'normal
   :keymaps 'help-mode-map
   "C-h" 'evil-window-left
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up
   "C-l" 'evil-window-right)

  (general-define-key
   :keymaps 'Custom-mode-map
   "M-j" 'widget-forward
   "M-k" 'widget-backward)
  )

(use-package eshell
  :ensure nil
  :init
  (setq eshell-destroy-buffer-when-process-dies t
        eshell-save-history-on-exit t)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (general-define-key
               :states 'insert
               :keymaps 'eshell-mode-map
               "C-d" 'ts/eshell-quit-or-delete-char
               "C-a" 'eshell-bol
               "C-p" 'eshell-previous-matching-input-from-input
               "C-n" 'eshell-next-matching-input-from-input
               "C-r" 'helm-eshell-history)))
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell/alias "l" "ls -l"))))

(use-package undo-tree
  :general
  (:states 'normal
   ;;  "u"   'undo-tree-undo
   ;;  "C-r" 'undo-tree-redo
   "M-u" 'undo-tree-visualize)
  (:states 'normal
   :keymaps 'undo-tree-visualizer-mode-map
   "j"   'undo-tree-visualize-redo
   "k"   'undo-tree-visualize-undo
   "l"   'undo-tree-visualize-switch-branch-right
   "h"   'undo-tree-visualize-switch-branch-left
   "C-h" 'evil-window-left
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up
   "C-l" 'evil-window-right)
  :config
  (global-undo-tree-mode))

(use-package evil
  :after undo-tree
  :init
  (setq evil-want-integration nil
        evil-vsplit-window-right t
        evil-split-window-below nil
        evil-move-beyond-eol nil
        evil-move-cursor-back t)
  :config
  (evil-mode t)
  ;; Some commands are just not meant to be repeated
  (mapc 'evil-declare-not-repeat '(undo-tree-undo undo-tree-redo))
  (add-to-list 'evil-normal-state-modes 'Custom-mode))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode 1))

(use-package rainbow-mode
  :defer t
  :hook ((emacs-lisp-mode css-mode html-mode js-mode rjsx-mode) . rainbow-mode))

(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons)

(use-package oceanic-theme
  :config
  (load-theme 'oceanic)
  (load-theme 'ts-overrides))

(use-package neotree
  :defer t
  :general
  (:states 'normal
   :keymaps 'neotree-mode-map
   "TAB"    'neotree-enter
   "SPC"    'neotree-quick-look
   "o"      'neotree-enter
   "O"      'neotree-enter-horizontal-split
   "q"      'neotree-hide
   "c"      'neotree-create-node
   "r"      'neotree-rename-node
   "d"      'neotree-delete-node
   "H"      'neotree-hidden-file-toggle
   "z"      'neotree-stretch-toggle
   "R"      'neotree-refresh
   "RET"    'neotree-enter)
  (:states 'normal
   :prefix ts-leader
   "e"      'ts/contextual-neotree-toggle)
  (:states 'normal
   :keymaps 'neotree-mode-map
   :prefix ts-leader
   "e"      'ts/contextual-neotree-toggle)
  (:states 'normal
   :prefix ts-file-prefix
   "e"      'neotree-toggle)
  :init
  (setq neo-smart-open t
        neo-create-file-auto-open t
        neo-vc-integration '(face)
        neo-mode-line-type 'neotree
        neo-theme 'icons)
        ;; neo-theme (if (display-graphic-p) 'icons 'nerd))
  :config
  (add-hook 'neo-enter-hook (lambda (type &rest args) (if (equal type 'file) (neotree-hide)))))

(use-package helm
  :defer t
  :general
  ("M-x"     'helm-M-x
   "C-x C-b" 'helm-mini
   "M-P"     'helm-M-x
   "M-r"     'helm-recentf
   "M-y"     'helm-show-kill-ring
   "M-m"     'helm-bookmarks
   "M-p"     'ts/contextual-helm-find-files
   "M-b"     'helm-buffers-list)
  (:keymaps 'helm-map
   "C-j"     'helm-next-line
   "C-k"     'helm-previous-line
   "C-f"     'helm-next-page
   "C-b"     'helm-previous-page
   "C-h"     'helm-next-source
   "C-v"     'helm-toggle-visible-mark
   "C-p"     'helm-copy-to-buffer
   "C-l"     'helm-confirm-and-exit-minibuffer
   "ESC"     'helm-keyboard-quit
   "C-S-H"   'describe-key
   "C-S-V"   'clipboard-yank)
  (:keymaps 'helm-find-files-map
   "C-l"     'helm-execute-persistent-action
   "C-h"     'helm-find-files-up-one-level)
  (:keymaps 'helm-buffer-map
   "C-d"     'helm-buffer-run-kill-buffers)
  (:keymaps 'helm-read-file-map
   "C-l"     'helm-execute-persistent-action
   "C-h"     'helm-find-files-up-one-level
   "C-S-H"   'describe-key)
  (:states 'normal
   :prefix ts-leader
   "r"       'ts/contextual-helm-recentf)
  (:states 'normal
   :prefix ts-buffer-prefix
   "b"       'helm-buffers-list)
  (:states 'normal
   :prefix ts-file-prefix
   "f"       'helm-find-files
   "o"       'ts/helm-find-org-files
   "r"       'helm-recentf)
  (:states 'normal
   :prefix ts-project-prefix
   "p"       'helm-projectile-switch-project
   "f"       'ts/contextual-helm-find-files)
  (:states 'normal
   :prefix ts-org-prefix
   "h"       'helm-org-agenda-files-headings
   "f"       'ts/helm-find-org-files)
  :init
  (setq helm-buffers-fuzzy-matching t
        helm-autoresize-mode t
        helm-split-window-inside-p t
        helm-commands-using-frame nil
        helm-display-buffer-default-height 15)
  :config
  (helm-mode t)
  (add-hook 'helm-after-initialize-hook 'ts/hide-cursor-in-helm-buffer))

(use-package helm-org-rifle
  :defer t
  :commands helm-org-rifle
  :general
  (:states 'normal
   :prefix ts-org-prefix
   "s" 'helm-org-rifle-org-directory))

(use-package helm-ag
  :defer t
  :general
  ("∫" 'ts/contextual-helm-ag)
  (:states 'normal
   :keymaps 'helm-ag-mode-map
   "o" 'helm-ag-mode-jump
   "O" 'helm-ag-mode-jump-other-window)
  :init
  (setq helm-ag-base-command "ag --nocolor --nogroup"
        ;; helm-ag-command-option "-C2"
        helm-ag-insert-at-point 'symbol))

(use-package helm-projectile
  :init
  (setq helm-projectile-fuzzy-match t))

(use-package helm-google
  :defer t)

(use-package helm-dash
  :init
  (setq helm-dash-browser-func 'eww))

(use-package helm-descbinds
  :defer t)

(use-package helm-swoop
  :general
  ("M-i" 'helm-swoop)
  (:states '(normal visual)
   "M-i" 'helm-swoop-from-evil-search)
  (:keymaps 'isearch-mode-map
   "M-i" 'helm-swoop-from-isearch)
  :init
  (setq helm-swoop-use-fuzzy-match nil))

(use-package projectile
  :general
  (:states 'normal
   :prefix ts-project-prefix
   "r" 'projectile-run-project
   "t" 'projectile-test-project
   "s" 'ts/projectile-shell-pop
   "o" '((lambda () (interactive) (ts/load-project-org "veikkaus")) :which-key "ts/load-project-org"))
  :init
  (setq projectile-enable-caching t)
  :config
  (projectile-global-mode))

(use-package which-key
  :config
  (which-key-mode))

(use-package mu4e
  :general
  ("C-c m" 'mu4e)
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
  :init
  (setq mu4e-maildir "~/Mail"
        mu4e-sent-folder "/Gmail/Sent Mail"
        mu4e-drafts-folder "/Gmail/Drafts"
        mu4e-trash-folder "/Gmail/Trash"
        mu4e-sent-messages-behavior 'delete)
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)
  :config
  (require 'org-mu4e)
  (setq org-mu4e-link-query-in-headers-mode nil))
;;       mu4e-get-mail-command "offlineimap"
;;       mu4e-update-interval 600
;;       mu4e-view-show-images t
;;       mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")
;; ;; configuration for sending mail

(use-package mu4e-alert
  :init
  (setq mu4e-alert-style 'osx-notifier
        mu4e-alert-interesting-mail-query
        (concat
         "flag:unread maildir:/Houston/INBOX "
         "OR "
         "flag:unread maildir:/Gmail/INBOX"))
  ;; mu4e-alert-modeline-formatter 'ts/mu4e-alert-modeline-formatter)
  (mu4e-alert-enable-mode-line-display)
  ;; (mu4e-alert-enable-notifications)
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
  :hook (fundamental-mode . hide-mode-line-mode))

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
        spaceline-all-the-icons-hide-inactive-separators t
        spaceline-all-the-icons-separator-type 'wave
        spaceline-all-the-icons-icon-set-eyebrowse-slot 'square)
  :config
  (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-git-ahead)
  (spaceline-all-the-icons--setup-package-updates)
  (spaceline-all-the-icons--setup-paradox)
  ;; (spaceline-all-the-icons-flycheck-alternate t)
  (spaceline-toggle-all-the-icons-buffer-position-on)
  (spaceline-toggle-all-the-icons-region-info-on)
  (spaceline-toggle-all-the-icons-sunrise-off)
  (spaceline-toggle-all-the-icons-sunset-off))

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
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package lua-mode
  :init
  (setq lua-indent-level 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package rjsx-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))
  (add-hook 'js-mode-hook (lambda () (setq js2-strict-missing-semi-warning nil))))

(use-package js2-refactor
  :hook (js-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package expand-region
  :defer t
  :general
  ("M-+" 'er/expand-region
   "M--" 'er/contract-region))

(use-package shell-pop
  :init
  (setq shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell shell-pop-term-shell))))))

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package eshell-z)

(use-package magit
  :general
  (:keymaps 'magit-status-mode-map
   "C-h" 'evil-window-left
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up
   "C-l" 'evil-window-right
   "M-j" 'magit-section-forward-sibling
   "M-k" 'magit-section-backward-sibling
   "j"   'magit-section-forward
   "k"   'magit-section-backward)
  (:states 'normal
   :prefix ts-git-prefix
   "i"  'ts/show-git-info
   "c"  'magit-checkout
   "s"  'magit-status
   "l"  'magit-log-current
   "b"  'magit-blame)
  :init
  (setq git-commit-summary-max-length 50)
  :config
  (magit-define-popup-switch 'magit-pull-popup ?r "Rebase" "--rebase"))

(use-package evil-magit
  :after magit)

(use-package magithub
  :after magit
  :config (magithub-feature-autoinject t))

(use-package git-timemachine
  :defer t
  :config
  (eval-after-load 'git-timemachine
    '(progn
       (evil-make-overriding-map git-timemachine-mode-map 'normal)
       ;; force update evil keymaps after git-timemachine-mode loaded
       (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))))

(use-package git-gutter+
  :general
  (:states 'normal
   :prefix ts-git-prefix
   "n" 'git-gutter+-next-hunk
   "p" 'git-gutter+-previous-hunk
   "v" 'git-gutter+-show-hunk
   "r" 'git-gutter+-revert-hunk)
  :config
  (global-git-gutter+-mode))

(use-package yasnippet
  :config
  (yas-global-mode t))

(use-package htmlize)

(use-package org
  :defer t
  :general
  (:prefix "C-c"
   "l"       'org-store-link
   "a"       'org-agenda
   "c"       'org-capture
   "C-x C-i" 'org-clock-in
   "C-x C-o" 'org-clock-out)
  (:states '(normal visual)
   :keymaps 'org-mode-map
   "gh"      'org-shiftleft
   "gl"      'org-shiftright
   "gk"      'org-shiftup
   "gj"      'org-shiftdown)
  (:states '(normal visual insert)
   :keymaps 'org-mode-map
   "C-h"     'evil-window-left
   "C-h"     'evil-window-left
   "C-l"     'evil-window-right
   "C-k"     'evil-window-up
   "C-j"     'evil-window-down
   "M-h"     'org-metaleft
   "M-l"     'org-metaright
   "M-k"     'org-metaup
   "M-j"     'org-metadown
   "M-H"     'org-shiftmetaleft
   "M-L"     'org-shiftmetaright
   "M-K"     'org-shiftmetaup
   "M-J"     'org-shiftmetadown
   "C-S-H"   'org-shiftcontrolleft
   "C-S-L"   'org-shiftcontrolright
   "C-S-K"   'org-shiftcontrolup
   "C-S-J"   'org-shiftcontroldown)
  (:states 'normal
   :prefix ts-org-prefix
   "a"       'ts/org-agenda-show-agenda-and-todo
   "c"       'org-capture
   "i"       'ts/open-org-inbox
   "r"       'org-refile
   "b"       'org-switchb)
  :config
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
        org-ellipsis "…"
        ;; org-ellipsis " ⤵"
        org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
                            (sequence "⚑ WAITING(w@)" "|")
                            (sequence "|" "✘ CANCELED(c@)")))
  (setq org-capture-templates
        '(("t" "Task" entry
           (file+headline org-default-notes-file "Refile")
           "* ☛ TODO %^{Task}\n"
           :immediate-finish t :kill-buffer t)
          ("b" "Bill" entry
           (file+headline "bills.org" "Bills")
           "* ☛ TODO %^{Description}\n%a\nDUE DATE: %^{Deadline}t\n"
           :immediate-finish t :kill-buffer t)
          ("s" "Snippet" entry
           (file+headline org-snippets-file "Snippets")
           "* %^{Title}\t%^g\n#+BEGIN_SRC %^{Language|javascript|emacs-lisp}\n%i%?\n#+END_SRC\n")
          ("i" "Interrupting task" entry
           (file+headline org-default-notes-file "Inbox")
           "* STARTED %^{Task}"
           :clock-in :clock-resume :kill-buffer t)
          ("e" "Emacs task" entry
           (file+headline "emacs.org" "Tasks")
           "* ☛ TODO %^{Task}\n\n"
           :immediate-finish t :kill-buffer t))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "►" "◎" "◇")))

(use-package persistent-scratch
  :config
  (setq persistent-scratch-scratch-buffer-p-function
        (lambda () (string-prefix-p "*scratch" (buffer-name))))
  (persistent-scratch-setup-default))

(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode)
  :general
  (:states 'normal
   :keymaps 'flycheck-error-list-mode-map
   "j"   'flycheck-error-list-next-error
   "k"   'flycheck-error-list-previous-error
   "q"   'kill-buffer-and-window
   "RET" 'flycheck-error-list-goto-error)
  (:states 'normal
   :prefix ts-error-prefix
   "x" 'flycheck-mode
   "l" 'flycheck-list-errors
   "v" 'flycheck-verify-setup)
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint emacs-lisp-checkdoc)))
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

  (flycheck-add-mode 'javascript-eslint 'web-mode))

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
  (flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode 1)
  (tide-hl-identifier-mode t)
  (tide-completion-detailed t)
  (tide-default-mode "JSX"))

(use-package tide
  :defer t
  :hook ((js2-mode rjsx-mode) . setup-tide-mode)
  :general
  (:states 'normal
   :keymaps 'tide-mode-map
   "gd"  'tide-jump-to-definition
   "C-o" 'tide-jump-back
   "C-t" 'tide-jump-back
   "K"   'tide-documentation-at-point)
  (:states 'normal
   :keymaps 'tide-references-mode-map
   "gj"       'tide-find-next-reference
   "gk"       'tide-find-previous-reference
   "C-j"      'tide-find-next-reference
   "C-k"      'tide-find-previous-reference
   "C-l"      'tide-goto-reference
   "<return>" 'tide-goto-reference
   "q"        'quit-window)
  (:states 'normal
   :keymaps 'tide-project-errors-mode-map
   "gj"       'tide-find-next-error
   "gk"       'tide-find-previous-error
   "C-j"      'tide-find-next-error
   "C-k"      'tide-find-previous-error
   "C-l"      'tide-goto-error
   "<return>" 'tide-goto-error
   "q"        'quit-window)
  :init
  (setq tide-tsserver-executable "/usr/local/bin/tsserver")
  :config
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append))

(use-package avy
  :general
  (:states 'normal
   "M-j" 'avy-goto-word-1)
  (:states 'normal
   :prefix ts-jump-prefix
   "w" 'avy-goto-word-1
   "c" 'avy-goto-char
   "l" 'avy-goto-line)
  :init
  (setq avy-all-windows t))

;; (use-package yahoo-weather
;;   :init
;;   (setq yahoo-weather-location "Vantaa"
;;         yahoo-weather-format "[%(weather) %(temperature)℃]")
;;   :config
;;   (yahoo-weather-mode t))

(use-package solaire-mode
  :config
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'focus-in-hook #'solaire-mode-reset)
  (add-hook 'org-capture-mode-hook #'turn-on-solaire-mode)
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer))

(use-package vi-tilde-fringe
  :hook ((prog-mode) . vi-tilde-fringe-mode))

(use-package try)

(use-package paradox
  :config
  (paradox-enable))

(use-package persp-mode
  :init
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  :config
  (add-hook 'after-init-hook (lambda () (persp-mode t))))

(use-package eyebrowse
  :general
  ("M-1" 'eyebrowse-switch-to-window-config-1
   "M-2" 'eyebrowse-switch-to-window-config-2
   "M-3" 'eyebrowse-switch-to-window-config-3
   "M-4" 'eyebrowse-switch-to-window-config-4
   "M-5" 'eyebrowse-switch-to-window-config-5
   "M-6" 'eyebrowse-switch-to-window-config-6
   "M-7" 'eyebrowse-switch-to-window-config-7
   "M-8" 'eyebrowse-switch-to-window-config-8
   "M-9" 'eyebrowse-switch-to-window-config-9)
  (:states 'normal
   :prefix ts-window-prefix
   "c"   'eyebrowse-create-window-config
   "d"   'eyebrowse-close-window-config
   "r"   'eyebrowse-rename-window-config
   "n"   'eyebrowse-next-window-config
   "p"   'eyebrowse-prev-window-config
   "j"   'eyebrowse-next-window-config
   "k"   'eyebrowse-prev-window-config
   "l"   'eyebrowse-next-window-config
   "h"   'eyebrowse-prev-window-config)
  :init
  (setq eyebrowse-new-workspace t
        eyebrowse-wrap-around t)
  :config
  (eyebrowse-mode t))

;; (load "~/.ercrc.el")
;; (use-package erc
;;   :ensure nil
;;   :commands erc
;;   :init
;;   (setq erc-server "irc.freenode.net"
;;         erc-port 6667
;;         erc-nick "tinimini"
;;         erc-away-nickname "tinimini_AWAY"
;;         erc-user-full-name "Tuomo Syvänperä"
;;         erc-prompt-for-password nil
;;         erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#emacs-beginners" "#evil-mode"))
;;         erc-prompt-for-nickserv-password nil
;;         erc-nickserv-passwords '((freenode     (("tinimini" . ,erc-password))))
;;         erc-prompt (lambda () (concat "[" (buffer-name) "]")))
;;   :config
;;   (erc-services-mode 1))

(load custom-file)

(provide 'init)
