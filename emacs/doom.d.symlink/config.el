;;; config.el -*- lexical-binding: t; -*-

(load "~/.emacs-secrets" t)

(fringe-mode '(4 . 8))

(add-to-list 'load-path (expand-file-name "elisp" doom-private-dir))

(setenv "PATH" (concat (expand-file-name "~/.dotfiles/bin") ":" (getenv "PATH")))
(setq exec-path (append (list (expand-file-name "~/.dotfiles/bin")) exec-path))

(defvar ts-scratch-mode 'lisp-interaction-mode
  "Default major mode of the scratch buffer.")

(with-current-buffer "*scratch*"  (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*" (emacs-lock-mode 'kill))

(load! +funcs)
(load! +bindings)
(load! +hydras)

;; (setq save-interprogram-paste-before-kill t
;;       x-select-enable-clipboard nil)

(setq-default indent-tabs-mode nil
              standard-indent 2
              tab-width 2
              js-indent-level 2
              js2-basic-offset 2
              web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-indent-style 2)

(setq doom-line-numbers-style 'relative)

(setq +org-dir (expand-file-name "~/Google Drive/org/"))

(setq +email-backend 'offlineimap)

(set! :email "Gmail"
  '((mu4e-sent-folder       . "/Gmail/Sent Mail")
    (mu4e-drafts-folder     . "/Gmail/Drafts")
    (mu4e-trash-folder      . "/Gmail/Trash")
    (smtpmail-smtp-user     . "tuomo.syvanpera@gmail.com")
    (user-mail-address      . "tuomo.syvanpera@gmail.com")
    (mu4e-compose-signature . "---\nTuomo"))
  t)

(global-prettify-symbols-mode t)
(setq prettify-symbols-unprettify-at-point t)

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'dired-mode-hook (lambda () (require 'dired-sort)))
(add-hook 'prog-mode-hook (lambda ()
                            (setq-local show-trailing-whitespace t)
                            (push '("<="  . ?≤) prettify-symbols-alist)
                            (push '(">="  . ?≥) prettify-symbols-alist)
                            (push '("=="  . ?≡) prettify-symbols-alist)
                            (push '("===" . ?≣) prettify-symbols-alist)
                            (push '("/="  . ?≠) prettify-symbols-alist)
                            (push '("!="  . ?≠) prettify-symbols-alist)
                            (push '("!==" . ?≢) prettify-symbols-alist)
                            (push '("=>"  . ?⇨) prettify-symbols-alist)
                            ;; (push '("<="  . ?⇦) prettify-symbols-alist)
                            (push '("->"  . ?→) prettify-symbols-alist)
                            (push '("<-"  . ?←) prettify-symbols-alist)))

(setq dired-listing-switches (concat dired-listing-switches "Gg"))

(setq eshell-buffer-maximum-lines 12000)
;; After being idle for 5 seconds, truncate all the eshell-buffers if
;; needed. If this needs to be canceled, you can run `(cancel-timer
;; eos/eshell-truncate-timer)'
(setq ts/eshell-truncate-timer (run-with-idle-timer 5 t #'ts/truncate-eshell-buffers))

(after! evil
  (evil-put-command-property 'evil-yank-line :motion 'evil-line))

(after! flycheck
  (setq flycheck-check-syntax-automatically '(new-line save idle-change mode-enabled)
        flycheck-indication-mode 'right-fringe)
  (fringe-helper-define 'flycheck-fringe-bitmap-double-arrow 'center
    "...11.11"
    "..11.11."
    ".11.11.."
    "11.11..."
    ".11.11.."
    "..11.11."
    "...11.11"))

(after! ivy
  (setq +ivy-buffer-icons t))

(after! company
  (setq company-idle-delay 0.4
        company-minimum-prefix-length 3))

(after! web-beautify
  :init
  (map! (:map* (json-mode-map)
          :n "gQ" #'web-beautify-js)))

(after! tide
  ;; (setq tide-tsserver-executable "/usr/local/bin/tsserver")
  ;; (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
  (add-hook 'js-mode-hook
            (lambda ()
              (eldoc-mode -1)
              (tide-hl-identifier-mode +1))))

(after! elm-mode
  (add-hook 'elm-mode-hook (lambda ()
                             (push '("|>"  . ?⊳) prettify-symbols-alist)
                             (push '("<|"  . ?⊲) prettify-symbols-alist)
                             (push '("\\"  . ?λ) prettify-symbols-alist)
                             (push '(">>"  . ?») prettify-symbols-alist)
                             (push '("<<"  . ?«) prettify-symbols-alist))))

(after! doom-themes
  (setq doom-neotree-file-icons t)
  (load! +theming)
  (add-hook 'doom-load-theme-hook #'ts-theme-config))

(after! org
  (require 'ob-elm)
  (setq org-snippets-file (concat org-directory "/snippets.org")
        org-bullets-bullet-list '("■" "○" "◉" "◆" "▶" "▲")
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
        ;; org-agenda-window-setup 'reorganize-frame
        ;; org-blank-before-new-entry (quote ((heading) (plain-list-item)))
        ;; org-refile-use-outline-path 'file
        ;; org-outline-path-complete-in-steps nil
        org-use-fast-todo-selection t
        org-treat-S-cursor-todo-selection-as-state-change nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-set-startup-visibility 'content
        org-pretty-entities t
        ;; org-src-fontify-natively t
        ;; org-fontify-done-headline t
        ;; org-src-tab-acts-natively t
        ;; org-log-done 'time
        ;; org-startup-indented 'indent
        ;; org-ellipsis "…"
        org-ellipsis " ⤵"
        org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
                            (sequence "⚑ WAITING(w@)" "|")
                            (sequence "|" "✘ CANCELED(c@)")))
  (setq org-capture-templates
        '(("t" "Task" entry
           (file+headline "refile.org" "Tasks")
           "* ☛ TODO %^{Task}\n"
           :immediate-finish t :kill-buffer t)
          ("n" "Note" entry
           (file+headline +org-default-notes-file "Notes")
           "* %?\n"
           :immediate-finish t :kill-buffer t)
          ("b" "Bill" entry
           (file+headline "bills.org" "Bills")
           "* ☛ TODO %^{Description}\n%a\nDUE DATE: %^{Deadline}t\n"
           :immediate-finish t :kill-buffer t)
          ("s" "Snippet" entry
           (file+headline org-snippets-file "Snippets")
           "* %^{Title}\t%^g\n#+BEGIN_SRC %^{Language|javascript|emacs-lisp}\n%i%?\n#+END_SRC\n")
          ("i" "Interrupting task" entry
           (file+headline +org-default-todo-file "Inbox")
           "* STARTED %^{Task}"
           :clock-in :clock-resume :kill-buffer t)
          ("e" "Emacs task" entry
           (file+headline "emacs.org" "Tasks")
           "* ☛ TODO %^{Task}\n\n"
           :immediate-finish t :kill-buffer t))))

(def-package! all-the-icons-ivy
  :after ivy
  :config
  (all-the-icons-ivy-setup))

(def-package! persistent-scratch
  :config
  (setq persistent-scratch-scratch-buffer-p-function
        (lambda () (string-prefix-p "*scratch" (buffer-name))))
  (persistent-scratch-setup-default))

;; (def-package! indium
;;   :hook (js-mode . indium-interaction-mode))

(def-package! org-projectile
  :after org
  :config
  (progn
    (setq org-projectile-projects-file (concat org-directory "/projects.org")
          org-projectile-capture-template "* ☛ TODO %?\n"
          org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)))

(def-package! org-gcal
  :after org
  :config
  (setq org-gcal-client-id ts-secrets/org-gcal-client-id
        org-gcal-client-secret ts-secrets/org-gcal-client-secret
        org-gcal-file-alist '(("tuomo.syvanpera@gmail.com" .  "~/Google Drive/org/gcal.org"))))

