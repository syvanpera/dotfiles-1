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

(load! "+funcs")
(load! "+bindings")
(load! "+hydras")
(load! "+theming")

(setq save-interprogram-paste-before-kill t
      x-select-enable-clipboard nil)

(setq-default indent-tabs-mode nil
              standard-indent 2
              tab-width 2
              js-indent-level 2
              js2-basic-offset 2
              web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-indent-style 2
              fill-column 100)

(setq +pretty-code-iosevka-ligatures-enabled-by-default t)

(setq doom-line-numbers-style 'relative)

(setq +org-dir (expand-file-name "~/Google Drive/org/"))

(setq +email-backend 'offlineimap)

;; use display-buffer-alist instead of display-buffer-function if the following line won't work
;; (setq display-buffer-function 'ts/display-new-buffer)

(setq split-height-threshold nil
      split-width-threshold 0)

(set! :email "Gmail"
  '((mu4e-sent-folder       . "/Gmail/Sent Mail")
    (mu4e-drafts-folder     . "/Gmail/Drafts")
    (mu4e-trash-folder      . "/Gmail/Trash")
    (smtpmail-smtp-user     . "tuomo.syvanpera@gmail.com")
    (user-mail-address      . "tuomo.syvanpera@gmail.com")
    (mu4e-compose-signature . "---\nTuomo"))
  t)

;; (global-prettify-symbols-mode t)
(setq prettify-symbols-unprettify-at-point t)
;; (mac-auto-operator-composition-mode t)

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'dired-mode-hook (lambda () (require 'dired-sort)))
(add-hook 'prog-mode-hook (lambda ()
                            (setq-local show-trailing-whitespace t)))
;;                             (push '("<="  . ?≤) prettify-symbols-alist)
;;                             (push '(">="  . ?≥) prettify-symbols-alist)
;;                             (push '("=="  . ?≡) prettify-symbols-alist)
;;                             (push '("===" . ?≣) prettify-symbols-alist)
;;                             (push '("!="  . ?≠) prettify-symbols-alist)
;;                             (push '("!==" . ?≢) prettify-symbols-alist)
;;                             ;; (push '("=>"  . ?⇒) prettify-symbols-alist)
;;                             ;; (push '("=>"  . ?⇨) prettify-symbols-alist)
;;                             (push '("=>"  . ?⟹) prettify-symbols-alist)
;;                             ;; (push '("<="  . ?⇦) prettify-symbols-alist)
;;                             (push '("->"  . ?→) prettify-symbols-alist)
;;                             (push '("<-"  . ?←) prettify-symbols-alist)))

;; Hasklug Font Ligatures
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(setq dired-listing-switches (concat dired-listing-switches "Gg"))

(setq eshell-buffer-maximum-lines 12000)
;; After being idle for 5 seconds, truncate all the eshell-buffers if
;; needed. If this needs to be canceled, you can run `(cancel-timer
;; eos/eshell-truncate-timer)'
(setq ts/eshell-truncate-timer (run-with-idle-timer 5 t #'ts/truncate-eshell-buffers))

(setq doom-neotree-file-icons t)
(add-hook 'doom-load-theme-hook #'ts-theme-config)

; (after! whitespace
;   (advice-remove #'company-box--make-frame #'doom*fix-whitespace-mode-in-childframes)
;   (advice-remove #'posframe--create-posframe #'doom*fix-whitespace-mode-in-childframes))

(after! evil
  (evil-put-command-property 'evil-yank-line :motion 'evil-line))

;; (after! flycheck
;;   (setq flycheck-check-syntax-automatically '(new-line save idle-change mode-enabled)
;;         flycheck-indication-mode 'right-fringe)
;;   (fringe-helper-define 'flycheck-fringe-bitmap-double-arrow 'center
;;     "...11.11"
;;     "..11.11."
;;     ".11.11.."
;;     "11.11..."
;;     ".11.11.."
;;     "..11.11."
;;     "...11.11"))

(after! ivy
  (setq +ivy-buffer-icons t))

(after! company
  (setq company-idle-delay 0.4
        company-minimum-prefix-length 3)
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion))

(after! web-beautify
  :init
  (map! (:map* (json-mode-map)
          :n "gQ" #'web-beautify-js)))

(after! tide
  ;; (setq tide-tsserver-executable "/usr/local/bin/tsserver")
  ;; (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
  (add-hook 'js-mode-hook
            (lambda ()
              (setq js--prettify-symbols-alist nil)
              (eldoc-mode -1)
              (tide-hl-identifier-mode +1))))

(after! (:any js2-mode web-mode)
  (set-pretty-symbols! '(js2-mode web-mode)
    :lambda "foobar() =>"))

(after! elm-mode
  (setq elm-tags-on-save t
        elm-tags-exclude-elm-stuff t)
  (set-pretty-symbols! 'elm-mode
    :lambda "\\")
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  ;; (add-hook 'elm-mode-hook (lambda ()
                             ;; (push '("/="  . ?≠) prettify-symbols-alist)
                             ;; (push '("|>"  . ?⊳) prettify-symbols-alist)
                             ;; (push '("<|"  . ?⊲) prettify-symbols-alist)
                             ;; (push '("\\"  . ?λ) prettify-symbols-alist)
                             ;; (push '(">>"  . ?») prettify-symbols-alist)
                             ;; (push '("<<"  . ?«) prettify-symbols-alist)))
  )

(after! treemacs
  (setq treemacs-no-png-images nil))

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

(def-package! highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  :hook
  ((elm-mode coffee-mode) . highlight-indent-guides-mode))

(def-package! all-the-icons-ivy
  :after ivy
  :config
  (all-the-icons-ivy-setup))

(def-package! persistent-scratch
  :config
  (setq persistent-scratch-scratch-buffer-p-function
        (lambda () (string-prefix-p "*scratch" (buffer-name))))
  (persistent-scratch-setup-default))

(def-package! golden-ratio
  :config
  ;; (golden-ratio-mode 1)
  (setq golden-ratio-extra-commands
      (append golden-ratio-extra-commands
              '(evil-window-left
                evil-window-right
                evil-window-up
                evil-window-down
                select-window-1
                select-window-2
                select-window-3
                select-window-4
                select-window-5))))

(def-package! ag
  :config
  (setq ag-highlight-search t
        ag-arguments (quote ("-C 2"))))

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

;;----------------------------------------------------------------------------
;; Reason setup
;;----------------------------------------------------------------------------

(defun shell-cmd (cmd)
  "Returns the stdout output of a shell command or nil if the command returned
   an error"
  (car (ignore-errors (apply 'process-lines (split-string cmd)))))

(def-package! reason-mode
  :config
  (setq merlin-ac-setup t)
  (let* ((refmt-bin (or (shell-cmd "refmt ----where")
                        (shell-cmd "which refmt")))
        (merlin-bin (or (shell-cmd "ocamlmerlin ----where")
                        (shell-cmd "which ocamlmerlin")))
        (merlin-base-dir (when merlin-bin
                            (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin))))
    ;; Add npm merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
    (when merlin-bin
      (add-to-list 'load-path (concat merlin-base-dir "share/emacs/site-lisp/"))
      (setq merlin-command merlin-bin))

    (when refmt-bin
      (setq refmt-command refmt-bin)))


  (add-hook 'reason-mode-hook (lambda ()
                                (add-hook 'before-save-hook 'refmt-before-save)
                                (merlin-mode))))

