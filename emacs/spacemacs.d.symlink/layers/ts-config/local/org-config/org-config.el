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
         :immediate-finish t :kill-buffer t)))

(defun ts/open-org-inbox ()
  "Open org inbox file."
  (interactive)
  (find-file org-default-notes-file))

(defun ts/open-org-dir ()
  "Open org directory."
  (interactive)
  (let ((default-directory (concat org-directory "/")))
    (helm-find-files nil)))


(provide 'org-config)
