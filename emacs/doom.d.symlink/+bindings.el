;;; +bindings.el -*- lexical-binding: t; -*-

(map!
 ;; --- Global keybindings ---------------------------
 ;; :gnvime "M-p" #'counsel-projectile-find-file
 :gnvime "M-p" #'helm-projectile-find-file
 :gnvime "M-P" #'execute-extended-command
 :gnvime "M-e" #'+neotree/find-this-file
 ;; :gnvime "M-y" #'counsel-yank-pop
 :gnvime "M-y" #'helm-show-kill-ring
 :gnvime "M-u" #'undo-tree-visualize
 :gnvime "M-g" #'ts@git-hydra/body
 ;; :gnvime "M-F" #'ts/project-search
 :gnvime "M-F" #'+helm/project-search
 :gnvime "M-f" #'swiper
 :gnvime "C-u" #'universal-argument
 :gnvime "M-√" #'evil-window-increase-height
 :gnvime "M-ª" #'evil-window-decrease-height
 :gnvime "M-ﬁ" #'evil-window-increase-width
 :gnvime "M-˛" #'evil-window-decrease-width
 ;; :gnvime "M-r" #'counsel-imenu
 :gnvime "M-r" #'helm-imenu
 :gnvime "M-R" #'+eval/buffer
 :gnvime "M-o" #'ff-find-other-file
 :gnvime "M-O" #'projectile-find-other-file-other-window
 :gnvime "C-j" #'evil-window-down
 :gnvime "C-k" #'evil-window-up
 :gnvime "<C-f5>" '(lambda () (interactive) (bookmark-set "QUICKSAVE"))
 :gnvime "<f5>"   '(lambda () (interactive) (bookmark-jump "QUICKSAVE"))
 :nv     "C-SPC" nil

 :i      "M-s" #'save-buffer

 ;; Text-scaling
 :ne "M-=" (λ! (text-scale-set 0))
 :ne "M-0" (λ! (text-scale-set 0))
 :ne "M-+" #'text-scale-increase
 :ne "M--" #'text-scale-decrease
 :ne "M-z" #'doom@text-zoom/body

 ;; --- Personal vim-esque bindings ------------------
 :nm "gf" #'projectile-find-file-dwim
 :nm "gF" #'projectile-find-file-dwim-other-window

 ;; Org
 (:prefix "C-c"
   :gnvime "i"       #'ts/open-org-inbox
   :gnvime "l"       #'org-store-link
   :gnvime "a"       #'org-agenda
   :gnvime "c"       #'org-capture
   :gnvime "b"       #'org-switchb)

 ;; --- <leader> -------------------------------------
 (:leader
   :desc "Pop up shell"           :n  "'" #'projectile-run-eshell
   (:desc "buffer" :prefix "b"
     :desc "Kill buffer"          :n  "d" #'kill-this-buffer
     :desc "Open messages buffer" :n  "m" #'spacemacs/switch-to-messages-buffer
     :desc "Open scratch buffer"  :n  "s" #'spacemacs/switch-to-scratch-buffer)
   (:desc "errors" :prefix "e"
     :desc "List errors"          :n  "l" #'flycheck-list-errors
     :desc "Next error"           :n  "n" #'flycheck-next-error
     :desc "Previous error"       :n  "p" #'flycheck-previous-error)
   (:desc "git" :prefix "g"
     :desc "Git status"           :n  "s" #'magit-status
     :desc "Next hunk"            :nv "n" #'git-gutter:next-hunk
     :desc "Previous hunk"        :nv "p" #'git-gutter:previous-hunk
     :desc "View hunk"            :nv "v" #'git-gutter:popup-hunk)
   (:desc "open" :prefix "o"
     :desc "Eshell"               :n  "s" #'eshell)
   ;; (:desc "file" :prefix "f"
   ;;   :desc "Find file"            :n  "f" #'counsel-find-file)
   (:desc "file" :prefix "f"
     :desc "Find file"            :n  "f" #'helm-find-files)
   ;; (:desc "project" :prefix "p"
   ;;   :desc "Find file in project" :n  "f" #'counsel-projectile-find-file))
   (:desc "project" :prefix "p"
     :desc "Find file in project" :n  "f" #'helm-projectile-find-file))

 ;; --- <localleader> -------------------------------------
 (:localleader
   :desc "Switch to alt buffer"      :nv "TAB" #'spacemacs/alternate-buffer
   ;; :desc "Close window or workspace" :nv "q"   #'+workspace/close-window-or-workspace
   ;; :desc "Switch workspace buffer"   :nv "b"   #'persp-switch-to-buffer
   ;; :desc "Switch buffer"             :nv "B"   #'helm-mini
   ;; :desc "Browse files"              :nv "f"   #'find-file
   ;; :desc "Browse files"              :nv "F"   #'projectile-find-file
   :desc "Toggle Neotree"            :nv "e"   #'neotree-toggle
   :desc "Open private config"       :n  "v"   #'ts/open-config-file
   (:desc "git" :prefix "g"
     :desc "Git status"              :n  "s"   #'magit-status
     :desc "Git transient state"     :n  "g"   #'ts@git-hydra/body
     :desc "Stage hunk"              :n  "S"   #'git-gutter:stage-hunk
     :desc "View hunk"               :nv "v"   #'git-gutter:popup-hunk
     :desc "Next hunk"               :nv "n"   #'git-gutter:next-hunk
     :desc "Previous hunk"           :nv "p"   #'git-gutter:previous-hunk
     :desc "Git revert hunk"         :n  "r"   #'git-gutter:revert-hunk))

 ;; company
 (:after elm-mode
   (:map elm-mode-map
     "C-o" #'pop-tag-mark
     :nvme "gd"  #'elm-mode-goto-tag-at-point))

 ;; company
 (:after company
   (:map company-active-map
     "C-f"     #'company-next-page
     "C-b"     #'company-previous-page))

 ;; evil
 (:after evil
   (:map evil-window-map ; prefix "C-w"
     "o" #'doom/window-zoom
     "z" #'doom/window-enlargen))

 (:after elm-mode
   (:map elm-mode-map
     :nvme "M-." 'elm-mode-goto-tag-at-point))

 ;; (:after haskell-mode
 ;;   :map inf-haskell-map
 ;;   :nvim "C-j" #'evil-window-down
 ;;   :nvim "C-k" #'evil-window-up
 ;;   :nvim "C-h" #'evil-window-left
 ;;   :nvim "C-l" #'evil-window-right)

 ;; ivy
 (:after ivy
   :map ivy-minibuffer-map
   "C-h" #'ivy-backward-delete-char
   "C-f" #'ivy-scroll-up-command
   "C-b" #'ivy-scroll-down-command)

 ;; org
 (:after org
   :map org-mode-map
   :nvime "M-h" #'org-metaleft
   :nvime "M-l" #'org-metaright
   :nvime "M-J" #'org-shiftmetadown
   :nvime "M-K" #'org-shiftmetaup
   :nvime "M-H" #'org-shiftmetaleft
   :nvime "M-L" #'org-shiftmetaright
   :nvime "C-j" #'evil-window-down
   :nvime "C-k" #'evil-window-up
   :nvime "ﬁ" #'org-shiftright
   :nvime "˛" #'org-shiftleft
   :nvime "ª" #'org-shiftup
   :nvime "√" #'org-shiftdown)

 ;; helm
 ;; (:after helm
 ;;   (:map helm-map
 ;;     "C-k"        #'helm-previous-line
 ;;     "C-j"        #'helm-next-line
 ;;     "C-l"        (kbd "RET")
 ;;     "C-b"        #'helm-previous-page
 ;;     "C-f"        #'helm-next-page)

 ;;   (:after helm-files
 ;;     (:map helm-find-files-map
 ;;       "C-h" #'helm-find-files-up-one-level
 ;;       "C-l" (kbd "RET"))))
 )


;; evil-collection defines some of these and we need to override them. So bind these
;; after evil-collection
(after! evil-collection
  (defun ts/init-eshell-keymap ()
    "Setup eshell keybindings. This must be done in a hook because eshell-mode
    redefines its keys every time `eshell-mode' is enabled."
    (map! :map eshell-mode-map
          :nvim "C-j" #'evil-window-down
          :nvim "C-k" #'evil-window-up
          :nvim "C-h" #'evil-window-left
          :nvim "C-l" #'evil-window-right
          :i    "C-p" #'eshell-previous-matching-input-from-input
          :i    "C-n" #'eshell-next-matching-input-from-input))

  (add-hook 'eshell-first-time-mode-hook #'ts/init-eshell-keymap))
