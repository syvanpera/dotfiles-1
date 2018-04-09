;;; misterioso-overrides-theme.el --- My overrides for built-in Misterioso theme for Emacs

;;; Code:

(deftheme misterioso-overrides
  "Misterioso theme overrides.")

(custom-theme-set-faces
 'misterioso-overrides
 '(default ((t (:background "#1B2B32"))))
 '(fringe ((t (:background "#1B2B32"))))
 '(cursor ((t (:background "white"))))
 '(line-number-current-line ((t (:foreground "white" :background "#3d5958" :weight bold))))
 '(line-number ((t (:background "#2d4948"))))
 '(highlight ((t (:background "#3d5958" :foreground nil))))
 '(region ((t (:background "#338f86" :foreground "#e1e1e0"))))
 '(flycheck-fringe-error ((t (:foreground "systemRedColor"))))
 '(flycheck-fringe-warning ((t (:foreground "DarkGoldenrod2"))))
 '(flycheck-fringe-info ((t (:foreground "DarkOliveGreen3"))))
 '(spaceline-evil-emacs ((t (:background "#3E3D32" :foreground "#3E3D31" :inherit 'mode-line))))
 '(spaceline-evil-normal ((t (:background "DarkGoldenrod2" :foreground "#3E3D31" :inherit 'mode-line))))
 '(spaceline-evil-visual ((t (:background "#338f86" :foreground "white" :inherit 'mode-line))))
 '(spaceline-evil-insert ((t (:background "DarkOliveGreen3" :foreground "#3E3D31" :inherit 'mode-line))))
 '(show-paren-match ((t (:foreground: "white" :background "#6699CC"))))
 '(helm-selection ((t (:background "#3d5958" :weight bold))))
 '(helm-source-header ((t (:background nil :foreground "white" :weight bold :height 1.1 :family "Menlo"))))
 '(helm-visible-mark ((t (:background "#ffbd29" :foreground "black"))))
 '(helm-candidate-number ((t (:background "#ffbd29" :foreground "black"))))
 '(helm-buffer-directory ((t (:background "#5d7978" :foreground "white"))))
 '(helm-header ((t (:background "#5d7978" :foreground "gray90"))))
 '(helm-ff-file ((t (:background nil :foreground "gray80"))))
 '(helm-ff-directory ((t (:background nil :foreground "#6699CC"))))
 '(helm-ff-executable ((t (:background nil :foreground "DarkOliveGreen3"))))
 '(helm-ff-dotted-directory ((t (:background nil :foreground "#6699CC"))))
 '(helm-ff-prefix ((t (:background "#ffbd29" :foreground "white"))))
 '(company-tooltip ((t :inherit default :background "#3d5958")))
 '(company-scrollbar-bg ((t :background "#232526")))
 '(company-scrollbar-fg ((t :background "#338f86")))
 '(company-tooltip-selection ((t :background "#5d7978")))
 '(company-tooltip-common ((t :inherit font-lock-constant-face)))
 '(org-todo ((t (:bold t :weight bold :foreground "#e2716c"))))
 '(org-done ((t (:bold t :weight bold :foreground "DarkOliveGreen3"))))
 '(org-headline-done ((t (:foreground "#5d7978" :strike-through t))))
 '(org-level-1 ((t (:height 1.1 :inherit 'outline-1 :foreground "#72afe9"))))
 '(org-level-2 ((t (:inherit 'outline-2 :foreground "#bb7ed7"))))
 '(org-level-3 ((t (:inherit 'outline-3 :foreground "#ffbd29"))))
 '(org-ellipsis ((t (:underline nil))))
 '(org-archived ((t (:underline t))))
 '(font-lock-comment-face ((t (:foreground "#65737E"))))
 '(font-lock-string-face ((t (:foreground "#74af68"))))
 ;; '(solaire-default-face ((t (:background "#2d3743"))))
 )

;;; misterioso-overrides-theme.el ends here
