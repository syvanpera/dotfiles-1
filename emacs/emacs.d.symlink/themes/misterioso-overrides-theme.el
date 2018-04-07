;;; misterioso-overrides-theme.el --- My overrides for built-in Misterioso theme for Emacs

;;; Code:

(deftheme misterioso-overrides
  "Misterioso theme overrides.")

(custom-theme-set-faces
 'misterioso-overrides
 '(cursor ((t (:background "DarkOliveGreen3"))))
 '(line-number-current-line ((t (:foreground "white" :background "#3d5958" :weight bold))))
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
 '(helm-selection ((t (:background "#4d6968" :weight bold))))
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
 '(org-level-1 ((t (:height 1.2 :inherit 'outline-1))))
 ;; '(org-level-1 ((t (:height 1.2 :inherit 'outline-3))))
 ;; '(org-level-2 ((t (:inherit 'outline-4))))
 ;; '(org-level-3 ((t (:inherit 'outline-2))))
 ;; '(org-level-4 ((t (:inherit 'outline-1))))
 ;; '(org-level-2 ((t (:foreground "DarkOliveGreen3"))))
 ;; '(org-level-3 ((t (:foreground "#6699CC"))))
 ;; '(org-level-4 ((t (:foreground "white"))))
 '(org-ellipsis ((t (:underline nil))))
 )

;;; misterioso-overrides-theme.el ends here
