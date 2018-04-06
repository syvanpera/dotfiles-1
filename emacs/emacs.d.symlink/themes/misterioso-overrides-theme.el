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
 '(flycheck-fringe-error ((t (:foreground "#fc5c94"))))
 '(flycheck-fringe-warning ((t (:foreground " #ff7128"))))
 '(flycheck-fringe-info ((t (:foreground "#ffbd29"))))
 '(helm-header ((t (:background: "DarkOliveGreen3"))))
 '(spaceline-evil-emacs ((t (:background "#3E3D32" :foreground "#3E3D31" :inherit 'mode-line))))
 '(spaceline-evil-normal ((t (:background "SkyBlue2" :foreground "#3E3D31" :inherit 'mode-line))))
 '(spaceline-evil-visual ((t (:background "DarkGoldenrod2" :foreground "#3E3D31" :inherit 'mode-line))))
 '(show-paren-match ((t (:foreground: "white" :background "#6699CC"))))
 '(helm-selection ((t (:background "#4d6968" :weight bold))))
 '(helm-source-header ((t (:background "#338f86" :foreground "white" :weight bold :height 1.1 :family "Sans Serif"))))
 '(helm-visible-mark ((t (:background "#ffbd29" :foreground "black"))))
 '(helm-candidate-number ((t (:background "#ffbd29" :foreground "black"))))
 '(company-tooltip ((t :inherit default :background "#3d5958")))
 '(company-scrollbar-bg ((t :background "#232526")))
 '(company-scrollbar-fg ((t :background "#338f86")))
 '(company-tooltip-selection ((t :background "#5d7978")))
 '(company-tooltip-common ((t :inherit font-lock-constant-face)))
 )

;;; misterioso-overrides-theme.el ends here
