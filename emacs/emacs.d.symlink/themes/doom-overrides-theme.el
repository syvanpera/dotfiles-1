;;; doom-overrides-theme.el --- Overrides for doom theme for Emacs

;;; Code:

(deftheme doom-overrides
  "Doom theme overrides.")

(custom-theme-set-faces
 'doom-overrides
 '(line-number ((t (:foreground "#65737E"))))
 '(highlight ((t (:background "#3d5958" :foreground nil))))
 '(fringe ((t (:foreground "#65737E"))))
 '(region ((t (:background "#338f86" :foreground "#e1e1e0"))))
 '(show-paren-match ((t (:foreground: "white" :background "#74af68"))))
 '(org-todo ((t (:bold t :weight bold :foreground "#e2716c"))))
 '(org-done ((t (:strike-through nil :bold t :weight bold :foreground "DarkOliveGreen3"))))
 '(org-level-1 ((t (:height 1.1))))
 '(org-level-2 ((t (:bold nil :weight normal))))
 '(org-level-3 ((t (:bold nil :weight normal))))
 '(org-level-4 ((t (:bold nil :weight normal))))
 '(org-ellipsis ((t (:underline nil :background nil :foreground "#74af68"))))
 '(spaceline-evil-normal ((t (:background "DarkGoldenrod2" :foreground "#3E3D31" :inherit 'mode-line))))
 '(spaceline-evil-visual ((t (:background "#338f86" :foreground "white" :inherit 'mode-line))))
 '(spaceline-evil-insert ((t (:background "DarkOliveGreen3" :foreground "#3E3D31" :inherit 'mode-line))))
 )

;;; doom-overrides-theme.el ends here
