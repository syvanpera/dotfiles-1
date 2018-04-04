;;; misterioso-overrides-theme.el --- My overrides for built-in Misterioso theme for Emacs

;;; Code:

(deftheme misterioso-overrides
  "Misterioso theme overrides.")

(custom-theme-set-faces
 'misterioso-overrides
 '(cursor ((t (:background "#6699CC"))))
 '(line-number-current-line ((t (:foreground "white" :background "#3d5958" :weight bold))))
 '(highlight ((t (:background "#3d5958" :foreground nil))))
 '(region ((t (:background "#338f86" :foreground "#e1e1e0"))))
 '(flycheck-fringe-error ((t (:foreground "#ff7128"))))
 '(flycheck-fringe-warning ((t (:foreground "#ffbd29"))))
 '(flycheck-fringe-info ((t (:foreground "#66bbCC"))))
 )

(provide-theme 'misterioso-overrides)

;;; misterioso-overrides-theme.el ends here
