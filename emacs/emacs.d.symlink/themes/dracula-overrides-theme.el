(deftheme dracula-overrides
  "Overrides to be applied over `dracula'.")

(custom-theme-set-faces
 'dracula-overrides
 '(line-number-current-line ((t (:foreground "white"))))
 '(telephone-line-evil-normal ((t (:background "#ff79c6"))))
 '(spaceline-evil-normal ((t (:background "#ff79c6"))))
 '(spaceline-evil-visual ((t (:background "dark orange")))))

(provide-theme 'dracula-overrides)
