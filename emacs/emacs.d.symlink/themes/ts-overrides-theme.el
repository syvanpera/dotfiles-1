(deftheme ts-overrides
  "My own theme overrides.")

(custom-theme-set-faces
 'ts-overrides
 '(line-number-current-line ((t (:foreground "white"))))
 '(spaceline-evil-normal ((t (:background "green"))))
 '(spaceline-evil-visual ((t (:background "dark orange")))))

(provide-theme 'ts-overrides)
