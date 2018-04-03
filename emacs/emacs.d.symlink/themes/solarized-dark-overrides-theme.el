(deftheme solarized-dark-overrides
  "Overrides to be applied over `solarized-dark'.")

(custom-theme-set-faces
 'solarized-dark-overrides
 '(line-number ((t (:background "#073641"))))
 '(line-number-current-line ((t (:foreground "white" :background "#073641"))))
 '(hl-line ((t (:background "#073641")))))

(provide-theme 'solarized-dark-overrides)
