(deftheme ts-overrides
  "My own theme overrides.")

(custom-theme-set-faces
 'ts-overrides
 '(line-number-current-line ((t (:foreground "white" :background "#343D46"))))
 ;; '(spaceline-evil-normal ((t (:background "#99C794"))))
 ;; '(spaceline-evil-insert ((t (:background "#99C794"))))
 ;; '(spaceline-evil-visual ((t (:background "dark orange"))))
 '(fringe ((t (:background "#343D46"))))
 '(region ((t (:background "wheat"))))
 '(mode-line ((t (:background "#6699CC" :foreground "#1B2B34" :box nil :weight normal))))
 )

(provide-theme 'ts-overrides)
