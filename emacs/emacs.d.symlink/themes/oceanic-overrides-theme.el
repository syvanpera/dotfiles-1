(deftheme oceanic-overrides
  "Oceanic theme overrides.")

(custom-theme-set-faces
 'oceanic-overrides
 '(line-number-current-line ((t (:foreground "white" :background "#343D46"))))
 ;; '(spaceline-evil-normal ((t (:background "#99C794"))))
 ;; '(spaceline-evil-insert ((t (:background "#99C794"))))
 ;; '(spaceline-evil-visual ((t (:background "dark orange"))))
 '(fringe ((t (:background "#343D46"))))
 ;; '(region ((t (:background "wheat"))))
 '(region ((t (:background "#6699CC"))))
 '(mode-line ((t (:background "#6699CC" :foreground "#1B2B34" :box nil :weight normal))))
 '(company-tooltip ((t :inherit default :background "#343D46")))
 '(company-scrollbar-bg ((t :background "#232526")))
 '(company-scrollbar-fg ((t :background "#99C794")))
 '(company-tooltip-selection ((t :background "#6699CC")))
 '(company-tooltip-common ((t :inherit font-lock-constant-face)))
 '(font-lock-comment-face ((t (:foreground "#65737E" :weight normal)))))

(provide-theme 'oceanic-overrides)
