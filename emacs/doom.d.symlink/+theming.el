;;; +theming.el -*- lexical-binding: t; -*-

(require 'doom-one-theme)
(require 'doom-city-lights-theme)
(require 'doom-nord-theme)

(defun ts-theme-config ()
  (doom-themes-set-faces 'doom-one
    (line-number :foreground base5 :background bg)
    (line-number-current-line :foreground blue :background base4)
    (hl-line :background base4)
    (solaire-hl-line-face :background base4)
    (region :background base5 :foreground nil :distant-foreground (doom-darken fg 0.2))
    (org-level-1 :foreground blue :background base3 :weight 'semi-bold :height 1.2)
    (org-level-2 :foreground magenta :weight 'normal)
    (org-level-3 :foreground violet :weight 'normal)
    (org-level-4 :foreground (doom-lighten blue 0.25) :weight 'normal)
    (org-level-5 :foreground (doom-lighten magenta 0.25) :weight 'normal)
    (org-level-6 :foreground (doom-lighten blue 0.5) :weight 'normal)
    (org-level-7 :foreground (doom-lighten magenta 0.5) :weight 'normal)
    (org-level-8 :foreground (doom-lighten blue 0.8) :weight 'normal))

  (doom-themes-set-faces 'doom-city-lights
    (line-number :foreground base5 :background bg)
    (line-number-current-line :foreground blue :background base4)
    (region :background base5 :foreground nil :distant-foreground (doom-darken fg 0.2))
    (hl-line :background base4)
    (solaire-hl-line-face :inherit 'hl-line)
    (ivy-current-match :background base4 :distant-foreground nil)
    (show-paren-match :foreground bg :background orange)
    (tooltip :foreground fg :background base4)
    (mode-line :background base3 :foreground nil)
    (solaire-mode-line-face :inherit 'mode-line :background base3)
    (org-level-1 :foreground blue :background base3 :weight 'semi-bold :height 1.1)
    (org-level-2 :foreground yellow :weight 'normal)
    (org-level-3 :foreground orange :weight 'normal)
    (org-level-4 :foreground (doom-lighten blue 0.25) :weight 'normal)
    (org-level-5 :foreground (doom-lighten magenta 0.25) :weight 'normal)
    (org-level-6 :foreground (doom-lighten blue 0.5) :weight 'normal)
    (org-level-7 :foreground (doom-lighten magenta 0.5) :weight 'normal)
    (org-level-8 :foreground (doom-lighten blue 0.8) :weight 'normal)
    (org-ellipsis :foreground base6)
    (org-todo :weight 'bold :foreground red)
    (org-done :strike-through nil :weight 'bold :foreground green)
    (org-headline-done :strike-through nil :foreground base5)
    (org-property-value :foreground base5)
    (org-block :background base2)
    (org-block-begin-line :background base3 :foreground comments)
    (org-block-end-line :inherit 'org-block-begin-line)
    (font-lock-keyword-face :foreground blue)
    (font-lock-string-face :foreground green)
    (js2-object-property :foreground yellow))

  (doom-themes-set-faces 'doom-nord
    (default :background bg-alt)
    (solaire-default-face :inherit 'default :background bg)
    (hl-line :background base3)
    (variable-pitch :font "-xos4-terminus-bold-r-normal--12-120-72-72-c-60-iso10646-1")
    ;; (solaire-hl-line-face :inherit 'hl-line)
    ;; (line-number :foreground base5 :background base2)
    ;; (line-number-current-line :foreground blue :background base5)
    (show-paren-match :foreground bg-alt :background magenta)
    (ivy-current-match :foreground green :background base5 :distant-foreground nil)
    ;; (font-lock-variable-name-face :foreground (doom-lighten yellow 0.5))
    (font-lock-variable-name-face :foreground yellow)
    ;; (js2-function-param :foreground (doom-lighten yellow 0.5))
    (company-tooltip-annotation :foreground teal)
    (org-level-1 :foreground blue :background base3 :weight 'semi-bold :height 1.1)
    (org-level-2 :foreground magenta :weight 'normal)
    (org-level-3 :foreground violet :weight 'normal)
    (org-level-4 :foreground (doom-lighten blue 0.25) :weight 'normal)
    (org-level-5 :foreground (doom-lighten magenta 0.25) :weight 'normal)
    (org-level-6 :foreground (doom-lighten blue 0.5) :weight 'normal)
    (org-level-7 :foreground (doom-lighten magenta 0.5) :weight 'normal)
    (org-level-8 :foreground (doom-lighten blue 0.8) :weight 'normal)
    (org-block :background base1)
    (org-block-begin-line :background base2 :foreground comments)
    ;; (org-todo :weight 'bold :foreground red)
    (helm-source-header :foreground fg :background (doom-lighten base2 0.25))
    ))
