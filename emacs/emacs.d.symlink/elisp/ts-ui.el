(setq display-line-numbers-type 'relative
      custom-safe-themes t)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

;;(global-display-line-numbers-mode)
(global-hl-line-mode 1)
(show-paren-mode)

; (use-package rainbow-mode
;   :config
;   (add-hook 'css-mode-hook 'rainbow-mode)
;   (add-hook 'scss-mode-hook 'rainbow-mode))
; (use-package rainbow-delimiters)

(use-package all-the-icons)

(defvar ts/theme-directory (concat user-emacs-directory "themes"))

(if (not (file-exists-p ts/theme-directory))
    (make-directory ts/theme-directory t))

(add-to-list 'custom-theme-load-path ts/theme-directory)

(use-package zenburn-theme :ensure t)
(use-package solarized-theme :ensure t)
(use-package dracula-theme :ensure t)
(use-package doom-themes :ensure t)

;;(setq solarized-distinct-fringe-background t)

;;(load-theme 'zenburn)
;;(load-theme 'solarized-dark)
;;(load-theme 'solarized-dark-overrides)
(load-theme 'dracula)

;; Doom theme specific settings
;; Global settings (defaults)
;;(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
;;(load-theme 'doom-one t)

;; Enable flashing mode-line on errors
;;(doom-themes-visual-bell-config)

;; Enable custom neotree theme
;;(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

;; Corrects (and improves) org-mode's native fontification.
;;(doom-themes-org-config)

(provide 'ts-ui)
