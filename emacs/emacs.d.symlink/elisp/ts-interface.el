(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

  :config
  (evil-leader/set-key
    "e"    'neotree-toggle
    ))

(provide 'ts-interface)
