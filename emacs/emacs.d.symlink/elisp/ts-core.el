(require 'ts-funcs)

(defvar ts/backup-directory (concat user-emacs-directory "backups"))

(if (not (file-exists-p ts/backup-directory))
    (make-directory ts/backup-directory t))

(setq backup-directory-alist `(("." . ,ts/backup-directory)))

(setq make-backup-files nil
      backup-by-copying t
      version-control nil
      delete-old-versions t
      delete-by-moving-to-trash nil
      create-lockfiles nil
      kept-old-versions 1
      kept-new-versions 1)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil
      ring-bell-function 'ignore)

(fset 'yes-or-no-p 'y-or-n-p)

(setq mac-command-modifier 'meta
      mac-option-modifier  'alt
      mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll t
      ;; mouse-wheel-scroll-amount '(5 ((shift) . 2))  ; one line at a time
      ;; mouse-wheel-progressive-speed nil             ; don't accelerate scrolling
      ;; ns-use-native-fullscreen nil
      ns-alternate-modifier 'none)

(provide 'ts-core)
