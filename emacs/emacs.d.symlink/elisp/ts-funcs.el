;;; ts-funcs.el --- Misc functions

;;; Code:

(defun ts/line-numbers-absolute ()
  "Set absolute line numbering."
  (setq-local display-line-numbers t))

(defun ts/line-numbers-relative ()
  "Set relative line numbering."
  (setq-local display-line-numbers 'relative))

(defun ts/toggle-relative-line-numbers ()
  "Toggles between relative and absolute line numbering."
  (interactive)
  (if (eq display-line-numbers 'relative)
      (ts/line-numbers-absolute)
    (ts/line-numbers-relative)))

(defun ts/minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun ts/hide-cursor-in-helm-buffer ()
  "Hide the cursor in helm buffers."
  (with-helm-buffer
    (setq cursor-in-non-selected-windows nil)))

(defun ts/edit-configuration ()
  "Opens Emacs configuration file."
  (interactive)
  (find-file user-init-file))

(defun ts/load-configuration ()
  "Load the Emacs configuration file."
  (interactive)
  (load-file user-init-file))

(defun ts/contextual-helm-find-files (args)
  "Do helm-find-files or helm-projectile-find-file depending on whether we are in a project or not."
  (interactive "P")
  (if (projectile-project-p)
      (helm-projectile-find-file args)
    (helm-find-files args)))

(defun ts/contextual-helm-ag (args)
  "Do helm-ag or helm-projectile-ag depending on whether we are in a project or not."
  (interactive "P")
  (if (projectile-project-p)
      (helm-projectile-ag args)
    (helm-ag args)))

(defun ts/contextual-helm-recentf ()
  "Do helm-recentf or helm-projectile-recentf depending on whether we are in a project or not."
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-recentf)
    (helm-recentf)))

(defun ts/contextual-shell-pop (args)
  "Do shell-pop or ts/projectile-shell-pop depending on whether we are in a project or not."
  (interactive "P")
  (if (projectile-project-p)
      (ts/projectile-shell-pop)
    (shell-pop args)))

(defun ts/paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(defun ts/evil-keyboard-quit ()
  "Keyboard quit and force normal state."
  (interactive)
  (and evil-mode (evil-force-normal-state))
  (keyboard-quit))

(defun ts/eshell-evil-input-mode ()
  (interactive)
  (eshell-return-to-prompt)
  (evil-insert nil))

(defun ts/projectile-shell-pop ()
  "Open a term buffer at projectile project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-interactively 'shell-pop)))

(defun ts/contextual-neotree-toggle ()
  "Open Neotree with project root as the root directory if inside a project."
  (interactive)
  (let ((project-dir
         (ignore-errors
           (projectile-project-root)
           ))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
        (neotree-hide)
      (progn
        (neotree-show)
        (if project-dir
            (neotree-dir project-dir))
        (if file-name
            (neotree-find file-name))))))

(defun ts/mu4e-alert-modeline-formatter (mail-count)
  "Mu4e-alert modeline formatter for spaceline-all-the-icons."
  (when (not (zerop mail-count))
    (let* ((icon (all-the-icons-faicon "envelope" :v-adjust 0.0)))
      (propertize
       (concat
        (propertize icon
                    'face `(:height ,(spaceline-all-the-icons--height 0.9) :inherit)
                    'display '(raise 0.1))
        (propertize (format " %d" mail-count) 'face `(:height ,(spaceline-all-the-icons--height 0.9) :inherit) 'display '(raise 0.1))
        (spaceline-all-the-icons--separator spaceline-all-the-icons-secondary-separator " "))
       'help-echo (concat (if (= mail-count 1)
                              "You have an unread email"
                            (format "You have %s unread emails" mail-count))
                          "\nClick here to view "
                          (if (= mail-count 1) "it" "them"))
       'mouse-face (spaceline-all-the-icons--highlight)
       'local-map (make-mode-line-mouse-map 'mouse-1 'mu4e-alert-view-unread-mails)))))

(provide 'ts-funcs)
