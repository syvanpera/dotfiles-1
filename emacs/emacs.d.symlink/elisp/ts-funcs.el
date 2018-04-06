;;; init.el --- My magnificent Emacs configuration

;;; Code:

(defun ts/minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defvar ts/not-to-kill-buffer-list '("*scratch*" "*Messages*" "*Warnings*"))

(defun ts/kill-window-or-buffer ()
  "Kills the current window if more than one with same buffer open, otherwise kills the buffer."
  (interactive)
  (if (member (buffer-name (current-buffer)) ts/not-to-kill-buffer-list)
      (delete-window)
    (if (= 1 (length (get-buffer-window-list (current-buffer))))
        (kill-buffer-and-window)
        (delete-window))))

(defun ts/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

(defun ts/hide-cursor-in-helm-buffer ()
  "Hide the cursor in helm buffers."
  (with-helm-buffer
    (setq cursor-in-non-selected-windows nil)))

(defun ts/edit-configuration ()
  "Opens Emacs configuration file."
  (interactive)
  (find-file user-init-file))

(defun ts/load-project-org (project-name)
  "Open project specific org file."
  (interactive)
  (find-file (expand-file-name (format "projects/%s.org" project-name) org-directory)))

(defun ts/load-configuration ()
  "Load the Emacs configuration file."
  (interactive)
  (load-file user-init-file))

(defun ts/contextual-helm-find-files (args)
  "Does helm-find-files or helm-projectile-find-file depending on whether we are in a project or not."
  (interactive "P")
  (if (projectile-project-p)
      (helm-projectile-find-file args)
    (helm-find-files args)))

(defun ts/contextual-helm-ag (args)
  "Does helm-ag or helm-projectile-ag depending on whether we are in a project or not."
  (interactive "P")
  (if (projectile-project-p)
      (helm-projectile-ag args)
    (helm-ag args)))

(defun ts/contextual-helm-recentf ()
  "Does helm-recentf or helm-projectile-recentf depending on whether we are in a project or not."
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-recentf)
    (helm-recentf)))

(defun ts/contextual-shell-pop (args)
  "Does shell-pop or ts/projectile-shell-pop depending on whether we are in a project or not."
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

(defun ts/org-agenda-show-agenda-and-todo (&optional arg)
  (interactive "P")
  (org-agenda arg "n"))

(defun ts/eshell-evil-input-mode ()
  (interactive)
  (eshell-return-to-prompt)
  (evil-insert nil))

(defun ts/helm-find-org-files ()
  (interactive)
  (message org-directory)
  (helm-find-files-1 (concat org-directory "/")))

(defun ts/projectile-shell-pop ()
  "Open a term buffer at projectile project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-interactively 'shell-pop)))

(defun ts/open-create-scratch-buffer ()
  "Opens (and creates) a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun ts/contextual-neotree-toggle ()
  "Opens the neotree with project root as the root directory if inside a project."
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

(provide 'ts-funcs)
