;;; +funcs.el -*- lexical-binding: t; -*-

(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the current window."
  (interactive)
  (let ((current-buffer (window-buffer window)))
    ;; if no window is found in the windows history, `switch-to-buffer' will
    ;; default to calling `other-buffer'.
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window))))))

(defun spacemacs/switch-to-scratch-buffer (&optional arg)
  "Switch to the `*scratch*' buffer, creating it first if needed.
if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (let ((exists (get-buffer "*scratch*")))
    (if arg
        (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
      (switch-to-buffer (get-buffer-create "*scratch*")))
    (when (and (not exists)
               (not (eq major-mode ts-scratch-mode))
               (fboundp ts-scratch-mode))
      (funcall ts-scratch-mode))))

(defun spacemacs/switch-to-messages-buffer (&optional arg)
  "Switch to the `*Messages*' buffer.
if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (with-current-buffer (messages-buffer)
    (goto-char (point-max))
    (if arg
        (switch-to-buffer-other-window (current-buffer))
      (switch-to-buffer (current-buffer)))))

(defun ts/open-org-inbox ()
  "Open org inbox file."
  (interactive)
  (find-file org-default-notes-file))

(defun ts/open-config-file ()
  "Open custom config file"
  (interactive)
  (find-file (concat doom-private-dir "/config.el")))

(defun ts/truncate-eshell-buffers ()
  "Truncates all eshell buffers"
  (interactive)
  (save-current-buffer
    (dolist (buffer (buffer-list t))
      (set-buffer buffer)
      (when (eq major-mode 'eshell-mode)
        (eshell-truncate-buffer)))))


(defun ts/display-new-buffer (buffer force-other-window)
  "If BUFFER is visible, select it.
If it's not visible and there's only one window, split the
current window and select BUFFER in the new window. If the
current window (before the split) is more than 100 columns wide,
split horizontally(left/right), else split vertically(up/down).
If the current buffer contains more than one window, select
BUFFER in the least recently used window.
This function returns the window which holds BUFFER.
FORCE-OTHER-WINDOW is ignored."
  (or (get-buffer-window buffer)
      (if (one-window-p)
          (let ((new-win
                 (if (> (window-width) 100)
                     (split-window-horizontally)
                   (split-window-vertically))))
            (set-window-buffer new-win buffer)
            new-win)
        (let ((new-win (get-lru-window)))
          (set-window-buffer new-win buffer)
          new-win))))


(defun ts/project-search-helm (arg)
  "Performs a project search using the first available search backend from a
list of: ripgrep, ag, pt, git-grep and grep. If ARG (universal argument),
preform search from current directory."
  (interactive "P")
  (call-interactively
   (cond ((executable-find "rg") (if arg #'+helm/rg-from-cwd #'+helm/rg))
         ((executable-find "ag") (if arg #'+helm/ag-from-cwd #'+helm/ag))
         ((executable-find "pt") (if arg #'+helm/pt-from-cwd #'+helm/pt))
         (arg #'+helm/grep-from-cwd)
         (#'+helm/grep))))

(defun ts/project-search-ivy (arg)
  "Performs a project search using the first available search backend from a
list of: ripgrep, ag, pt, git-grep and grep. If ARG (universal argument),
preform search from current directory."
  (interactive "P")
  (call-interactively
   (cond ((executable-find "ag") (if arg #'+ivy/ag-from-cwd #'+ivy/ag))
         ((executable-find "rg") (if arg #'+ivy/rg-from-cwd #'+ivy/rg))
         ((executable-find "pt") (if arg #'+ivy/pt-from-cwd #'+ivy/pt))
         (arg #'+ivy/grep-from-cwd)
         (#'+ivy/grep))))

(defun ts/projectile-find-file-dwim-other-window-h ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action
     (lambda (candidate)
       (let ((split-height-threshold 0)
             (split-width-threshold nil))
         (helm-find-files-other-window candidate))))))

(defun ts/projectile-find-file-dwim-other-window-v ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action
     (lambda (candidate)
       (let ((split-height-threshold nil)
             (split-width-threshold 0))
         (helm-find-files-other-window candidate))))))
