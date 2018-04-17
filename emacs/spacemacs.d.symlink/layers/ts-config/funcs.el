;;; funcs.el --- ts-config layer funcs file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Tuomo Syvänperä <tinimini@coruscant.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defvar ts/not-to-kill-buffer-list '("*scratch*" "*Messages*" "*spacemacs*") "List of buffers that shouldn't be killed.")

(defun ts/line-numbers-absolute ()
  "Set absolute line numbering."
  (setq-local display-line-numbers t))

(defun ts/line-numbers-relative ()
  "Set relative line numbering."
  (setq-local display-line-numbers 'relative))

(defun ts/eshell-quit-or-delete-char (arg)
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp))
      (progn
        (eshell-life-is-too-much)
        (ignore-errors
          (delete-window)))
    (delete-forward-char arg)))

(defun ts/kill-window-or-buffer ()
  "Kills the current window if more than one with same buffer open, otherwise kills the buffer."
  (interactive)
  (if (member (buffer-name (current-buffer)) ts/not-to-kill-buffer-list)
      (if (= 1 (length (window-list)))
          (bury-buffer)
        (delete-window))
    (if (= 1 (length (get-buffer-window-list (current-buffer))))
        (kill-buffer-and-window)
      (delete-window))))

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


;;; funcs.el ends here
