;;; ts-buffer.el --- Buffer related functions and stuff

;;; Code:

(defvar ts/not-to-kill-buffer-list '("*scratch*" "*Messages*" "*Warnings*" "*dashboard*") "List of buffers that shouldn't be killed.")

(defun ts/indent-buffer ()
  "Re-indent the current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun ts/untabify-buffer ()
  "Convert tabs to spaces in the current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun ts/open-create-scratch-buffer ()
  "Open and/or create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun ts/create-scratch-buffer nil
  "Create a new scratch buffer to work in. (could be *scratch* - *scratch - X*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (concat " - " (int-to-string n)))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)
    ))

(defun ts/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the current frame."
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

(provide 'ts-buffer)
