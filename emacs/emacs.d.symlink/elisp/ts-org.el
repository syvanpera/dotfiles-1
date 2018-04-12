;;; ts-org.el --- Org mode related functions and stuff

;;; Code:

(defun ts/open-org-inbox ()
  "Open org inbox file."
  (interactive)
  (find-file org-default-notes-file))

(defun ts/org-agenda-show-agenda-and-todo (&optional arg)
  (interactive "P")
  (org-agenda arg "n"))

(defun ts/helm-find-org-files ()
  "Find and show org files in helm."
  (interactive)
  (helm-find-files-1 (concat org-directory "/")))

(defun ts/load-project-org (project-name)
  "Open project specific org file."
  (interactive)
  (find-file (expand-file-name (format "projects/%s.org" project-name) org-directory)))

(provide 'ts-org)
