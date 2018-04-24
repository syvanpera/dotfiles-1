(require 'helm)
(require 'eshell)
(require 'projectile)

(defvar veikkaus/build-cmd "BUILD_SPEC=0 ./gulp --buildPages %s")
(defvar veikkaus/base-path "~/work/veikkaus/web/")
(defvar veikkaus/page-path (concat veikkaus/base-path "src/modules/pages"))
(defvar veikkaus/build-buffer-name "*Veikkaus - build*")

(defun veikkaus/build (candidate)
  (let ((cmd (format veikkaus/build-cmd (string-join (helm-marked-candidates) ","))))
    (projectile-with-default-dir (projectile-project-root)
      (if (get-buffer veikkaus/build-buffer-name)
          (delete-process "*Veikkaus - build*"))

      (let ((eshell-buffer-name veikkaus/build-buffer-name))
        (eshell)
        (insert cmd)
        (eshell-send-input)))))

(defun veikkaus/helm-page-source ()
  (helm-build-sync-source "Veikkaus pages"
    :candidates (directory-files veikkaus/page-path nil directory-files-no-dot-files-regexp)
    :action '(("Build" . veikkaus/build))))

(defun veikkaus/helm-pages ()
  (interactive)
  (if (string= (projectile-project-name) "web")
      (helm :sources (veikkaus/helm-page-source)
            :buffer "*veikkaus pages*")
    (message "You are not in Veikkaus project.")))

(provide 'ts-veikkaus)
