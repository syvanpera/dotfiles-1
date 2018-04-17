;;; packages.el --- ts-config layer packages file for Spacemacs.
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

(defconst ts-config-packages
  '(persistent-scratch
    highlight-indent-guides
    ;; spaceline-all-the-icons
    ;; beacon
    helm-org-rifle
    (multi-scratch :location local)
    (org-config :location local)))

(defun ts-config/init-persistent-scratch ()
  (use-package persistent-scratch
    :init  (setq persistent-scratch-scratch-buffer-p-function
                 (lambda () (string-prefix-p "*scratch" (buffer-name))))
    :config (persistent-scratch-setup-default)))

(defun ts-config/init-highlight-indent-guides ()
  (use-package highlight-indent-guides
    :defer t
    :hook ((coffee-mode) . highlight-indent-guides-mode)
    :init (setq highlight-indent-guides-method 'character)))

;; (defun ts-config/pre-init-spaceline-all-the-icons ()
;;     (setq spaceline-all-the-icons-highlight-file-name t
;;         ;; spaceline-all-the-icons-hide-inactive-separators t
;;         spaceline-all-the-icons-icon-set-eyebrowse-slot 'square))

;; (defun ts-config/post-init-spaceline-all-the-icons ()
;;   (spaceline-all-the-icons--setup-git-ahead)
;;   (spaceline-toggle-all-the-icons-buffer-position-on)
;;   (spaceline-toggle-all-the-icons-region-info-on)
;;   (spaceline-toggle-all-the-icons-eyebrowse-workspace-off))

;; (defun ts-config/init-beacon ()
;;   (use-package beacon
;;     :defer t
;;     :init
;;     (setq beacon-color "#51afef")
;;     :config (beacon-mode 1)))

(defun ts-config/init-helm-org-rifle ()
  (use-package helm-org-rifle
    :defer t
    :after helm))

(defun ts-config/init-multi-scratch ()
  (use-package multi-scratch
    :init (setq multi-scratch-buffer-name "scratch - ")))

(defun ts-config/init-org-config ()
  (use-package org-config))


;;; packages.el ends here
