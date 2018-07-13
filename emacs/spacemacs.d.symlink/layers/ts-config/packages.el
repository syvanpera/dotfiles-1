;;; packages.el --- ts-config layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Tuomo Syvänperä <tuomo.syvanpera@gmail.com>
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
    helm-org-rifle
    flycheck-posframe
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

(defun ts-config/init-helm-org-rifle ()
  (use-package helm-org-rifle
    :defer t
    :after helm))

(defun ts-config/init-flycheck-posframe ()
  (use-package flycheck-posframe
    :defer t
    :after flycheck
    :hook ((flycheck-mode) . flycheck-posframe-mode)
    :config (flycheck-posframe-configure-pretty-defaults)))

(defun ts-config/init-multi-scratch ()
  (use-package multi-scratch
    :init (setq multi-scratch-buffer-name "scratch - ")))

(defun ts-config/init-org-config ()
  (use-package org-config))


;;; packages.el ends here
