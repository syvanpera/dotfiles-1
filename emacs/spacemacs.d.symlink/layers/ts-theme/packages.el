;;; packages.el --- ts-theme layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Tuomo Syvänperä <tinimini@coruscant.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst ts-theme-packages
  '(doom-themes
    oceanic-theme))

(defun ts-theme/init-doom-themes ()
  (use-package doom-themes
    :init
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)
    :config
    ;; (doom-themes-visual-bell-config)
    (doom-themes-org-config)))

(defun ts-theme/init-oceanic-theme ()
  (use-package oceanic-theme))


;;; packages.el ends here
