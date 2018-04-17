;;; config.el --- ts-theme layer config file for Spacemacs.
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

(defvar ts-theme/sanityinc-tomorrow-eighties-theming
  '(sanityinc-tomorrow-eighties
    (highlight :background "#515151")
    ;; (highlight :background "#2d4948")
    (highlight-indentation-face :background "#444444")
    (highlight-indentation-current-column-face :background "#555555")
    (line-number-current-line :foreground "#f99157" :background "#515151" :weight bold)
    ;; (line-number-current-line :foreground "#cae682" :background "#515151" :weight bold)
    (region :background "#338f86")
    (org-todo :weight bold :foreground "#ec5f67")
    (org-headline-done :foreground "#5d7978" :strike-through nil)
    (org-document-title :foreground "#f99157" :weight bold :height 1.2)
    (org-level-1 :inherit 'outline-1 :height 1.1)
    (spacemacs-insert-face :background "#cae682" :foreground "#515151" :inherit 'mode-line)
    (helm-source-header :foreground "#f99157" :height 1.2)
    (helm-visible-mark :background "#338f86" :foreground "white")
    (neo-dir-link-face :foreground "DeepSkyBlue2" :family "M+ 1m")
    (neo-file-link-face :foreground "#f3f4f6" :family "M+ 1m")
    (neo-banner-face :foreground "lightblue" :weight bold :family "M+ 1m")
    (neo-root-dir-face :foreground "lightblue" :weight bold :family "M+ 1m")
    ))

(defvar ts-theme/zenburn
  '(zenburn
    ;; (default :foreground "#DCDCCC" :background "#383838")
    (hl-line :background "#4f4f4f")
    (region :background "#7F9F7F")
    (highlight-indentation-face :background "#494949")
    (highlight-indentation-current-column-face :background "#5f5f5f")
    (fringe :background "#3f3f3f")
    (lazy-highlight :foreground "#DC8CC3")
    (line-number :foreground "#858575" :background "#3f3f3f")
    (line-number-current-line :foreground "#f99157" :background "#4f4f4f" :weight bold)
    (font-lock-comment-face :foreground "#888888")
    (font-lock-comment-delimiter-face :foreground "#888888")
    ;; (font-lock-string-face :foreground "#DCA3A3")
    (font-lock-keyword-face :foreground "#F0DFAF" :weight bold)
    (spacemacs-insert-face :background "#7F9F7F" :foreground "#515151" :inherit 'mode-line)
    (neo-dir-link-face :foreground "#94BFF3" :family "M+ 1m")
    (neo-file-link-face :foreground "#FFFFEF" :family "M+ 1m")
    (neo-banner-face :foreground "#ACE0E3" :weight bold :family "M+ 1m")
    (neo-root-dir-face :foreground "#ACE0E3" :weight bold :family "M+ 1m")
    ;; (js2-function-call :foreground "#94BFF3")
    ;; (js2-object-property :foreground nil :weight bold)
    ;; (js2-object-property-access :foreground nil)
    ))

(defvar ts-theme/doom-one
  '(doom-one
    (hl-line :background "#3B4248")
    (fringe :foreground "#6b7278" :background "#1B2B34")
    (region :background "#5B6268")
    (line-number :inherit default :foreground "#6b7278" :strike-through nil :underline nil :slant normal :weight normal)
    (line-number-current-line :inherit default :foreground "#fac863" :background "#3B4248" :weight bold)
    (ahs-plugin-whole-buffer-face :background "#2257a0" :foreground "#dfdfdf")
    (spacemacs-insert-face :background "#99c794" :inherit 'spacemacs-normal-face)
    ;; (spacemacs-visual-face :background "#3da5ad" :inherit 'spacemacs-normal-face)
    (font-lock-builtin-face :foreground "#c594c5")
    (font-lock-function-name-face :foreground "#c594c5")
    (font-lock-variable-name-face :foreground nil)
    (font-lock-comment-face :foreground "#6b7278")
    (font-lock-string-face :foreground "#99c794")
    (sp-show-pair-match-face :background "#c678dd" :foreground "#23272e")
    (mode-line :background "#4f545a" :foreground "#cccccc" :box (:line-width 1 :color "#3f444a" :style released-button) :weight normal)
    (powerline-active2 :background "#4f545a" :foreground "#cccccc" :weight normal)
    (neo-banner-face :foreground "lightblue" :weight bold :family "M+ 1m")
    (neo-dir-link-face :foreground "#51afef" :family "M+ 1m")
    (neo-file-link-face :foreground "#bbc2cf" :family "M+ 1m")
    (neo-root-dir-face :background "#282c34" :foreground "#99c794" :box (:line-width 4 :color "#282c34") :family "M+ 1m")
    (treemacs-directory-face :foreground "#51afef" :family "M+ 1m")
    (treemacs-file-face :foreground "#bbc2cf" :family "M+ 1m")
    (treemacs-header-face :foreground "#99c794" :family "M+ 1m" :height 1.2 :underline t)
    ;; (org-block :background "#ecbe7b")
    (org-block-begin-line :background "#3f444a" :foreground "#dfdfdf" :underline t)
    (org-block-end-line :background "#3f444a" :foreground "#dfdfdf" :underline t)
    (org-todo :foreground "#ec5f67" :weight bold)
    (org-done :foreground "#99c794" :weight bold)
    (org-headline-done :foreground "#888888" :strike-through nil)
    (org-ellipsis :foreground "#888888" :underline nil)
    (org-level-1 :foreground "#51afef" :weight semi-bold :height 1.2)
    (org-level-2 :foreground "#c594c5" :weight semi-bold)
    (org-level-3 :foreground "#a9a1e1" :weight semi-bold)
    (org-level-4 :foreground "#7cc3f3" :weight semi-bold)
    ))

(defvar ts-theme/oceanic
  '(oceanic
    (default :foreground "gray85" :background "#1b2b34")
    ;; (default ((t (:foreground "gray85" :background "#242730")
    (fringe :background "#1b2b34")
    (cursor :background "#51afef")
    (ahs-plugin-whole-buffer-face :background "#2257a0" :foreground "#dfdfdf")
    ;; (line-number-current-line :foreground "#fac863" :background "#3d5958" :weight bold)
    (line-number-current-line :foreground "#fac863" :background "#3d5958" :weight bold)
    (line-number :foreground "#7d9998" :background nil)
    (highlight :background "#3d5958" :foreground nil)
    (highlight-indentation-face :background "#3d5958")
    (highlight-indentation-current-column-face :background "#4d6968")
    ;; (region :background "#99c794" :foreground "#f3f4f6")
    (region :background "#3d5958" :foreground "#f3f4f6")
    (flycheck-fringe-error :foreground "systemRedColor")
    (flycheck-fringe-warning :foreground "DarkGoldenrod2")
    (flycheck-fringe-info :foreground "#99c794")
    (spacemacs-insert-face :foreground "#4f545a" :background "#99c794" :inherit 'mode-line)
    (spacemacs-visual-face :foreground "#4f545a" :background "gray" :inherit 'mode-line)
    (spacemacs-normal-face :foreground "#4f545a" :background "#fac863" :inherit 'mode-line)
    (spacemacs-emacs-face :foreground "#4f545a" :background "#cc99cc" :inherit 'mode-line)
    (spacemacs-motion-face :foreground "#4f545a" :background "#cc99cc" :inherit 'mode-line)
    (spacemacs-evilified-face :foreground "#4f545a" :background "LightGoldenrod3" :inherit 'mode-line)
    (mode-line :background "#4f545a" :foreground "#cccccc" :box (:line-width 1 :color "#3f444a" :style released-button) :weight normal)
    (powerline-active2 :background "#4f545a" :foreground "#cccccc" :weight normal)
    (show-paren-match :foreground: nil :background "white")
    (helm-selection :background "#3d5958" :weight bold)
    (helm-source-header :background nil :foreground "#fac863" :weight bold :height 1.1 :family "Menlo")
    (helm-visible-mark :background "#ffbd29" :foreground "black")
    (helm-candidate-number :background "#ffbd29" :foreground "black")
    (helm-buffer-directory :background "#5d7978" :foreground "##fac863")
    (helm-header :background "#5d7978" :foreground "##fac863")
    (helm-ff-file :background nil :foreground "gray80")
    (helm-ff-directory :background nil :foreground "#6699cc")
    (helm-ff-executable :background nil :foreground "#99c794")
    (helm-ff-dotted-directory :background nil :foreground "#6699cc")
    (helm-ff-prefix :background "#ffbd29" :foreground "#fac863")
    (company-tooltip :inherit default :background "#3d5958")
    (company-scrollbar-bg :background "#232526")
    (company-scrollbar-fg :background "#338f86")
    (company-tooltip-selection :background "#5d7978")
    (company-tooltip-common :inherit font-lock-constant-face)
    (company-tooltip-annotation :foreground "#fac863")
    (org-todo :weight bold :foreground "#ec5f67")
    (org-done :weight bold :foreground "#99c794")
    (org-headline-done :foreground "#5d7978" :strike-through nil)
    ;; (org-headline-done :foreground "#888888" :strike-through nil)
    (org-level-1 :height 1.2 :inherit 'outline-1 :foreground "#dbe2ef" :weight semi-bold)
    (org-level-2 :inherit 'outline-2 :foreground "#6699cc" :weight semi-bold)
    (org-level-3 :inherit 'outline-3 :foreground "#fac863" :weight semi-bold)
    (org-level-4 :inherit 'outline-2 :foreground "#99c794" :weight semi-bold)
    (org-archived :underline t)
    (org-verbatim :foreground "#65737e")
    (org-hide :foreground "#2a2e38")
    (org-block :background "#2B3B44")
    (org-block-begin-line :background "#3f444a" :foreground "#dfdfdf" :underline t)
    (org-block-end-line :background "#3f444a" :foreground "#dfdfdf" :underline t)
    (org-ellipsis :foreground "#888888" :underline nil)
    (solaire-default-face :inherit 'default :background "#2a2e38")
    (tide-hl-identifier-face :foreground "#fac863")
    (font-lock-comment-face :foreground "#65737e")
    (font-lock-function-name-face :foreground "#6699cc")
    (font-lock-string-face :foreground "#99c794")
    (font-lock-keyword-face :foreground "#5FB3B3")
    (js2-object-property :foreground "#f3f4f6")
    ;; (js2-object-property :foreground "#fac863" :weight normal)
    (js2-function-call :foreground "#6699cc")
    ;; (js2-function-call :foreground "#94BFF3")
    (rainbow-delimiters-depth-1-face :foreground "#6699cc")
    (rainbow-delimiters-depth-2-face :foreground "#ec5f67")
    (rainbow-delimiters-depth-3-face :foreground "#99c794")
    (rainbow-delimiters-depth-4-face :foreground "#fac863")
    (rainbow-delimiters-depth-5-face :foreground "#cc99cc")
    (rainbow-delimiters-depth-6-face :foreground "#fae683")
    (rainbow-delimiters-depth-7-face :foreground "#338f86")
    (rainbow-delimiters-unmatched-face :foreground "red" :weight bold :inverse-video t)
    (rainbow-delimiters-mismatched-face :inherit 'rainbow-delimiters-unmatched-face)
    (neo-banner-face :foreground "lightblue" :weight bold :family "M+ 1m")
    (neo-dir-link-face :foreground "#51afef" :family "M+ 1m")
    (neo-file-link-face :foreground "#bbc2cf" :family "M+ 1m")
    (neo-root-dir-face :background "#282c34" :foreground "#99c794" :box (:line-width 4 :color "#282c34") :family "M+ 1m")
    (treemacs-directory-face :foreground "#51afef" :family "M+ 1m")
    (treemacs-file-face :foreground "#bbc2cf" :family "M+ 1m")
    (treemacs-header-face :foreground "#99c794" :family "M+ 1m" :height 1.2 :underline t)
    ))

(setq theming-modifications (list ts-theme/sanityinc-tomorrow-eighties-theming ts-theme/zenburn ts-theme/doom-one ts-theme/oceanic))

;; let s:base00=['#1b2b34', '235']
;; let s:base01=['#343d46', '237']
;; let s:base02=['#4f5b66', '240']
;; let s:base03=['#65737e', '243']
;; let s:base04=['#a7adba', '145']
;; let s:base05=['#c0c5ce', '251']
;; let s:base06=['#cdd3de', '252']
;; let s:base07=['#d8dee9', '253']
;; let s:base08=['#ec5f67', '203']
;; let s:base09=['#f99157', '209']
;; let s:base0A=['#fac863', '221']
;; let s:base0B=['#99c794', '114']
;; let s:base0C=['#62b3b2', '73']
;; let s:base0D=['#6699cc', '68']
;; let s:base0E=['#c594c5', '176']
;; let s:base0F=['#ab7967', '137']
;; let s:base10=['#ffffff', '15']

;;; config.el ends here

;; ((bg         '("#282c34" nil       nil            ))
;;    (bg-alt     '("#21242b" nil       nil            ))
;;    (base0      '("#1B2229" "black"   "black"        ))
;;    (base1      '("#1c1f24" "#1e1e1e" "brightblack"  ))
;;    (base2      '("#202328" "#2e2e2e" "brightblack"  ))
;;    (base3      '("#23272e" "#262626" "brightblack"  ))
;;    (base4      '("#3f444a" "#3f3f3f" "brightblack"  ))
;;    (base5      '("#5B6268" "#525252" "brightblack"  ))
;;    (base6      '("#73797e" "#6b6b6b" "brightblack"  ))
;;    (base7      '("#9ca0a4" "#979797" "brightblack"  ))
;;    (base8      '("#DFDFDF" "#dfdfdf" "white"        ))
;;    (fg         '("#bbc2cf" "#bfbfbf" "brightwhite"  ))
;;    (fg-alt     '("#5B6268" "#2d2d2d" "white"        ))

;;    (grey       base4)
;;    (red        '("#ff6c6b" "#ff6655" "red"          ))
;;    (orange     '("#da8548" "#dd8844" "brightred"    ))
;;    (green      '("#98be65" "#99bb66" "green"        ))
;;    (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
;;    (yellow     '("#ECBE7B" "#ECBE7B" "yellow"       ))
;;    (blue       '("#51afef" "#51afef" "brightblue"   ))
;;    (dark-blue  '("#2257A0" "#2257A0" "blue"         ))
;;    (magenta    '("#c678dd" "#c678dd" "magenta"      ))
;;    (violet     '("#a9a1e1" "#a9a1e1" "brightmagenta"))
;;    (cyan       '("#46D9FF" "#46D9FF" "brightcyan"   ))
;;    (dark-cyan  '("#5699AF" "#5699AF" "cyan"         ))
