;;; +hydras.el -*- lexical-binding: t; -*-

(after! hydra
  (def-hydra! ts@git-hydra (:hint t :color red)
    "
      Git transient state:
      [_j_/_n_] next  [_s_] stage   [_v_] show
      [_k_/_p_] prev  [_r_] revert  [_q_]^^ quit
"
    ("j" git-gutter:next-hunk)
    ("k" git-gutter:previous-hunk)
    ("n" git-gutter:next-hunk)
    ("p" git-gutter:previous-hunk)
    ("v" git-gutter:popup-hunk)
    ("s" git-gutter:stage-hunk)
    ("r" git-gutter:revert-hunk)
    ("q" nil :exit t)))


(provide '+hydras)
;;; +hydras.el ends here
