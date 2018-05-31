;; Change cursor color according to mode; inspired by
;; http://www.emacswiki.org/emacs/ChangingCursorDynamically
(setq djcb-read-only-color       "pink")
;; valid values are t, nil, box, hollow, bar, (bar . WIDTH), hbar,
 ;; (hbar. HEIGHT); see the docs for set-cursor-type
(setq djcb-read-only-cursor-type 'hbar)
(setq djcb-overwrite-color       "red")
(setq djcb-overwrite-cursor-type 'box)
(setq djcb-normal-color          "grey")
(setq djcb-normal-cursor-type    'box)

(defun djcb-set-cursor-according-to-mode ()
 "change cursor color and type according to some minor modes."

    (cond
        (buffer-read-only
            (set-cursor-color djcb-read-only-color)
            (setq cursor-type djcb-read-only-cursor-type))
        (overwrite-mode
            (set-cursor-color djcb-overwrite-color)
            (setq cursor-type djcb-overwrite-cursor-type))
        (t
            (set-cursor-color djcb-normal-color)
            (setq cursor-type djcb-normal-cursor-type)))
    )

(provide 'set-cursor-according-to-mode)
