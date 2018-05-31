;; Note on a fresh system install will need to install use-package
;;M-x package-refresh-contents RET M-x package-install RET use-package RET

;; Change cursor color according to mode; inspired by
;; http://www.emacswiki.org/emacs/ChangingCursorDynamically
 (setq djcb-read-only-color       "pink")
;; valid values are t, nil, box, hollow, bar, (bar . WIDTH), hbar,
 ;; (hbar. HEIGHT); see the docs for set-cursor-type
 (setq djcb-read-only-cursor-type 'hbar)
 (setq djcb-overwrite-color       "red")
 (setq djcb-overwrite-cursor-type 'box)
 (setq djcb-normal-color          "grey")
 (setq djcb-normal-cursor-type    'bar)

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
(add-hook 'post-command-hook 'djcb-set-cursor-according-to-mode)

;;Set the color scheme
(use-package darcula-theme
  :ensure t
  :config
  ;; your preferred main font face here
  (set-frame-font "Monospace-12"))

;;EMACS setup
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)

;;Disable the bell
(setq ring-bell-function 'ignore)

;;Store backups to temp
(setq backup-directory-alist
      `((".*" . ,"/tmp")))

;;GUI Setup
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(setq inhibit-startup-screen t)
(put 'upcase-region 'disabled nil)

;; display only tails of lines longer than 80 columns, tabs and
;; trailing whitespaces
(setq whitespace-line-column 80
      whitespace-style '(face tabs trailing lines-tail))

;; activate whitespace modes when appropriate
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'rst-mode-hook 'whitespace-mode)

;; Setup for Flymake code checking.
(require 'flymake)
(load-library "flymake-cursor")

;;Start emacs server so new windows open in existing frame
(load "server")
(unless (server-running-p) (server-start))

(require 'ido)
(ido-mode t)

;;Revbufs setup and keybinding
(require 'revbufs)
(global-set-key [f12] (quote revbufs))

;;Smooth Scrolling
(require 'smooth-scrolling)
(global-set-key (kbd "<C-mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<C-mouse-5>") 'scroll-up-line)

;; Latex Preview
;; Available in elpa `M-x package-install latex-preview-pane`
;;(latex-preview-pane-enable)

(defun my-c-mode-font-lock-if0 (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)

(defun my-c-mode-common-hook ()
  (font-lock-add-keywords
   nil
   '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Setup javascript formatting
(setq js-indent-level 2)
