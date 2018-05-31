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

;;GUI Setup
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(setq inhibit-startup-screen t)
(put 'upcase-region 'disabled nil)
(setq ring-bell-function 'ignore)

;; display only tails of lines longer than 80 columns, tabs and
;; trailing whitespaces
(setq whitespace-line-column 80
      whitespace-style '(face tabs trailing lines-tail))

;; Activate whitespace modes when appropriate
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'rst-mode-hook 'whitespace-mode)

;; Setup Flyspell
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Other programming mode hooks.
(add-hook 'prog-mode-hook 'show-paren-mode)

;; Store backups to temp instead of the current directory
(setq backup-directory-alist
      `((".*" . ,"/tmp")))

;; Setup for Flymake code checking.
(require 'flymake)
(load-library "flymake-cursor")

;; Start emacs server so new windows open in existing frame
(load "server")
(unless (server-running-p) (server-start))

;; IDO mode for interactive file browsing.
(require 'ido)
(ido-mode t)

;; Revbufs setup and keybinding
(require 'revbufs)
(global-set-key [f12] (quote revbufs))

;; Smooth Scrolling
(require 'smooth-scrolling)
(global-set-key (kbd "<C-mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<C-mouse-5>") 'scroll-up-line)

;; Latex Preview
;; Available in elpa `M-x package-install latex-preview-pane`
;;(latex-preview-pane-enable)

(require 'cmode-ifdef-comments)
(add-hook 'c-mode-common-hook 'c-mode-ifdef-comments-common-hook)

;; Setup javascript formatting
(setq js-indent-level 2)
