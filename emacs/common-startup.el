;; Note on a fresh system install will need to install use-package
;;M-x package-refresh-contents RET M-x package-install RET use-package RET

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

;; Set the cursor according to the edit mode.
(require 'set-cursor-according-to-mode)
(add-hook 'post-command-hook 'djcb-set-cursor-according-to-mode)

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
