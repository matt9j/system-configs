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

;; activate minor whitespace mode when in c++, python, rest, mode
;(add-hook 'c++-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'rst-mode-hook 'whitespace-mode)

;; Setup for Flymake code checking.
(require 'flymake)
(load-library "flymake-cursor")

;;Start emacs server so new windows open in existing frame
(load "server")
(unless (server-running-p) (server-start))
;;disable server close buffer confirm:
;;(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

(require 'ido)
(ido-mode t)

;;Revbufs setup and keybinding
(require 'revbufs)
(global-set-key [f12] (quote revbufs))

 ; Outline-minor-mode key map
 (define-prefix-command 'cm-map nil "Outline-")
 ; HIDE
 (define-key cm-map "q" 'hide-sublevels)    ; Hide everything but the top-level headings
 (define-key cm-map "t" 'hide-body)         ; Hide everything but headings (all body lines)
 (define-key cm-map "o" 'hide-other)        ; Hide other branches
 (define-key cm-map "c" 'hide-entry)        ; Hide this entry's body
 (define-key cm-map "l" 'hide-leaves)       ; Hide body lines in this entry and sub-entries
 (define-key cm-map "d" 'hide-subtree)      ; Hide everything in this entry and sub-entries
 ; SHOW
 (define-key cm-map "a" 'show-all)          ; Show (expand) everything
 (define-key cm-map "e" 'show-entry)        ; Show this heading's body
 (define-key cm-map "i" 'show-children)     ; Show this heading's immediate child sub-headings
 (define-key cm-map "k" 'show-branches)     ; Show all sub-headings under this heading
 (define-key cm-map "s" 'show-subtree)      ; Show (expand) everything in this heading & below
 ; MOVE
 (define-key cm-map "u" 'outline-up-heading)                ; Up
 (define-key cm-map "n" 'outline-next-visible-heading)      ; Next
 (define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
 (define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
 (define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level
 (global-set-key "\M-o" cm-map)

 (define-key cm-map "\M-o" 'outline-minor-mode)       ; Backward - same level

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

(c-add-style "e-and-m-c"
             '((c-basic-offset . 4)     ; Guessed value
               (c-offsets-alist
                (block-close . 0)       ; Guessed value
                (brace-list-close . 0)  ; Guessed value
                (brace-list-entry . 0)  ; Guessed value
                (brace-list-intro . +)  ; Guessed value
                (brace-list-open . 0)   ; Guessed value
                (case-label . +)        ; Guessed value
                (class-close . 0)       ; Guessed value
                (class-open . 0)        ; Guessed value
                (defun-block-intro . +) ; Guessed value
                (defun-close . 0)       ; Guessed value
                (defun-open . 0)        ; Guessed value
                (else-clause . 0)       ; Guessed value
                (inclass . +)           ; Guessed value
                (statement . 0)             ; Guessed value
                (statement-block-intro . +) ; Guessed value
                (statement-case-open . 0) ; Guessed value
                (statement-cont . +)      ; Guessed value
                (substatement-open . 0)   ; Guessed value
                (topmost-intro . 0)       ; Guessed value
                (access-label . -)
                (annotation-top-cont . 0)
                (annotation-var-cont . +)
                (arglist-close . c-lineup-close-paren)
                (arglist-cont c-lineup-gcc-asm-reg 0)
                (arglist-cont-nonempty . c-lineup-arglist)
                (arglist-intro . c-lineup-arglist-intro-after-paren)
                (block-open . 0)
                (brace-entry-open . 0)
                (c . c-lineup-C-comments)
                (catch-clause . 0)
                (comment-intro . c-lineup-comment)
                (composition-close . 0)
                (composition-open . 0)
                (cpp-define-intro c-lineup-cpp-define +)
                (cpp-macro . -1000)
                (cpp-macro-cont . +)
                (do-while-closure . 0)
                (extern-lang-close . 0)
                (extern-lang-open . 0)
                (friend . 0)
                (func-decl-cont . +)
                (incomposition . +)
                (inexpr-class . +)
                (inexpr-statement . +)
                (inextern-lang . +)
                (inher-cont . c-lineup-multi-inher)
                (inher-intro . +)
                (inlambda . c-lineup-inexpr-block)
                (inline-close . 0)
                (inline-open . 0)
                (inmodule . +)
                (innamespace . +)
                (knr-argdecl . 0)
                (knr-argdecl-intro . 5)
                (label . 0)
                (lambda-intro-cont . +)
                (member-init-cont . c-lineup-multi-inher)
                (member-init-intro . +)
                (module-close . 0)
                (module-open . 0)
                (namespace-close . 0)
                (namespace-open . 0)
                (objc-method-args-cont . c-lineup-ObjC-method-args)
                (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
                (objc-method-intro .
                                   [0])
                (statement-case-intro . +)
                (stream-op . c-lineup-streamop)
                (string . -1000)
                (substatement . +)
                (substatement-label . 0)
                (template-args-cont c-lineup-template-args +)
                (topmost-intro-cont first c-lineup-topmost-intro-cont c-lineup-gnu-DEFUN-intro-cont))))

(defun my-c-mode-common-hook ()
  (font-lock-add-keywords
   nil
   '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end)
  (c-set-style "e-and-m-c"))


(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Setup javascript formatting
(setq js-indent-level 2)
