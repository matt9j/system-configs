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

;;EMACS setup
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;GUI Setup
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 1)
(line-number-mode 1)
(column-number-mode 1)

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

;; Script that flymake uses to check code. This script must be
;; present in the system path.
(setq pycodechecker "pylint_etc_wrapper.py")

(when (load "flymake" t)
  (defun flymake-pycodecheck-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list pycodechecker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pycodecheck-init)))

(add-hook 'python-mode-hook 'flymake-mode)

;;Start emacs server so new windows open in existing frame
(server-start)
;;disable server close buffer confirm:
;;(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

(require 'ido)
(ido-mode t)

;;Enable Syntax Highlighting for Jam files
(require 'jam-mode)
(add-to-list 'auto-mode-alist '("\\.jam\\'" . jam-mode))
(add-to-list 'auto-mode-alist '("\\Jamroot\\'" . jam-mode))

;;Revbufs setup and keybinding
(require 'revbufs)
(global-set-key [f12] (quote revbufs))

(defun create-tags (dir-name)
    "Create tags file."
    (interactive "DDirectory: ")
    (shell-command
     (format "ctags -f %s -e -R %s" path-to-ctags (directory-file-name dir-name)))
  )

;;Outline Magic plugin
(eval-after-load 'outline
  '(progn
    (require 'outline-magic)
    (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle)))

;;package installer
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

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

;;Outline key re-bindings to use arrows
(add-hook 'outline-mode-hook 'my-outline-easy-bindings)
(add-hook 'outline-minor-mode-hook 'my-outline-easy-bindings)
(defun my-outline-easy-bindings ()
(require 'outline-mode-easy-bindings nil t))

;;Smooth Scrolling
(require 'smooth-scrolling)
(global-set-key (kbd "<C-mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<C-mouse-5>") 'scroll-up-line)

;; CEDIT Setup
;;(load-file "/home/mwjohnson/customization/emacs/cedet-1.1/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
;;(global-semantic-highlight-func-mode 1)
;;(global-semantic-show-unmatched-syntax-mode 1)
;;(global-srecode-minor-mode 1)            ; Enable template insertion menu
;;(global-semantic-idle-scheduler-mode 1)
;;(global-semantic-idle-local-symbol-highlight-mode 1)
;;(global-cedet-m3-minor-mode 1)           ; Enable right click context menu
;;(global-semantic-idle-summary-mode 1)

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
