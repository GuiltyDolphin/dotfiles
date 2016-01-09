;; Emacs Configuration File
;; Copyright (C) 2016 Ben Moon
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(package-initialize)
;;; Custom Variables
(custom-set-variables
 '(haskell-process-type 'cabal-repl))

;;; Code:

(require 'cl)  ; Use all the common-lisp goodies
;;;;;;;;
;; EL ;;
;;;;;;;;

;; NO FRILLS

(setq inhibit-startup-screen t) ; no splash screen on start
(tool-bar-mode -1) ; no tool bar with icons
(scroll-bar-mode -1) ; no scroll bars
(menu-bar-mode -1) ; no menu bar


;; NO JUNK
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory)))


;; EL-GET
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))

(unless (require 'el-get nil t) ; t -> 'noerror
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

(setq my:el-get-packages
      '(auto-complete
        color-theme-solarized
        el-get
	evil
	evil-leader
	flycheck
        magit
	slime
	yasnippet))

(el-get 'sync my:el-get-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun el-dir (&optional path)
  "Return the user's `el-get' directory with PATH optionally appended."
  (concat el-get-dir "/" path))

;; Color theme
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)

;; Font
(set-face-font 'default "Inconsolata-14")

; Folder with own content
(add-to-list 'load-path (locate-user-emacs-file "custom"))


(defvar user-preferred-license "GPL-3"
  "License to use by default with some modes")

(setq user-mail-address "guiltydolphin@gmail.com")

;; Evil leader
(require 'evil-leader)
(global-evil-leader-mode 1)

; Use the space key as leader
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "ir" 'align-regexp
  "ev" 'find-user-init-file)

(defun find-user-init-file ()
  "Find the user's init.el file"
  (interactive)
  (find-file user-init-file))

(defun reload-user-init-file ()
  "Evaluate the user's init.el file"
  (interactive)
  (load-file user-init-file))

(evil-leader/set-key
  "ex" 'eval-expression)


(add-to-list 'load-path (locate-user-emacs-file "custom/evil"))
(require 'evil-local-leader) ; Merely a modification of `evil-leader'
(global-evil-local-leader-mode 1)

; Use ',' as the 'local leader' key
(evil-local-leader/set-local-leader ",")

(setq lisp-modes '(emacs-lisp-mode
                   lisp-interaction-mode
                   lisp-mode slime-mode))

(dolist (mode lisp-modes)
  (evil-local-leader/set-key-for-mode mode
    "er" 'eval-region
    "eb" 'eval-buffer))

(evil-local-leader/set-key-for-mode 'haskell-mode
  "en" 'ghc-goto-next-error
  "eN" 'ghc-goto-prev-error
  "t"  'ghc-show-type
  "i"  'ghc-show-info
  "sd" 'inferior-haskell-send-decl)

(evil-local-leader/set-key-for-mode 'latex-mode
  "ib" 'latex-insert-block
  "ir" 'tex-region
  "cb" 'latex-close-block)

(evil-local-leader/set-key-for-mode 'org-mode
  "ih" 'org-insert-heading
  "y" 'org-mode-yank)

;; Eveeel....
(require 'evil)
(evil-mode 1)

;; Find out what this was supposed to do

;(defmacro def-evil-remapper (name states)
;  `(defun ,name (key command)
;     ,(loop for state in states collect
;	    `(define-key
;	       ,(read
;		(concatenate 'string "evil-" `(symbol-name ,state) "-state-map")) key command))))
;
;(def-evil-remapper evil-noremap (normal visual operator))
;(def-evil-remapper evil-other-map (normal))

(require 'evil-remap)

(evil-nnoremap! ";" 'evil-ex)
(evil-nnoremap! ":" 'evil-repeat-find-char)
(evil-nnoremap! (kbd "C-t") 'evil-window-map)
(evil-inoremap (kbd "C-c") 'evil-normal-state)
(evil-vnoremap (kbd "C-c") 'evil-exit-visual-state)
(define-key evil-window-map (kbd "C-t") 'evil-window-next)
(define-key evil-window-map "t" 'evil-window-right) ; Replaces evil-window-top-left
(define-key evil-window-map "-" 'evil-window-split) ; Replaces evil-window-set-width
(define-key evil-window-map "|" 'evil-window-vsplit) ; Replaces evil-window-decrease-height
(global-set-key (kbd "C-w") 'nil)

(evil-nnoremap! (kbd "C-u") 'evil-scroll-up)
(evil-nnoremap! (kbd "M-u") 'universal-argument)

;; Magit
(require 'magit)

(evil-leader/set-key
  "gd" 'magit-diff
  "gs" 'magit-status
  "gca" 'magit-commit-ammend
  "gcs" 'magit-commit-squash)


;; Flycheck
(add-to-list 'load-path (locate-user-emacs-file "el-get/dash"))
(load (locate-user-emacs-file "el-get/dash/dash.el"))
(require 'dash)

(add-to-list 'load-path (locate-user-emacs-file "el-get/flycheck"))
(require 'flycheck)
;(add-hook 'after-init-hook #'(global-flycheck-mode 1))
(global-flycheck-mode 1)

(evil-leader/set-key
  "fn" 'flycheck-next-error
  "fp" 'flycheck-previous-error)

;; Column and line number in mode line
(line-number-mode 1)
(column-number-mode 1)

(global-linum-mode 1) ; line number in margin
(global-hl-line-mode 1) ; highlight current line


(setq x-select-enable-clipboard t) ; Use the clipboard

(display-time-mode t) ; Allow displaying of time in mode line

(require 'ido) ; Use ido for minibuffer completion
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)


; Other
(setq-default indent-tabs-mode nil)
(defun my-move-key (keymap-from keymap-to key)
  "Moves a keybinding from one keymap to another, removing previous binding"
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

;(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

; Lisp

(setq inferior-lisp-program (executable-find "sbcl"))

; Slime
(add-to-list 'load-path "~/.emacs.d/el-get/slime")
(require 'slime)
(require 'slime-autoloads)

(slime-setup '(slime-fancy))


;; Python
(add-to-list 'load-path (locate-user-emacs-file "el-get/python"))
;(autoload 'python-mode "python-mode" "Python Mode." t)
;(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; Haskell-mode
(add-to-list 'load-path (el-dir "ghc-mod/elisp"))
(add-to-list 'load-path (el-dir "haskell-mode"))
(require 'haskell-mode)
(load (locate-user-emacs-file "el-get/haskell-mode/haskell-mode.el"))
(load (locate-user-emacs-file "el-get/haskell-mode/haskell-mode-autoloads.el"))
;(setq haskell-program-name (executable-find "cabal-dev"))
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;(setq haskell-program-name "cabal")
;(setq haskell-ghci-program-name "cabal repl")
;(setq haskell-ghci-program-args "repl")
(setq haskell-process-type 'cabal-repl)
(add-hook 'haskell-mode-hook 'flymake-mode-off) ; This seems to have fixed the flymake issue.
                                                ; Flycheck seems to handle errors well, and the
                                                ; cably-repl doesn't seem to be broken.
                                                ; Not sure what the issue was before.
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(setq haskell-interactive-popup-errors nil)

;; Alignment
;'(add-to-list 'align-rules-list
;              '(haskell-types
;                (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
;                (modes quote (haskell-mode literate-haskell-mode))))
;(add-to-list 'align-rules-list
;             '(haskell-assignment
;               (regexp . "\\(\\s-+\\)=\\s-+")
;               (modes quote (haskell-mode literate-haskell-mode))))
;(add-to-list 'align-rules-list
;             '(haskell-arrows
;               (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
;               (modes quote (haskell-mode literate-haskell-mode))))
;(add-to-list 'align-rules-list
;             '(haskell-left-arrows
;               (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
;               (modes quote (haskell-mode literate-haskell-mode))))


;; YASnippet
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get/yasnippet"))
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs
             (locate-user-emacs-file "custom/snippets"))

(add-to-list 'auto-mode-alist '("custom/snippets" . snippet-mode))
(yas-global-mode 1)

(add-hook 'yas-minor-mode-hook
          (lambda ()
            (yas-activate-extra-mode 'fundamental-mode)))

(defmacro after (mode &rest body)
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(after 'yasnippet
       (yas/reload-all)
       (setq yas/prompt-functions '(yas/ido-prompt yas/completing-prompt yas/no-prompt)))

(after "yasnippet-autoloads"
       (add-hook 'prog-mode-hook 'yas-minor-mode))

;; hippie-expand
(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "C-SPC") 'hippie-expand)


;; Other commands

(defun date (&optional insert-date format-string)
  ; Retrieve the current system date (time)
  (interactive "P")
  (let ((current-date (shell-command-to-string
                       (if format-string
                           (format "date +%s" format-string) "date"))))
    (if insert-date
        (insert current-date)
      (message "%s" (string-remove-suffix "\n" current-date)))))

(defun emacs-lisp-space (n)
  ; Similar to 'slime-space', but support all emacs-lisp and custom functions - not just
  ; those supported by slime packages
  (interactive "p")
  (self-insert-command n)
  (emacs-lisp-echo-arglist))

(defun emacs-lisp-echo-arglist ()
  "Echo to the minibuffer the argument syntax of the symbol under `point', if any"
  (let ((opr (slime-operator-before-point)))
    (when opr
      (let* ((op (read opr))
             (arglist (get-function-arglist op)))
        (when arglist
          (slime-message "%s" arglist))))))

(defun get-function-arglist (fn)
  "Retrieve the arglist of the function-like object fn.
   returns nil if no there is no function with the specified symbol"
  (when (symbol-function fn)
    (let* ((full-doc (split-lines (documentation fn)))
           (argstring (car (last full-doc))))
      (if (argument-string-p argstring)
        (format "%s" (replace-regexp-in-string "^(fn" (format "(%s" fn) argstring))
        (let ((argstring (help-function-arglist fn t)))
          (when argstring
            (format "%s" (append (list fn) argstring))))))))

(defun split-lines (str &optional omit-nulls trim)
  (split-string str "\n" omit-nulls trim))

(defun first-pred (pred ls)
  "Return the first of LS for which PRED returns non-nil.

Return nil if PRED never returns non-nil."
  (dolist (elt ls nil)
    (when (funcall pred elt)
      (return elt))))

(defun argument-string-p (str)
  "Return t if STR is a valid argument string

Argument strings should follow a pattern similar to
(fn arg1 arg2 &rest args)"
  (when (string-match "(fn[^)]*)" str) t))

;; Use `emacs-lisp-space' when using the space key in emacs-lisp modes
(evil-define-key 'insert ielm-map " " 'emacs-lisp-space)
(evil-define-key 'insert emacs-lisp-mode-map " " 'emacs-lisp-space)


;;; init.el ends here
