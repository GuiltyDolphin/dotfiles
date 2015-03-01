(package-initialize)
;;; Custom Variables
(custom-set-variables
 '(haskell-process-type 'cabal-repl))

;;; Code:

(require 'cl)
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

;(unless (require 'el-get nil t) ; t -> 'noerror
;  (url-retrieve
;    "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
;  (lambda (s)
;    (end-of-buffer)
;  (eval-print-last-sexp))))

(unless (require 'el-get nil t) ; t -> 'noerror
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

(setq my:el-get-packages
      '(el-get
	evil
	evil-leader
	auto-complete
        magit
	yasnippet
	slime
	flycheck
	color-theme-solarized))

(el-get 'sync my:el-get-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun el-dir (&optional path)
  (concatenate 'string el-get-dir "/" path))

;; Color theme
(load-theme 'solarized-dark t)

;; Font
(set-face-font 'default "Inconsolata-14")

(add-to-list 'load-path (locate-user-emacs-file "custom"))


;; Evil leader
(require 'evil-leader)
(global-evil-leader-mode 1)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key 
  "ir" 'align-regexp)


(add-to-list 'load-path (locate-user-emacs-file "custom/evil"))
(require 'evil-local-leader)
(global-evil-local-leader-mode 1)

(evil-leader/set-key
  "ex" 'eval-expression)

(setq lisp-modes '(emacs-lisp-mode lisp-interaction-mode lisp-mode slime-mode))

(evil-local-leader/set-local-leader ",")
(dolist (mode lisp-modes) (evil-local-leader/set-key-for-mode mode
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
  

;; Eveeel....
(require 'evil)
(evil-mode 1)

;(defmacro def-evil-remapper (name states)
;  `(defun ,name (key command)
;     ,(loop for state in states collect
;	    `(define-key 
;	       ,(read 
;		(concatenate 'string "evil-" `(symbol-name ,state) "-state-map")) key command))))
;
;(def-evil-remapper evil-noremap (normal visual operator))
;(def-evil-remapper evil-other-map (normal))
;(evil-other-map "ttt" '(message "yay"))
(defun evil-noremap (key command)
  (progn
  (evil-nnoremap key command)
  (evil-vnoremap key command)
  (evil-onoremap key command)))
(defun evil-nnoremap (key command)
  (define-key evil-normal-state-map key command))
(defun evil-vnoremap (key command)
  (define-key evil-visual-state-map key command))
(defun evil-xnoremap (key command)
  (define-key evil-ex-map key command))
;(defun evil-snoremap key command
;  (define-key evil-sel
(defun evil-onoremap (key command)
  (define-key evil-operator-state-map key command))
;(defun evil-noremap! key command
;  (define-key evil-insert-state-map key command)
(defun evil-inoremap (key command)
  (define-key evil-insert-state-map key command))
(defun evil-enoremap (key command)
  (define-key evil-emacs-state-map key command))
(defun evil-mnoremap (key command)
  (define-key evil-motion-state-map key command))
(defun evil-nnoremap! (key command)
  (evil-nnoremap key command)
  (evil-mnoremap key command))



(evil-nnoremap! ";" 'evil-ex)
(evil-nnoremap! ":" 'evil-repeat-find-char)
(evil-nnoremap! (kbd "C-t") 'evil-window-map)
(define-key evil-window-map (kbd "C-t") 'evil-window-next)
(global-set-key (kbd "C-w") 'nil)


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
;(add-hook 'haskell-mode-hook '(flymake-mode nil))
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
;(setq yas-snippet-dirs
;      '((locate-user-emacs-file "custom/snippets")
;        (el-dir "yasnippet/snippets")))
                               
(yas-global-mode 1)

;; hippie-expand
(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "C-SPC") 'hippie-expand)


;; Other commands

(defun date (&optional insert-date format-string)
  (interactive "P")
  (let ((current-date (shell-command-to-string (if format-string (format "date +%s" format-string) "date"))))
    (if insert-date 
        (insert current-date) 
      (message "%s" current-date))))


;;; init.el ends here

