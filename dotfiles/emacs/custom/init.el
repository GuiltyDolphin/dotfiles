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
(customize-set-variable 'haskell-process-type 'cabal-repl)

;;; Code:

;;;;;;;;
;; EL ;;
;;;;;;;;

;; NO FRILLS

(customize-set-variable 'inhibit-startup-screen t) ; no splash screen on start
(tool-bar-mode -1) ; no tool bar with icons
(scroll-bar-mode -1) ; no scroll bars
(menu-bar-mode -1) ; no menu bar


;; NO JUNK
(customize-set-variable 'auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(customize-set-variable 'backup-directory-alist `((".*" . ,temporary-file-directory)))


;; EL-GET
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))

(unless (require 'el-get nil t) ; t -> 'noerror
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(setq my:el-get-packages
  '(auto-complete
    color-theme-solarized
    duckpan
    el-get
    emaps
    evil
    evil-leader
    evil-remap
    evil-surround
    flycheck
    flx
    git-modes
    haskell-mode
    magit
    projectile
    slime
    use-package
    yasnippet))

(el-get 'sync my:el-get-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cl-lib)

(defun el-dir (&optional path)
  "Return the user's `el-get' directory with PATH optionally appended."
  (concat el-get-dir "/" path))


;;; TIME & Colorscheme

(require 'calendar)

(defvar location-name "London"
  "Name of major location for use in calendar calculations.")

(defvar location-longitude [0 5 west]
  "Longitude of major location.")

(defvar location-latitude [51 32 north]
  "Latitude of major location.")

(defvar location-loc (list location-name location-latitude location-longitude)
  "Preferred location information in the form '(NAME LATITUDE LONGITUDE).")

(require 'solar)

(defun sunrise-sunset-times (loc-name loc-lat loc-lon)
  "Get the sunrise, sunset and hours of daylight in the form (SUNRISE SUNSET HOURS).

LOC-NAME, LOC-LON, and LOC-LAT should be the name, longitude, and latitude of the location for
which sunrise/sunset times should be retrieved. They should be in a form acceptable to
calendar-location-name, calendar-longitude, and calendar-latitude respectively."
  (let* ((calendar-location-name loc-name)
        (calendar-longitude loc-lon)
        (calendar-latitude loc-lat)
        (times (solar-sunrise-sunset (calendar-current-date)))
        (sunrise-time (solar-daylight (caar times)))
        (sunset-time (solar-daylight (caadr times)))
        (wrap-start (format-time-string "%FT"))
        (wrap-end (format-time-string "%Z")))
    (mapcar (lambda (x) (date-to-time (concat wrap-start x wrap-end)))
            (list sunrise-time sunset-time))))

(defun time-compare (time1 time2)
  "Return 'lt if TIME1 is less than TIME2, 'eq if they are equal or 'gt otherwise."
  (if (time-less-p time1 time2) 'lt
    (if (= 0 (time-to-seconds (time-subtract time1 time2))) 'eq 'gt)))

(defun time-greater-p (time1 time2)
  "Return non-nil if TIME1 is later than TIME2."
  (not (or (time-less-p time1 time2) (eq time1 time2))))

(defun location-sunrise-sunset (location)
  "Get the '(SUNRISE SUNSET) times for LOCATION."
  (let ((loc-name (car location))
        (loc-lat (cadr location))
        (loc-lon  (caddr location)))
    (sunrise-sunset-times loc-name loc-lat loc-lon)))

(defun date-in-daylight-hours (date)
  "Return non-nil if DATE is within the daylight hours for the current location.

Default to NIL if daylight times cannot be retrieved."
  (let* ((sunrise-sunset (location-sunrise-sunset location-loc))
         (sunrise-time (car sunrise-sunset))
         (sunset-time (cadr sunrise-sunset))
         (curr-time (or date (current-time))))
    (and sunrise-time sunset-time
         (time-greater-p curr-time sunrise-time) (time-less-p curr-time sunset-time))))

(defun get-background-mode-for-time-of-day (&optional date)
  "Return either 'light or 'dark based on whether DATE (or (current-time)) is during daylight
hours or not."
  (if (date-in-daylight-hours (or date (current-time))) 'light 'dark))

;; Color theme
(let ((bgmode (get-background-mode-for-time-of-day)))
  (set-frame-parameter nil 'background-mode bgmode)
  (set-terminal-parameter nil 'background-mode bgmode))
(load-theme 'solarized t)

;; Font
(set-face-font 'default "Inconsolata-14")

; Folder with own content
(add-to-list 'load-path (locate-user-emacs-file "custom"))


(defvar user-preferred-license "GPL-3"
  "License to use by default with some modes")

(customize-set-variable 'user-mail-address "guiltydolphin@gmail.com")

;; Emaps
(use-package emaps)

;; Evil leader
(use-package evil-leader
  :config
  (global-evil-leader-mode 1))

; Use the space key as leader
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "ir" 'align-regexp
  "sv" 'reload-user-init-file
  "ns" 'scratch-buffer
  "nS" 'new-scratch
  ","  'execute-extended-command)

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
(use-package evil-local-leader ; Merely a modification of `evil-leader'
  :config
  (global-evil-local-leader-mode 1)
  (evil-local-leader/set-local-leader ","))

(setq lisp-modes '(emacs-lisp-mode
                   lisp-interaction-mode
                   lisp-mode slime-mode))

(dolist (mode lisp-modes)
  (evil-local-leader/set-key-for-mode mode
    "er" 'eval-region
    "eb" 'eval-buffer
    "ed" 'eval-defun))

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
(use-package evil
  :config
  (customize-set-variable 'evil-want-C-w-in-emacs-state t)
  ; * and # search for full symbols.
  (customize-set-variable 'evil-symbol-word-search t)
  (evil-mode 1))

(use-package evil-remap
  :config
  (evil-nnoremap! ";" 'evil-ex)
  (evil-nnoremap! ":" 'evil-repeat-find-char)
  (global-set-key (kbd "C-t") 'nil)
  (evil-nnoremap! (kbd "C-t") 'evil-window-map)
  (evil-nnoremap! (kbd "C-t C-h") 'previous-buffer)
  (evil-nnoremap! (kbd "C-t C-l") 'next-buffer)

  (evil-inoremap (kbd "C-c") 'evil-normal-state)
  (evil-vnoremap (kbd "C-c") 'evil-exit-visual-state)
  (define-key evil-window-map (kbd "C-t") 'evil-window-next)
  (define-key evil-window-map "t" 'evil-window-right) ; Replaces evil-window-top-left
  (define-key evil-window-map "-" 'evil-window-split) ; Replaces evil-window-set-width
  (define-key evil-window-map "|" 'evil-window-vsplit) ; Replaces evil-window-decrease-height
  (define-key evil-window-map "x" 'kill-buffer-and-window-ask)
  (define-key evil-window-map "s" 'ido-switch-buffer)
  (global-set-key (kbd "C-w") 'nil)

  (evil-nnoremap! (kbd "C-u") 'evil-scroll-up)
  (evil-nnoremap! (kbd "M-u") 'universal-argument)

  (evil-nnoremap! (kbd "Q") 'quit-window)) ; So we can *always* quit

;; Magit
(use-package magit
  :init
  (defvar evil-leader-magit-map
    (make-sparse-keymap "keymap for magit bindings under leader key"))
  (evil-leader/set-key
    "m" evil-leader-magit-map)
  :config
  (emaps-define-key evil-leader-magit-map
    "d" 'magit-diff-working-tree
    "s" 'magit-status))

;; Todo

;; Flycheck
(add-to-list 'load-path (locate-user-emacs-file "el-get/dash"))
(load (locate-user-emacs-file "el-get/dash/dash.el"))
(use-package dash)

(add-to-list 'load-path (locate-user-emacs-file "el-get/flycheck"))
(use-package flycheck
  :config
  (global-flycheck-mode 1)
  (evil-leader/set-key
    "f" flycheck-command-map))

;; Column and line number in mode line
(line-number-mode 1)
(column-number-mode 1)

(global-linum-mode 1) ; line number in margin
(global-hl-line-mode 1) ; highlight current line


(customize-set-variable 'x-select-enable-clipboard t) ; Use the clipboard

(display-time-mode t) ; Allow displaying of time in mode line

(use-package flx-ido
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  (customize-set-variable 'ido-enable-flex-matching t)
  (customize-set-variable 'ido-use-faces nil))

; Other
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-stop-list '(4 8 12))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun my-move-key (keymap-from keymap-to key)
  "Moves a keybinding from one keymap to another, removing previous binding"
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))

;(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

(define-key global-map (kbd "C-h h") 'help)
(emaps-define-key help-map
  (kbd "C-e") 'evil-scroll-line-down
  (kbd "C-y") 'evil-scroll-line-up)

; Lisp

(setq inferior-lisp-program (executable-find "sbcl"))

; Slime
(add-to-list 'load-path "~/.emacs.d/el-get/slime")
(use-package slime-autoloads)
(use-package slime
  :config
  (slime-setup '(slime-fancy)))

;; Python
(add-to-list 'load-path (locate-user-emacs-file "el-get/python"))
;(autoload 'python-mode "python-mode" "Python Mode." t)
;(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; Haskell-mode
(add-to-list 'load-path (el-dir "ghc-mod/elisp"))
(add-to-list 'load-path (el-dir "haskell-mode"))

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (customize-set-variable 'haskell-process-type 'cabal-repl)
  (add-hook 'haskell-mode-hook 'flymake-mode-off) ; This seems to have fixed the flymake issue.
                                                  ; Flycheck seems to handle errors well, and the
                                                  ; cably-repl doesn't seem to be broken.
                                                  ; Not sure what the issue was before.
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (customize-set-variable 'haskell-interactive-popup-errors nil))

; (load (locate-user-emacs-file "el-get/haskell-mode/haskell-mode.el"))
; (load (locate-user-emacs-file "el-get/haskell-mode/haskell-mode-autoloads.el"))

(defmacro after (mode &rest body)
(declare (indent defun))
`(eval-after-load ,mode
    '(progn ,@body)))

;; YASnippet
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get/yasnippet"))
(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs
               (locate-user-emacs-file "custom/snippets"))

  (add-to-list 'auto-mode-alist '("custom/snippets" . snippet-mode))
  (yas-global-mode 1)

  (define-key yas-minor-mode-map (kbd "C-b") 'yas-expand)

  (add-hook 'yas-before-expand-snippet-hook
            (lambda ()
              (define-key yas-minor-mode-map (kbd "C-b") 'yas-next-field)))

  (add-hook 'yas-after-exit-snippet-hook
            (lambda ()
              (define-key yas-minor-mode-map (kbd "C-b") 'yas-expand)))

  (add-hook 'yas-minor-mode-hook
            (lambda ()
              (yas-activate-extra-mode 'fundamental-mode)))

  (after 'yasnippet
    (yas/reload-all)
    (customize-set-variable 'yas/prompt-functions '(yas/ido-prompt yas/completing-prompt yas/no-prompt)))

  (after "yasnippet-autoloads"
    (add-hook 'prog-mode-hook 'yas-minor-mode)))

;; hippie-expand
(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "C-SPC") 'hippie-expand)

;; projectile
(use-package projectile
  :config
  (projectile-global-mode 1)
  (evil-leader/set-key
    "p" 'projectile-command-map))

;; org
(use-package org
  :init
  (defvar evil-leader-org-map
    (make-sparse-keymap "leader org-mode map"))

  (emaps-define-key evil-leader-org-map
    "a" 'org-agenda
    "c" 'org-capture
    "l" 'org-store-link
    "s" 'org-switchb)
  :config
  (defun org-subdir (path)
    "Return PATH under ORG-DIRECTORY"
    (concat org-directory "/" path))

  (customize-set-variable 'org-agenda-files `(,(org-subdir "todo.org") ,(org-subdir "homework.org")))

  (customize-set-variable 'org-default-notes-file (concat org-directory "/notes.org"))

  (evil-leader/set-key
    "o" evil-leader-org-map)
  (setq org-capture-templates
        `(("t" "Todo" entry (file+headline ,(org-subdir "todo.org") "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree ,(org-subdir "journal.org"))
           "* %?\nEntered on %U\n  %i\n  %a"))))

;; Other commands




(defun scratch-buffer ()
  "Switch to the *scratch* buffer, making a new
one if necessary."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun new-scratch ()
  "Opens a clean *scratch* buffer.

If a *scratch* buffer exists, this will undo any changes
made in that buffer."
  (interactive)
  (scratch-buffer)
  (clear-buffer))

(defun kill-buffer-and-window-ask ()
  "Kill the current buffer and window if user responds in the affirmative.

Ask again if the buffer is modified."
  (interactive)
  (when (y-or-n-p "Kill current buffer and window?: ")
    (when (or
           (not (buffer-modified-p))
           (and (buffer-modified-p) (y-or-n-p "Buffer is modified, are you sure?: ")))
      (kill-buffer-and-window))))

(defun clear-buffer (&optional buffer)
  "Clear all the text in BUFFER without modifying the kill ring"
  (interactive "b")
  (let ((buffer (or buffer (current-buffer))))
       (with-current-buffer buffer
            (kill-region (point-min) (point-max)))))

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

(defun argument-string-p (str)
  "Return t if STR is a valid argument string

Argument strings should follow a pattern similar to
(fn arg1 arg2 &rest args)"
  (when (string-match "(fn[^)]*)" str) t))

;; Use `emacs-lisp-space' when using the space key in emacs-lisp modes
(evil-define-key 'insert ielm-map " " 'emacs-lisp-space)
(evil-define-key 'insert emacs-lisp-mode-map " " 'emacs-lisp-space)

(global-unset-key (kbd "C-s"))

(defvar state-switch-map
  (make-sparse-keymap "evil state switch map")
  "Map for switching evil states")

(emaps-define-key state-switch-map
  "n" 'evil-normal-state
  "m" 'evil-motion-state
  "e" 'evil-emacs-state)

(emaps-define-key global-map (kbd "C-s") state-switch-map)


(defvar jump-map
  (make-sparse-keymap "jump map")
  "Keymap for jumping around.")

(emaps-define-key jump-map
  "t" 'eshell
  "i" 'find-user-init-file)

(evil-leader/set-key "g" jump-map)
;;; init.el ends here
