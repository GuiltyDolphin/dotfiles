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
    helm
    idris-mode
    magit
    projectile
    slime
    use-package
    yasnippet))

(el-get 'sync my:el-get-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cl-lib)

(defun my-el-dir (&optional path)
  "Return the user's `el-get' directory with PATH optionally appended."
  (concat el-get-dir "/" path))


;;; TIME & Colorscheme

(require 'calendar)

(defvar my-location-name "London"
  "Name of major location for use in calendar calculations.")

(defvar my-location-longitude [0 5 west]
  "Longitude of major location.")

(defvar my-location-latitude [51 32 north]
  "Latitude of major location.")

(defvar my-location-loc (list my-location-name my-location-latitude my-location-longitude)
  "Preferred location information in the form '(NAME LATITUDE LONGITUDE).")

(require 'solar)

(defun my-sunrise-sunset-times (loc-name loc-lat loc-lon)
  "Get the sunrise, sunset and hours of daylight in the form (SUNRISE SUNSET HOURS).

LOC-NAME, LOC-LON, and LOC-LAT should be the name, longitude, and latitude of the location for
which sunrise/sunset times should be retrieved. They should be in a form acceptable to
calendar-location-name, calendar-longitude, and calendar-latitude respectively."
  (let* ((calendar-location-name loc-name)
        (calendar-longitude loc-lon)
        (calendar-latitude loc-lat)
        (times (solar-sunrise-sunset (calendar-current-date)))
        (sunrise-time (solar-daylight (caar times)))
        (sunset-time (solar-daylight (cl-caadr times)))
        (wrap-start (format-time-string "%FT"))
        (wrap-end (format-time-string "%Z")))
    (mapcar (lambda (x) (date-to-time (concat wrap-start x wrap-end)))
            (list sunrise-time sunset-time))))

(defun my-time-greater-p (time1 time2)
  "Return non-nil if TIME1 is later than TIME2."
  (not (or (time-less-p time1 time2) (eq time1 time2))))

(defun my-location-sunrise-sunset (location)
  "Get the '(SUNRISE SUNSET) times for LOCATION."
  (let ((loc-name (car location))
        (loc-lat (cadr location))
        (loc-lon  (cl-caddr location)))
    (my-sunrise-sunset-times loc-name loc-lat loc-lon)))

(defun my-date-in-daylight-hours (date)
  "Return non-nil if DATE is within the daylight hours for the current location.

Default to NIL if daylight times cannot be retrieved."
  (let* ((sunrise-sunset (my-location-sunrise-sunset my-location-loc))
         (sunrise-time (car sunrise-sunset))
         (sunset-time (cadr sunrise-sunset))
         (curr-time (or date (current-time))))
    (and sunrise-time sunset-time
         (my-time-greater-p curr-time sunrise-time) (time-less-p curr-time sunset-time))))

(defvar my-background-timers nil
  "Timers for changing the background mode.")

(defun my-background-clear-timers ()
  "Clear the current background timers."
  (dolist (timer my-background-timers)
    (cancel-timer timer))
  (setq my-background-timers nil))

(defun my-background-set (bgmode)
  "Set the current background mode to BGMODE.
BGMODE should be one of 'light or 'dark."
  (set-frame-parameter nil 'background-mode bgmode)
  (set-terminal-parameter nil 'background-mode bgmode)
  (my-background-clear-timers)
  ; give it time to sort itself out
  (run-at-time "2 minutes" nil 'my-background-initialize-timers)
  (load-theme 'solarized t))

(defun my-background-set-dark ()
  "Set the current background mode to 'dark."
  (my-background-set 'dark))

(defun my-background-set-light ()
  "Set the current background mode to 'light."
  (my-background-set 'light))

(defun my-background-initialize-timers ()
  (let* ((sunrise-sunset (my-location-sunrise-sunset my-location-loc))
         (sunrise-time (car sunrise-sunset))
         (sunset-time (cadr sunrise-sunset))
         (time-format "%F %T %Z")
         (add-bg-timer (lambda (mode time)
                         (push (run-at-time
                                (format-time-string time-format time) nil
                                (intern (format "my-background-set-%s" mode)))
                               my-background-timers))))
  (if (my-date-in-daylight-hours (current-time))
      (funcall add-bg-timer 'dark sunset-time)
    (let ((sunrise-today-or-next (if (time-less-p (current-time) sunrise-time)
                                     sunrise-time
                                   (time-add (days-to-time 1) sunrise-time)))) ; close enough
      (funcall add-bg-timer 'light sunrise-today-or-next)))))


;; Color theme
(if (my-date-in-daylight-hours (current-time))
    (my-background-set-light)
  (my-background-set-dark))

;; Font
(set-face-font 'default "Inconsolata-14")

; Folder with own content
(add-to-list 'load-path (locate-user-emacs-file "custom"))


(defvar my-user-preferred-license "GPL-3"
  "License to use by default with some modes")

(customize-set-variable 'user-mail-address "guiltydolphin@gmail.com")

;;; dash
(add-to-list 'load-path (locate-user-emacs-file "el-get/dash"))
(load (locate-user-emacs-file "el-get/dash/dash.el"))
(use-package dash)

;; Emaps
(use-package emaps
  :config
  (define-key global-map (kbd "C-h K") 'emaps-describe-keymap-bindings))

;; Evil leader
(use-package evil-leader
  :config
  (global-evil-leader-mode 1))

; Use the space key as leader
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "ir" 'align-regexp
  "sv" 'my-reload-user-init-file
  "ns" 'my-scratch-buffer
  "nS" 'my-new-scratch
  ","  'helm-M-x)

(defun my-find-user-init-file ()
  "Find the user's init.el file"
  (interactive)
  (find-file user-init-file))

(defun my-reload-user-init-file ()
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

(defun my-evil-set-initial-state-modes (state &rest modes)
  "Set STATE as the initial state for each of MODES.

See `evil-set-initial-state'."
  (--map (evil-set-initial-state it state) modes))
(put 'my-evil-set-initial-state-modes 'lisp-indent-function 'defun)

(defun my-kill-buffer-and-window-ask ()
  "Kill the current buffer and window if user responds in the affirmative.

Ask again if the buffer is modified."
  (interactive)
  (when (y-or-n-p "Kill current buffer and window?: ")
    (when (or
           (not (buffer-modified-p))
           (and (buffer-modified-p) (y-or-n-p "Buffer is modified, are you sure?: ")))
      (kill-buffer-and-window))))

(defun my-clear-buffer (&optional buffer)
  "Clear all the text in BUFFER without modifying the kill ring"
  (interactive "b")
  (let ((buffer (or buffer (current-buffer))))
       (with-current-buffer buffer
            (kill-region (point-min) (point-max)))))

(use-package evil-remap
  :config
  (evil-nnoremap! ";" 'evil-ex)
  (evil-nnoremap! ":" 'evil-repeat-find-char)
  (global-set-key (kbd "C-t") 'nil)
  (dolist (state '(insert motion normal visual))
    (emaps-define-key (symbol-value (intern (concat "evil-" (symbol-name state) "-state-map")))
      (kbd "C-t") evil-window-map))
  (emaps-define-key evil-window-map
    (kbd "C-h") 'previous-buffer
    (kbd "C-l") 'next-buffer
    (kbd "C-t") 'evil-window-next
    "t" 'evil-window-right ; Replaces evil-window-top-left
    "-" 'evil-window-split ; Replaces evil-window-set-width
    "|" 'evil-window-vsplit ; Replaces evil-window-decrease-height
    "x" 'my-kill-buffer-and-window-ask
    "s" 'helm-buffers-list)

  (evil-inoremap (kbd "C-c") 'evil-normal-state)
  (evil-vnoremap (kbd "C-c") 'evil-exit-visual-state)
  (global-set-key (kbd "C-w") 'nil)

  (evil-nnoremap! (kbd "C-u") 'evil-scroll-up)
  (evil-nnoremap! (kbd "M-u") 'universal-argument)

  (evil-nnoremap! (kbd "Q") 'quit-window)) ; So we can *always* quit

;; Magit
(use-package magit
  :init
  (defvar my-evil-leader-magit-map
    (make-sparse-keymap "keymap for magit bindings under leader key"))
  (evil-leader/set-key
    "m" my-evil-leader-magit-map)
  :config
  (emaps-define-key my-evil-leader-magit-map
    "b" 'magit-branch-manager
    "d" 'magit-diff-working-tree
    "s" 'magit-status)
  (evil-define-key '(motion normal) magit-mode-map
    (kbd "TAB") 'magit-toggle-section
    (kbd "RET") 'magit-visit-item
    (kbd "z o") 'magit-show-section
    (kbd "z c") 'magit-hide-section
    "{" 'magit-goto-previous-section
    "}" 'magit-goto-next-section)
  (evil-define-key 'visual magit-mode-map
    "s" 'magit-stage-item
    "u" 'magit-unstage-item)
  (my-evil-set-initial-state-modes 'motion
    'magit-branch-manager-mode
    'magit-status-mode
    'magit-commit-mode
    'magit-diff-mode
    'magit-log-mode))

(add-hook 'git-commit-mode-hook (lambda () (flyspell-mode t)))

(evil-set-initial-state 'git-commit-mode 'insert)

;; Todo

;;; Flycheck
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
(add-to-list 'load-path (my-el-dir "ghc-mod/elisp"))
(add-to-list 'load-path (my-el-dir "haskell-mode"))

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

;;; Idris
(use-package idris-mode
  :config
  (evil-local-leader/set-key-for-mode 'idris-mode
    "a" 'idris-add-clause
    "c" 'idris-case-dwim
    "l" 'idris-make-lemma
    "p" 'idris-proof-search
    "t" 'idris-type-at-point))

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

;;; helm
(use-package helm
  :init
  (defvar my-helm-leader-map
    (make-sparse-keymap "helm leader map"))
  (evil-leader/set-key "h" my-helm-leader-map)
  :config
  (emaps-define-key my-helm-leader-map
    "i" 'helm-imenu)
  (evil-leader/set-key "b" 'helm-imenu)
  (evil-nnoremap! (kbd "C-p") 'helm-find-files)
  (global-set-key (kbd "C-x C-f") 'helm-find-files))

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
  (defvar my-evil-leader-org-map
    (make-sparse-keymap "leader org-mode map"))

  (emaps-define-key my-evil-leader-org-map
    "a" 'org-agenda
    "c" 'org-capture
    "l" 'org-store-link
    "s" 'org-switchb)
  :config
  (defun my-org-subdir (path)
    "Return PATH under ORG-DIRECTORY"
    (concat org-directory "/" path))

  (customize-set-variable 'org-agenda-files `(,(my-org-subdir "todo.org") ,(my-org-subdir "homework.org")))

  (customize-set-variable 'org-default-notes-file (concat org-directory "/notes.org"))

  (evil-leader/set-key
    "o" my-evil-leader-org-map)
  (setq org-capture-templates
        `(("t" "Todo" entry (file+headline ,(my-org-subdir "todo.org") "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree ,(my-org-subdir "journal.org"))
           "* %?\nEntered on %U\n  %i\n  %a"))))

;; Other commands




(defun my-scratch-buffer ()
  "Switch to the *scratch* buffer, making a new
one if necessary."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun my-new-scratch ()
  "Opens a clean *scratch* buffer.

If a *scratch* buffer exists, this will undo any changes
made in that buffer."
  (interactive)
  (my-scratch-buffer)
  (my-clear-buffer))

(defun my-emacs-lisp-space (n)
  ; Similar to 'slime-space', but support all emacs-lisp and custom functions - not just
  ; those supported by slime packages
  (interactive "p")
  (self-insert-command n)
  (my-emacs-lisp-echo-arglist))

(defun my-emacs-lisp-echo-arglist ()
  "Echo to the minibuffer the argument syntax of the symbol under `point', if any"
  (let ((opr (slime-operator-before-point)))
    (when opr
      (let* ((op (read opr))
             (arglist (my-get-function-arglist op)))
        (when arglist
          (slime-message "%s" arglist))))))

(defun my-get-function-arglist (fn)
  "Retrieve the arglist of the function-like object fn.
   returns nil if no there is no function with the specified symbol"
  (when (symbol-function fn)
    (let* ((full-doc (my-split-lines (documentation fn)))
           (argstring (car (last full-doc))))
      (if (my-argument-string-p argstring)
        (format "%s" (replace-regexp-in-string "^(fn" (format "(%s" fn) argstring))
        (let ((argstring (help-function-arglist fn t)))
          (when argstring
            (format "%s" (append (list fn) argstring))))))))

(defun my-split-lines (str &optional omit-nulls trim)
  (split-string str "\n" omit-nulls trim))

(defun my-argument-string-p (str)
  "Return t if STR is a valid argument string

Argument strings should follow a pattern similar to
(fn arg1 arg2 &rest args)"
  (when (string-match "(fn[^)]*)" str) t))

;; Use `my-emacs-lisp-space' when using the space key in emacs-lisp modes
(evil-define-key 'insert ielm-map " " 'my-emacs-lisp-space)
(evil-define-key 'insert emacs-lisp-mode-map " " 'my-emacs-lisp-space)

(global-unset-key (kbd "C-s"))

(defvar my-state-switch-map
  (make-sparse-keymap "evil state switch map")
  "Map for switching evil states")

(emaps-define-key my-state-switch-map
  "n" 'evil-normal-state
  "m" 'evil-motion-state
  "e" 'evil-emacs-state)

(emaps-define-key global-map (kbd "C-s") my-state-switch-map)

;;; Spelling
(add-hook 'text-mode-hook (lambda () (flyspell-mode t)))

(defvar my-jump-map
  (make-sparse-keymap "jump map")
  "Keymap for jumping around.")

(emaps-define-key my-jump-map
  "t" 'eshell
  "i" 'my-find-user-init-file)

(evil-leader/set-key "g" my-jump-map)

(defun my-evil-local-leader/subsume-keys-for-major-mode (major-mode)
  "Bind keys in MAJOR-MODE under `evil-local-leader' without overwriting bindings."
  (let ((major-mode-map-symbol (intern (concat (symbol-name major-mode) "-map"))))
    (when (boundp major-mode-map-symbol)
      (let ((major-mode-map (symbol-value major-mode-map-symbol))
            (local-major-bindings (evil-local-leader/bindings-for-mode major-mode)))
        (map-keymap
         (lambda (key def)
           (-if-let (key (and (characterp key) (char-to-string key)))
               (unless (and local-major-bindings (lookup-key local-major-bindings key))
                 (evil-local-leader/set-key-for-mode major-mode key def))))
         major-mode-map)))))

(add-hook 'after-change-major-mode-hook (lambda () (my-evil-local-leader/subsume-keys-for-major-mode major-mode)))
;;; init.el ends here
