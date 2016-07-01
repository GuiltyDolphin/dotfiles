;; THIS IS A MODIFIED SCRIPT
;;
;; Based on 'evil-leader.el' "git://github.com/cofi/evil-leader.git"
;; Copyright (C) 2011-2013 Michael Markert <markert.michael@googlemail.com>
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


;;; Code:

(require 'evil)

(defvar evil-local-leader--default-map (make-sparse-keymap)
  "Keymap used for mode-independent leader bindings.")

(defvar evil-local-leader--mode-maps nil
  "Alist of mode-local leader bindings, shadows mode-independent bindings.")

;;; customization
(defgroup evil-local-leader nil
  "<localleader> support for evil."
  :group 'evil
  :prefix 'evil-local-leader/)

(defcustom evil-local-leader/local-leader "\\"
  "The <leader> key, used to access keys defined by `evil-local-leader/set-key' in normal and visual state.
Must be readable by `read-kbd-macro'. For example: \",\"."
  :type 'string
  :group 'evil-local-leader)

(defcustom evil-local-leader/non-normal-prefix "C-"
  "Prefix for leader-map in insert- and emacs-state.
`evil-local-leader/in-all-states' has to be non-nil for this to be set.
The combination has to be readable by `read-kbd-macro'."
  :type 'string
  :group 'evil-local-leader)

(defcustom evil-local-leader/no-prefix-mode-rx nil
  "List of regular expressions for mode names where `evil-local-leader/local-leader' is used regardless of the state.

If the current major mode is matched by one of the regular expressions
`evil-local-leader/local-leader' is installed in emacs/insert state without
the prefix additionally to the prefixed key.

`evil-local-leader/in-all-states' has to be non-nil for this setting to have any effect."
  :type 'list
  :group 'evil-local-leader)

(defcustom evil-local-leader/in-all-states nil
  "If is non-nil leader-map is accessible by <prefixed-local-leader> in emacs/insert state.

<prefixed-local-leader> is `evil-local-leader/non-normal-prefix' + `evil-local-leader/local-leader'"
  :type 'boolean
  :group 'evil-local-leader)

;;;###autoload
(define-minor-mode global-evil-local-leader-mode
  "Global minor mode for <leader> support."
  nil nil nil
  (if global-evil-local-leader-mode
      (add-hook 'evil-local-mode-hook #'evil-local-leader-mode t)
    (remove-hook 'evil-local-mode-hook #'evil-local-leader-mode t)))

;;;###autoload
(define-minor-mode evil-local-leader-mode
  "Minor mode to enable <localleader> support."
  :init-value nil
  :keymap nil
  (let* ((prefixed (read-kbd-macro (concat evil-local-leader/non-normal-prefix evil-local-leader/local-leader)))
         (no-prefix (read-kbd-macro evil-local-leader/local-leader))
         (mode-map (cdr (assoc major-mode evil-local-leader--mode-maps)))
         (map (or mode-map evil-local-leader--default-map))
         (no-prefix-rx (if evil-local-leader/no-prefix-mode-rx
                           (mapconcat #'identity evil-local-leader/no-prefix-mode-rx "\\|")
                         nil)))
    (if evil-local-leader-mode
        (progn
          (evil-normalize-keymaps)
          (define-key evil-motion-state-local-map no-prefix map)
          (define-key evil-normal-state-local-map no-prefix map)
          (when evil-local-leader/in-all-states
            (define-key evil-emacs-state-local-map prefixed map)
            (define-key evil-insert-state-local-map prefixed map))
          (when (and no-prefix-rx (string-match-p no-prefix-rx (symbol-name major-mode)))
            (define-key evil-emacs-state-local-map no-prefix map)
            (define-key evil-insert-state-local-map no-prefix map)))
      (define-key evil-motion-state-local-map no-prefix nil)
      (define-key evil-normal-state-local-map no-prefix nil)
      (when evil-local-leader/in-all-states
        (define-key evil-emacs-state-local-map prefixed nil)
        (define-key evil-insert-state-local-map prefixed nil)
        (when (and no-prefix-rx (string-match-p no-prefix-rx (symbol-name major-mode)))
          (define-key evil-emacs-state-local-map no-prefix nil)
          (define-key evil-insert-state-local-map no-prefix nil))))))

(defun evil-local-leader/set-local-leader (key &optional prefix)
  "Set local-leader key to `key' and non-normal-prefix to `prefix' and remove old bindings.

Passing `nil' as `prefix' leaves prefix unchanged."
  (let ((global-on global-evil-local-leader-mode)
        (local-on evil-local-leader-mode))
    (when local-on
      (evil-local-leader-mode -1))
    (when global-on
      (global-evil-local-leader-mode -1))
    (setq evil-local-leader/local-leader key)
    (when prefix
      (setq evil-local-leader/non-normal-prefix prefix))
    (if global-on
        (global-evil-local-leader-mode 1)
      (when local-on
        (evil-local-leader-mode 1)))))

;;;###autoload
(defun evil-local-leader/set-key (key def &rest bindings)
  "Bind `key' to command `def' in `evil-local-leader/default-map'.

Key has to be readable by `read-kbd-macro' and `def' a command.
Accepts further `key' `def' pairs."
  (interactive "kKey: \naCommand: ")
  (evil-local-leader--def-keys evil-local-leader--default-map key def bindings))
(put 'evil-local-leader/set-key 'lisp-indent-function 'defun)

;;;###autoload
(defun evil-local-leader/set-key-for-mode (mode key def &rest bindings)
  "Create keybindings for major-mode `mode' with `key' bound to command `def'.

See `evil-local-leader/set-key'."
  (interactive "SMode: \nkKey: \naCommand: ")
  (let ((mode-map (cdr (assoc mode evil-local-leader--mode-maps))))
    (unless mode-map
      (setq mode-map (make-sparse-keymap))
      (set-keymap-parent mode-map evil-local-leader--default-map)
      (push (cons mode mode-map) evil-local-leader--mode-maps))
    (evil-local-leader--def-keys mode-map key def bindings)))
(put 'evil-local-leader/set-key-for-mode 'lisp-indent-function 'defun)

(defun evil-local-leader--def-keys (map key def bindings)
  (while key
    (define-key map (read-kbd-macro key) def)
    (setq key (pop bindings)
          def (pop bindings))))

;;;###autoload
(defun evil-local-leader/bindings-for-mode (mode)
  "Return a keymap of all bindings for MODE under `evil-local-leader/local-leader'.

Return NIL if no keymap exists."
  (cdr (assq mode evil-local-leader--mode-maps)))

(provide 'evil-local-leader)
;;; evil-local-leader.el ends here
