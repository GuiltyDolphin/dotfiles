;;; Provide additional means of remapping keys in evil-mode
;;; akin to those found in Vim.
;;;
;;; Overview of which functions affect which modes:
;;; FUNCTIONS      MODES
;;; noremap        Normal, Visual, Operator-pending
;;; nnoremap       Normal
;;; vnoremap       Visual
;;; xnoremap       Ex-mode
;;; onoremap       Operator-pending
;;; inoremap       Insert
;;; enoremap       Emacs
;;; mnoremap       Motion
;;; nnoremap!      Normal, Visual, Operator-pending, Motion

;; Copyright (C) 2015 Ben Moon
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



;;; (evil-noremap! ";" 'evil-ex)
;;; Code:

(require 'evil)

(defun evil-noremap (key command)
  "Bind key in evil normal, visual and operator modes."
  (evil-nnoremap key command)
  (evil-vnoremap key command)
  (evil-onoremap key command))

(defun evil-nnoremap (key command)
  "Bind key in evil normal mode."
  (define-key evil-normal-state-map key command))

;;;
(defun evil-nnoremap-mode (keymap key command)
  (evil-define-key 'normal key command))

(defun evil-define-key-multi (states keymap key def)
  "For each STATE in STATES, create a STATE binding from KEY to
DEF for the given KEYMAP.

STATES must be a list of valid evil-mode states.
See `evil-define-key' for more information."
  (dolist (state states)
    (evil-define-key state keymap key def)))
;;;

(defun evil-inoremap-mode (keymap key command)
  (evil-define-key-multi '(insert) key command))

(defun evil-vnoremap (key command)
  "Bind key in evil visual mode."
  (define-key evil-visual-state-map key command))

(defun evil-xnoremap (key command)
  (define-key evil-ex-map key command))

(defun evil-onoremap (key command)
  "Bind key in evil operator mode."
  (define-key evil-operator-state-map key command))

(defun evil-inoremap (key command)
  "Bind key in evil insert mode."
  (define-key evil-insert-state-map key command))

(defun evil-enoremap (key command)
  "Bind key in evil emacs mode."
  (define-key evil-emacs-state-map key command))

(defun evil-mnoremap (key command)
  "Bind key in evil motion mode."
  (define-key evil-motion-state-map key command))

(defun evil-nnoremap! (key command)
  (evil-nnoremap key command)
  (evil-mnoremap key command))

(provide 'evil-remap)
;;; evil-map.el ends here
