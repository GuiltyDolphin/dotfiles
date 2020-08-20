;; early-init.el --- Early Emacs Configuration File
;;
;; Copyright (C) 2020 Ben Moon
;; Author: Ben Moon <software@guiltydolphin.com>
;; URL: https://github.com/GuiltyDolphin/dotfiles
;; Git-Repository: git://github.com/GuiltyDolphin/dotfiles.git
;; Created: 2020-08-20

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

;;; Commentary:

;; Early configuration files for Emacs (loaded before init.el).

;;; Code:

;; Ensure MELPA packages are available
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; We load cask early to allow overriding built-in packages.
;;
;; Specifically, we need to initialize cask before calling
;; 'org-babel-load-file, in order to prevent the built-in org being
;; loaded.
(add-to-list 'load-path (locate-user-emacs-file "cask/elpa"))
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;;; early-init.el ends here
