;; init.el --- Emacs Configuration File
;;
;; Copyright (C) 2015-2018 Ben Moon
;; Author: Ben Moon <software@guiltydolphin.com>
;; URL: https://github.com/GuiltyDolphin/dotfiles
;; Git-Repository: git://github.com/GuiltyDolphin/dotfiles.git
;; Created: 2015-03-01

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

;; Configuration files for Emacs.

;;; Code:

;; some important config moved to early-init, so if using Emacs < 27,
;; manually load it up
(when (< emacs-major-version 27)
  (load-file (locate-user-emacs-file "early-init.el"))
  (package-initialize))

;; set file for custom variables
(setq custom-file (locate-user-emacs-file ".emacs-custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; This needs to be before loading config.org so I don't have to give
;; permission to load the file when it is symlinked.
(customize-set-variable
 'vc-follow-symlinks t "Follow Symlinks without asking")

;; been having issues with config not correctly reloading, so manually
;; deleting the files beforehand...
(delete-file (locate-user-emacs-file "config.el"))
(org-babel-load-file (locate-user-emacs-file "config.org"))

;;; init.el ends here
