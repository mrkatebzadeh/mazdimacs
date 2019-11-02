;;; core.el --- package management system -*- lexical-binding: t; -*-

;; Copyright (C) 2019  M.R. Siavash Katebzadeh

;; Author: M.R.Siavash Katebzadeh <mr.katebzadeh@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.



;;; Constants
(defconst mk-version "0.0.1"
  "Current version of MK Emacs.")

;;; Variables
(defvar mk-local-dir (concat mk-emacs-dir ".local/")
  "Root directory for local storage.
Use this as a storage location for this system's installation of MK Emacs.
These files should not be shared across systems. By default, it is used by
`mk-etc-dir' and `mk-cache-dir'. Must end with a slash.")

(defvar mk-etc-dir (concat mk-local-dir "etc/")
  "Directory for non-volatile local storage.
Use this for files that don't change much, like server binaries, external
dependencies or long-term shared data. Must end with a slash.")

(defvar mk-cache-dir (concat mk-local-dir "cache/")
  "Directory for volatile local storage.
Use this for files that change often, like cache files. Must end with a slash.")

(defvar mk-packages-dir (concat mk-local-dir "packages/")
  "Where package.el and quelpa plugins (and their caches) are stored.
Must end with a slash.")


;;; Uncomment the following line if you got “no match” error after `M-x package-install` on a package on MacOS
;;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;;; Initialize package.el
(require 'package)
(setq package--init-file-ensured t
      initial-scratch-message ""
      tab-width 4
      select-enable-clipboard t
      user-full-name "M.R. Siavash Katebzadeh"
      user-mail-address "mr.katebzadeh@gmail.com"
      package-user-dir (expand-file-name "elpa" mk-packages-dir)
      package-gnupghome-dir (expand-file-name "gpg" mk-packages-dir)
      package-enable-at-startup nil
      help-window-select t
      package-archives
      `(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("org"          . "https://orgmode.org/elpa/"))
      package-archive-priorities
      '(("melpa" . -1)
	("gnu" . -3)))
(package-initialize)
;;; Bootstrap `use-package'
(setq-default use-package-always-ensure t ; Auto-download package if not exists
              use-package-always-defer t ; Always defer load package to speed up startup
              use-package-verbose nil ; Don't report loading details
              use-package-expand-minimally t  ; make the expanded code as minimal as possible
              use-package-enable-imenu-support t) ; Let imenu finds use-package definitions
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;;; Load core-packages
(require 'core-packages)

(provide 'core)
;;; core.el ends here
