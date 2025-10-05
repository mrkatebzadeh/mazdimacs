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

;; Constants
(defconst mazd//version "0.0.2"
  "Current version of MK Emacs.")

;; Variables

(defconst mazd//emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst mazd//modules-dir (concat mazd//emacs-dir "modules/")
  "The root directory for Mazdimacs' modules. Must end with a slash.")

(defconst mazd//vars-file (concat mazd//modules-dir "mazd-vars.el")
  "The Mazdimacs' vars files. Must end with a slash.")

(defconst mazd//local-dir (concat mazd//emacs-dir ".local/")
  "Root directory for local storage.
Use this as a storage location for this system's installation of MK Emacs.
These files should not be shared across systems. By default, it is used by
`mazd//etc-dir' and `mazd//cache-dir'. Must end with a slash.")

(defconst mazd//etc-dir (concat mazd//local-dir "etc/")
  "Directory for non-volatile local storage.
Use this for files that don't change much, like server binaries, external
dependencies or long-term shared data. Must end with a slash.")

(defconst mazd//cache-dir (concat mazd//local-dir "cache/")
  "Directory for volatile local storage.
Use this for files that change often, like cache files. Must end with a slash.")

(defconst mazd//packages-dir (concat mazd//local-dir "packages/")
  "Where package.el and quelpa plugins (and their caches) are stored.
Must end with a slash.")

(defconst mazd//eln-dir (concat mazd//local-dir "eln-cache/")
  "Directory for eln-cache.")

(startup-redirect-eln-cache mazd//eln-dir)

(defconst mazd//lisp-dir (concat mazd//emacs-dir "site-lisp/")
  "The root directory of Mazdimacs' external files. Must end with a slash.")

(defconst mazd//backup-dir (concat mazd//local-dir "backups/")
  "The root directory of Mazdimacs' backup files. Must end with a slash.")

(defconst mazd//cache-dir (concat mazd//local-dir "cache/")
  "The root directory of Mazdimacs' cache files. Must end with a slash.")

(defconst mazd//autosave-dir (concat mazd//local-dir "autosave/")
  "The root directory of Mazdimacs' autosave files. Must end with a slash.")

(defconst mazd//eshell-dir (concat mazd//local-dir "eshell/")
  "The root directory of Mazdimacs' eshell files. Must end with a slash.")

(defconst mazd//desktop-dir (concat mazd//local-dir "desktop/")
  "Directory to save desktop sessions.")

(defconst mazd//variable-storage-file (concat mazd//cache-dir "mazd-vars.el")
  "File to store variable values." )

(defconst mazd//autoload-file (concat mazd//cache-dir "autoloads.el")
  "File to store autoloads." )

(setq org-directory     "~/Nextcloud/org")

(defconst mazd//tramp-backup-directory
  (concat mazd//cache-dir "tramp-backups/"))

(defconst mazd//tramp
  (concat mazd//cache-dir "tramp"))

(provide 'mazd-vars)
;;; mazd-vars.el ends here
