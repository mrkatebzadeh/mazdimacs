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

(defvar mazd//emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defvar mazd//modules-dir (concat mazd//emacs-dir "modules/")
  "The root directory for Mazdimacs' modules. Must end with a slash.")

(defvar mazd//vars-file (concat mazd//modules-dir "mazd-vars.el")
  "The Mazdimacs' vars files. Must end with a slash.")

(defvar mazd//local-dir (concat mazd//emacs-dir ".local/")
  "Root directory for local storage.
Use this as a storage location for this system's installation of MK Emacs.
These files should not be shared across systems. By default, it is used by
`mazd//etc-dir' and `mazd//cache-dir'. Must end with a slash.")

(defvar mazd//etc-dir (concat mazd//local-dir "etc/")
  "Directory for non-volatile local storage.
Use this for files that don't change much, like server binaries, external
dependencies or long-term shared data. Must end with a slash.")

(defvar mazd//cache-dir (concat mazd//local-dir "cache/")
  "Directory for volatile local storage.
Use this for files that change often, like cache files. Must end with a slash.")

(defvar mazd//packages-dir (concat mazd//local-dir "packages/")
  "Where package.el and quelpa plugins (and their caches) are stored.
Must end with a slash.")

(defvar mazd//core-file (concat mazd//modules-dir "mazd-core.el")
  "The root directory of Mazdimacs' core files. Must end with a slash.")

(defvar mazd//key-file (concat mazd//modules-dir "mazd-key.el")
  "The root directory of Mazdimacs' key configs. Must end with a slash.")

(defvar mazd//ui-file (concat mazd//modules-dir "mazd-ui.el")
  "The root directory of Mazdimacs' UI files. Must end with a slash.")

(defvar mazd//lisp-dir (concat mazd//emacs-dir "site-lisp/")
  "The root directory of Mazdimacs' external files. Must end with a slash.")

(defvar mazd//backup-dir (concat mazd//emacs-dir ".backups/")
  "The root directory of Mazdimacs' backup files. Must end with a slash.")

(defvar mazd//cache-dir (concat mazd//emacs-dir ".cache/")
  "The root directory of Mazdimacs' cache files. Must end with a slash.")

(defvar mazd//autosave-dir (concat mazd//emacs-dir ".autosave/")
  "The root directory of Mazdimacs' autosave files. Must end with a slash.")

(defvar mazd//eshell-dir (concat mazd//emacs-dir ".eshell/")
  "The root directory of Mazdimacs' eshell files. Must end with a slash.")

(defvar mazd//desktop-dir (concat mazd//emacs-dir ".desktop/")
  "Directory to save desktop sessions.")

(setq org-directory     "~/Dropbox/org")

(defvar mazd//alpha-variable 90
  "Default transparency level to toggle with 100.")


(defvar mazd//completion "light"
  "Completion frameworks: light -> vertico/consult/corf, featured -> helm/company ")

(defvar mazd//language-server "eglot"
  "Language server frameworks: eglot, lsp, or bridge")

(defvar mazd//tramp-backup-directory
  (concat mazd//cache-dir "tramp-backups/"))

(defvar mazd//tramp
  (concat mazd//cache-dir "tramp"))

(provide 'mazd-vars)
;;; mazd-vars.el ends here
