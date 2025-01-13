;;; init.el --- init file -*- lexical-binding: t; -*-

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

;;; Path vars
;;(setq user-emacs-directory (file-name-directory load-file-name))
(defvar mazd//emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defvar mazd//vars-file (concat mazd//emacs-dir "mazd-vars.el")
  "The MK's vars files. Must end with a slash.")

(defvar mazd//core-file (concat mazd//emacs-dir "mazd-core.el")
  "The root directory of MK's core files. Must end with a slash.")

(defvar mazd//key-file (concat mazd//emacs-dir "mazd-key.el")
  "The root directory of MK's key configs. Must end with a slash.")

(defvar mazd//modules-dir (concat mazd//emacs-dir "modules/")
  "The root directory for MK's modules. Must end with a slash.")

(defvar mazd//lisp-dir (concat mazd//emacs-dir "site-lisp/")
  "The root directory of MK's external files. Must end with a slash.")

(defvar mazd//ui-file (concat mazd//emacs-dir "mazd-ui.el")
  "The root directory of MK's UI files. Must end with a slash.")

(defvar mazd//backup-dir (concat mazd//emacs-dir ".backups/")
  "The root directory of MK's backup files. Must end with a slash.")

(defvar mazd//cache-dir (concat mazd//emacs-dir ".cache/")
  "The root directory of MK's cache files. Must end with a slash.")

(defvar mazd//autosave-dir (concat mazd//emacs-dir ".autosave/")
  "The root directory of MK's autosave files. Must end with a slash.")

(defvar mazd//eshell-dir (concat mazd//emacs-dir ".eshell/")
  "The root directory of MK's eshell files. Must end with a slash.")

(defvar mazd//desktop-dir (concat mazd//emacs-dir ".desktop/")
  "Directory to save desktop sessions.")

(defvar mazd//completion "light"
  "Completion frameworks: light -> vertico/consult/corf, featured -> helm/company ")

(defvar mazd//language-server "eglot"
  "Language server frameworks: eglot, lsp, or bridge")

(setq org-directory     "~/Dropbox/org")

(defvar mazd//alpha-variable 90
  "Default transparency level to toggle with 100.")

(message "Starting Mazdimacs")

;;; Increase the CPU processing restrictions
(when (boundp 'read-process-output-max)
  (setq process-adaptive-read-buffering nil
        read-process-output-max (* 24 1024 1024)))

;;; Speed up startup
(eval-when-compile (require 'cl-lib))

(let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "[Emacs initialized in %.3fs]" elapsed)))))

;;; Increase gc threshold to speedup starting up
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

;;; Basic configs
(setq warning-minimum-level :emergency)
(setq eshell-directory-name mazd//eshell-dir)
(setq pcache-directory (concat mazd//cache-dir "/var/pcache"))
(setq transient-history-file (concat mazd//cache-dir "/transient/history.el"))
(setq srecode-map-save-file (concat mazd//cache-dir "/srecode-map.el"))
(setq projectile-cache-file (concat mazd//cache-dir "/projectile.cache"))
					; stop creating backup~ files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)

(let ((backup-dir mazd//backup-dir)
      (auto-saves-dir mazd//autosave-dir))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))
(setq backup-by-copying t
      delete-old-versions t
      version-control t
      kept-new-versions 5
      kept-old-versions 2)
(setq custom-file (concat mazd//backup-dir "custom.el"))
(load custom-file 'noerror)

;; Load directory function
(defun load-directory (dir)
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))
		 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(defun load-modules (dir)
  (mapcar 'load (directory-files-recursively dir "")))

(add-to-list 'load-path mazd//lisp-dir)

(load mazd//vars-file)
(message "Vars has been loaded.")

(load mazd//core-file)
(message "Core has been loaded.")

(load mazd//key-file)
(message "Key has been loaded.")

;;; Load Theme
(load mazd//ui-file)
;;; Load modules
(load-modules mazd//modules-dir)
;;; run server
(require 'server)
(unless (server-running-p)
  (server-start))
(add-to-list 'exec-path "/usr/local/bin/")
(add-to-list 'exec-path "/usr/local/texlive/2019basic/bin/x86_64-darwin/")
(add-to-list 'exec-path "/Library/TeX/texbin/")
(add-to-list 'exec-path "/run/current-system/sw/bin")
(add-to-list 'exec-path "~/.nix-profile/bin/")
(add-to-list 'exec-path "/opt/homebrew/bin/")

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/"))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2019basic/bin/x86_64-darwin/"))
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
(setenv "PATH" (concat (getenv "PATH") ":/run/current-system/sw/bin"))
(setenv "PATH" (concat (getenv "PATH") ":~/.nix-profile/bin/"))
(setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin/"))


;; after started up, reset GC threshold to normal.
(run-with-idle-timer 4 nil
                     (lambda ()
                       "Clean up gc."
                       (setq gc-cons-threshold  67108864) ; 64M
                       (setq gc-cons-percentage 0.1) ; original value
                       (garbage-collect)))
(provide 'init)

;;; init.el ends here
