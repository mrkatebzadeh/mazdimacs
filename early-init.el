;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

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

;;; Commentary:

;;

;;; Code:

(defconst mazd//modules
  '("core" "ui" "editor" "langs" "extra" "completion" "org")
  "List of module names under `mazd//module-dir`.")

(setq package-enable-at-startup nil)

(setenv "LSP_USE_PLISTS" "true")

(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

(setq inhibit-startup-message t)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
(blink-cursor-mode -1)

(setq native-comp-async-report-warnings-errors 'silent)
(setq org-startup-with-inline-images t)

(load (concat (file-truename user-emacs-directory) "mazd-log.el"))

(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil)
  (setq mac-left-option-modifier nil)
  (setq mac-right-option-modifier nil)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (setq mac-control-modifier 'control))

(provide 'early-init)
;;; early-init.el ends here
