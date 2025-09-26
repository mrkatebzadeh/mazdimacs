;;; settings.el --- Settings -*- lexical-binding: t; -*-

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

;; List of variables to persist
(defvar mazd//persistent-variables
  '(
    mazd//alpha-variable
    mazd//icon
    mazd//font-size
    mazd//font-family
    mazd//theme
    )
  "List of variables whose values should persist across Emacs sessions.")

(defvar mazd//alpha-variable 90
  "Default transparency level to toggle with 100.")

(defvar mazd//icon t)

(defvar mazd//font-default-size 120
  "Default font size for resetting.")

(defvar mazd//font-default-family "FiraCode Nerd Font"
  "Default font family for resetting.")

(defvar mazd//font-size mazd//font-default-size
  "Current font size.")

(defvar mazd//font-family mazd//font-default-family
  "Current font family.")

;; Define the default theme
(defvar mazd//default-theme "frappe"
  "Default theme for Emacs.")

;; Initialize the current theme variable with the default
(defvar mazd//theme mazd//default-theme
  "Current active theme.")

;; Function to save all persistent variables
(defun mazd//save-persistent-settings ()
  "Save all variables in `my-persistent-variables` to a file."
  (with-temp-file mazd//variable-storage-file
    (insert ";; Automatically generated file, do not edit manually\n")
    (dolist (var mazd//persistent-variables)
      (when (boundp var)
        (insert (format "(setq %S %S)\n" var (symbol-value var)))))))

;; Load stored values if the file exists
(when (file-exists-p mazd//variable-storage-file)
  (load mazd//variable-storage-file))

;; Ensure variables are saved when Emacs exits
(add-hook 'kill-emacs-hook #'mazd//save-persistent-settings)

(provide 'mazd-settings)
;;; mazd-settings.el ends here
