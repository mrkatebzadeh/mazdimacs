;;; mazd-log.el --- LOG -*- lexical-binding: t; -*-

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

(defun mazd//load (file)
  "Load FILE quietly, without displaying load messages."
  (load file nil :no-message))

(defun mazd//message-filter (orig-func &rest args)
  "Filter out unwanted messages but always call ORIG-FUNC."
  (let ((msg (car args)))
    (unless (or (null msg)
                (string-blank-p msg)
                (string-match-p "Compiling" msg)
                (string-match-p "Loading" msg))
      (apply orig-func args))))
(advice-add 'message :around #'mazd//message-filter)

(defmacro mazd//loud (&rest body)
  "Evaluate BODY with `inhibit-message` disabled, ensuring messages are shown."
  `(let ((inhibit-message nil))
     ,@body))

(defvar mazd//catppuccin-colors
  '((green . "#8bd5ca")
    (red   . "#ed8796"))
  "Catppuccin Frappe colors for logging.")

(defun mazd//log (str &rest args)
  "Log STR formatted with ARGS as a green bullet message to *Messages* without echo."
  (mazd//loud
   (message "%s %s"
            (propertize "•" 'face `(:foreground ,(alist-get 'green mazd//catppuccin-colors) :weight bold))
            (apply #'format str args))))

(defun mazd//err (str &rest args)
  "Log STR formatted with ARGS as a red bullet error message (echoed)."
  (mazd//loud
   (message "%s %s"
            (propertize "•" 'face `(:foreground ,(alist-get 'red mazd//catppuccin-colors) :weight bold))
            (apply #'format str args))))

(defun mazd//clear-line ()
  "Clear the current and previous echo area lines."
  (mazd//loud (message "\033[2K\033[F\033[2K\033[F")))

(provide 'mazd-log)
;;; mazd-log.el ends here
