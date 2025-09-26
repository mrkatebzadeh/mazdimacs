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

;;<CODE>


(defun mazd//load (file)
  "Load FILE quietly, without displaying load messages."
  (load file nil :no-message))

(defun mazd//message-filter (msg &rest args)
  "Filter out noisy messages from `message`.

Suppress messages that are blank or contain \"Compiling\" or \"Loading\".
Other messages are passed through normally to MSG."
  (unless (or (null args)
              (string-blank-p (car args))
              (string-match-p "Compiling" (format "%s" (car args)))
              (string-match-p "Loading" (format "%s" (car args))))
    (let* ((str (car args))
           (format-args (cdr args)))
      (apply msg args))))

(advice-add 'message :around 'mazd//message-filter)

(defmacro mazd//loud (&rest body)
  "Evaluate BODY with `inhibit-message` disabled, ensuring messages are shown."
  `(let ((inhibit-message nil))
     ,@body))

(defun mazd//log (str &rest args)
  "Log STR formatted with ARGS as a green bullet message."
  (mazd//loud
   (message "%s %s"
            (propertize "•" 'face '(:foreground "green" :weight bold))
            (apply #'format str args))))

(defun mazd//err (str &rest args)
  "Log STR formatted with ARGS as a red bullet error message."
  (mazd//loud
   (message "%s %s"
            (propertize "•" 'face '(:foreground "red" :weight bold))
            (apply #'format str args))))



(defun mazd//clear-line ()
  "Clear the current and previous echo area lines."
  (mazd//loud (message "\033[2K\033[F\033[2K\033[F")))

(provide 'mazd-log)
;;; mazd-log.el ends here
