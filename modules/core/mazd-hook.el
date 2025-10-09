;;; mazd-hook.el --- MAZD-HOOK -*- lexical-binding: t; -*-

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

(defvar mazd--hook nil)
(defun mazd//run-hook (hook)
  "Run hooks (a hook function) with better error handling.
Meant to be used with `run-hook-wrapped'."
  (mazd//log "hook:%s: run %s" (or mazd--hook '*) hook)
  (condition-case-unless-debug e
      (funcall hook)
    (error
     (signal 'mazd-hook-error (list hook e))))
  nil)

(defun mazd//run-hooks (&rest hooks)
  "Run hoosk (a list of hook variable symbols) with better error handling.
Is used as advice to replace `run-hooks'."
  (dolist (hook hooks)
    (condition-case-unless-debug e
        (let ((mazd--hook hook))
          (run-hook-wrapped hook #'mazd//run-hook))
      (mazd//err
       (unless debug-on-error
         (lwarn hook :error "Error running hook %S because: %s"
                (if (symbolp (cadr e))
                    (symbol-name (cadr e))
                  (cadr e))
                (caddr e)))
       (signal 'mazd//hook-error (cons hook (cdr e)))))))

(defun mazd//run-hook-on (hook-var trigger-hooks)
  (dolist (hook trigger-hooks)
    (let ((fn (make-symbol (format "chain-%s-to-%s-h" hook-var hook)))
          running?)
      (fset
       fn (lambda (&rest _)
            (when (and (not running?)
                       (bound-and-true-p after-init-time)
                       (or (daemonp)
                           (and (boundp hook)
                                (symbol-value hook))))
              (setq running? t)
              (mazd//run-hooks hook-var)
              (set hook-var nil))))
      (when (daemonp)
        (add-hook 'server-after-make-frame-hook fn 'append))
      (if (eq hook 'find-file-hook)
          (advice-add 'after-find-file :before fn '((depth . -101)))
        (add-hook hook fn -101))
      fn)))
(defcustom mazd//first-input-hook ()
  "Transient hooks run before the first user input."
  :type 'hook
  :local 'permanent-local
  :group 'mazd)

(defcustom mazd//first-file-hook ()
  "Transient hooks run before the first interactively opened file."
  :type 'hook
  :local 'permanent-local
  :group 'mazd)

(defcustom mazd//first-buffer-hook ()
  "Transient hooks run before the first interactively opened buffer."
  :type 'hook
  :local 'permanent-local
  :group 'mazd)

(mazd//run-hook-on 'mazd//first-buffer-hook '(find-file-hook))
(mazd//run-hook-on 'mazd//first-file-hook   '(find-file-hook dired-initial-position-hook))
(mazd//run-hook-on 'mazd//first-input-hook  '(pre-command-hook))

(provide 'mazd-hook)
;;; mazd-hook.el ends here
