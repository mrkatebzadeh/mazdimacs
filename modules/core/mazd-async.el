;;; mazd-async.el --- ASYNC -*- lexical-binding: t; -*-

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

(defvar mazd//async-packages nil
  "List of packages registered for async loading.
Each element is a plist: (:package PACKAGE :priority PRIORITY).")

(dolist (keyword '(:async))
  (push keyword use-package-deferring-keywords)
  (setq use-package-keywords
        (use-package-list-insert keyword use-package-keywords :after)))

(defun use-package-normalize/:async (name-symbol keyword args)
  "Normalize the :async keyword for `use-package`.

ARGS can be:
- t            => load NAME-SYMBOL asynchronously with default priority 0
- number       => load NAME-SYMBOL asynchronously with numeric priority
- symbol       => a single package symbol, default priority 0
- (:priority P :packages (...)) => custom priority and package list"
  (use-package-only-one (symbol-name keyword) args
    (lambda (_label arg)
      (cond
       ((eq arg t)
        `(:priority 0 :packages (,name-symbol)))
       ((numberp arg)
        `(:priority ,arg :packages (,name-symbol)))
       ((symbolp arg)
        `(:priority 0 :packages (,arg)))
       ((and (listp arg) (keywordp (car arg)))
        arg)
       (t
        (use-package-error
         ":async argument must be t, number, symbol, or plist with :priority and :packages"))))))

(defun mazd//async-priority-to-number (priority)
  "Convert PRIORITY to a numeric value for async loading.

Accepts numbers or symbols: high=10, medium=5, low=0."
  (cond
   ((numberp priority) priority)
   ((eq priority 'high) 10)
   ((eq priority 'medium) 5)
   ((eq priority 'low) 0)
   (t 0)))

(defun use-package-handler/:async (name _keyword targets rest state)
  "Handle the :async keyword for `use-package` with optional priority.

TARGETS is always normalized to a plist by `use-package-normalize/:async`."
  (let ((plist (if (and (listp targets)
                        (= (length targets) 1)
                        (listp (car targets)))
                   (car targets)
                 targets)))
    (let* ((raw-priority (plist-get plist :priority))
           (priority (mazd//async-priority-to-number raw-priority))
           (pkgs (plist-get plist :packages)))
      (unless (listp pkgs) (setq pkgs (list pkgs)))
      (dolist (pkg pkgs)
        (cl-pushnew (list :package pkg :priority priority)
                    mazd//async-packages
                    :test #'equal))))
  (use-package-process-keywords name rest state))

(defvar mazd//async-load-progress 0
  "Tracks the number of packages loaded asynchronously.")

(defvar mazd//async-load-total 0
  "Total number of packages to load asynchronously.")

(defvar mazd//current-async-package ""
  "Current async package being loaded.")

(defvar mazd//async-idle-timer 0.05
  "Seconds between async package loads.")

(defun mazd//async-mode-line ()
  "Return a string showing async package load progress for the mode-line.
Returns an empty string if no async packages are registered.
Shows current progress while loading and a checkmark ✔ when all packages are loaded."
  (cond
   ((zerop mazd//async-load-total)
    "")
   ((>= mazd//async-load-progress mazd//async-load-total)
    " ⟦Async ✔⟧")
   (t
    (format " ⟦Async %d/%d: %s⟧"
            mazd//async-load-progress
            mazd//async-load-total
            mazd//current-async-package))))

(defun mazd//async-load-update (package-name)
  "Update progress counters for async package loading.
PACKAGE-NAME is the name of the package that was just loaded.
Updates `mazd//async-load-progress` and `mazd//current-async-package`,
forces a mode-line update, and prints a message when all async packages are loaded."
  (setq mazd//async-load-progress (1+ mazd//async-load-progress))
  (setq mazd//current-async-package package-name)
  (when (>= mazd//async-load-progress mazd//async-load-total)
    (message "All async packages loaded")))

(defun mazd//async-load-all-packages (&optional packages)
  "Load all packages in `mazd//async-packages` incrementally using idle timers.
Respects priority order: higher priority loaded first."
  (message "Incremental loading started")
  (let* ((packages (or packages mazd//async-packages))
         (packages (sort (cl-copy-list packages)
                         (lambda (a b) (> (plist-get a :priority)
                                          (plist-get b :priority))))))

    (setq packages (mapcar (lambda (p) (plist-get p :package)) packages))
    (setq packages (cl-remove-duplicates packages))
    (setq mazd//async-load-total (length packages))
    (setq mazd//async-load-progress 0)
    (cl-labels ((load-next ()
                  (when packages
                    (let ((pkg (pop packages)))
                      (setq mazd//current-async-package (symbol-name pkg))
                      (condition-case err
                          (require pkg nil t)
                        (error (message "Error loading %S: %S" pkg err)))
		      (mazd//async-load-update (symbol-name pkg))
                      (when packages
                        (run-with-idle-timer mazd//async-idle-timer nil #'load-next))))))
      (load-next))))


(add-hook 'emacs-startup-hook
	  (lambda ()
	    (mazd//schedule 0 nil
			    (mazd//async-load-all-packages))))

(provide 'mazd-async)
;;; mazd-async.el ends here
