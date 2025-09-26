;;; mazd//package.el --- File  -*- lexical-binding: t; -*-

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

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

(dolist (keyword '(:async))
  (push keyword use-package-deferring-keywords)
  (setq use-package-keywords
        (use-package-list-insert keyword use-package-keywords :after)))

(defalias 'use-package-normalize/:async #'use-package-normalize-symlist)

(defvar mazd//async-packages nil
  "List of (PACKAGE . PRIORITY) for async loading.
Higher PRIORITY loads first. :async nil means skip, :async t means priority 0.
Symbolic priorities like 'high or 'medium are mapped to numbers in the handler.")

(defalias 'use-package-normalize/:async #'use-package-normalize-symlist)

(defun use-package-handler/:async (name _keyword targets rest state)
  "Handle the :async keyword for `use-package`.

Keyword values:

- `nil`      : package is not added to the async list
- `t`        : package is added with lowest priority (0)
- Symbol     : mapped to a numeric priority, e.g. 'high -> 10, 'medium -> 5, 'low -> 1"
  (let ((value (if (listp targets) (car targets) targets)))
    (when value
      (let ((priority (pcase value
                        ('t 0)
                        ('high 10)
                        ('medium 5)
                        ('low 1)
                        (_ 0))))
        (push (cons name priority) mazd//async-packages))))
  (use-package-process-keywords name rest state))


(defvar mazd//async-load-progress 0
  "Number of async packages loaded so far.")

(defvar mazd//async-load-total 0
  "Total number of packages to load asynchronously.")

(defvar mazd//current-async-package ""
  "The name of the current package being loaded asynchronously.")

(defvar mazd//async-idle-timer 1
  "Time in seconds between loading packages asynchronously.")

(defvar mazd//async-load-done-timer nil)

(defun mazd//async-mode-line ()
  "Return a string showing async package load progress for the mode-line.
Shows current progress while loading and a checkmark [✔] when all packages are loaded."
  (cond
   ((>= mazd//async-load-progress mazd//async-load-total)
    " [✔]")
   ((zerop mazd//async-load-total)
    "")
   (t
    (format " [Async %d/%d: %s]"
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
  (force-mode-line-update)
  (when (>= mazd//async-load-progress mazd//async-load-total)
    (message "All async packages loaded")))

(defun mazd//async-load-all-packages (&optional packages)
  "Load all packages in `mazd//async-packages` incrementally without freezing UI."
  (let* ((packages (or packages mazd//async-packages))
         (packages (sort (cl-copy-list packages) (lambda (a b) (> (cdr a) (cdr b)))))
         (packages (mapcar #'car packages))
         (packages (cl-remove-duplicates packages)))
    (setq mazd//async-load-total (length packages))
    (setq mazd//async-load-progress 0)
    (cl-labels ((load-next ()
                  (when packages
                    (let ((pkg (pop packages)))
                      (setq mazd//current-async-package (symbol-name pkg))
                      (condition-case err
			  (load (symbol-name pkg) nil t)
                        (error (message "Error loading %S: %S" pkg err)))
                      (setq mazd//async-load-progress (1+ mazd//async-load-progress))
                      (force-mode-line-update)
                      (when packages
                        (run-with-idle-timer mazd//async-idle-timer nil #'load-next))))))
      (load-next))))

(use-package s
  :defer t
  :ensure t)

(use-package f
  :defer t
  :ensure t)

(use-package restart-emacs
  :defer t
  :ensure t)

(use-package exec-path-from-shell
  :defer t
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  )

(use-package gcmh
  :ensure t
  :init
  (setq gcmh-verbose             t
        gcmh-lows-cons-threshold #x800000
	gcmh-high-cons-threshold (* 128 1024 1024)
	gcmh-idle-delay 20
	)

  :config
  (gcmh-mode))

(provide 'mazd-package)
;;; mazd//package.el ends here
