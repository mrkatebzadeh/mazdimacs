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

;; Normalize function for :async (just a pass-through)
(defalias 'use-package-normalize/:async #'use-package-normalize-symlist)

;; Handler for :async
(defun use-package-handler/:async (name _keyword targets rest state)
  "Register the package NAME as async."
  (use-package-concat
   `((add-to-list 'mazd//async-packages ',name))
   (use-package-process-keywords name rest state)))

(defvar mazd//async-load-progress 0
  "Number of async packages loaded so far.")

(defvar mazd//async-load-total 0
  "Total number of packages to load asynchronously.")

(defvar mazd//current-async-package ""
  "The name of the current package being loaded asynchronously.")

(defvar mazd//async-idle-timer 0.5
  "Time in seconds between loading packages asynchronously.")

(defun mazd//async-load-update (package-name)
  "Update progress counters for async package loading."
  (setq mazd//async-load-progress (1+ mazd//async-load-progress))
  (setq mazd//current-async-package package-name)
  (message "Async loading [%d/%d]: %s"
           mazd//async-load-progress
           mazd//async-load-total
           package-name)
  (force-mode-line-update))

(defun mazd//async-load-all-packages (&optional packages)
  "Asynchronously load all packages in `mazd//async-packages`."
  (let* ((packages (or packages mazd//async-packages))
         (packages (cl-remove-duplicates packages))
         (mazd//async-load-total (length packages))
         (mazd//async-load-progress 0))
    (cl-labels ((load-next ()
                  (when packages
                    (let ((pkg (pop packages)))
                      (setq mazd//current-async-package (symbol-name pkg))
                      (condition-case err
                          (progn
                            (require pkg)
                            (mazd//async-load-update (symbol-name pkg)))
                        (error (message "Error loading %S: %S" pkg err)))
                      (when packages
                        (run-with-idle-timer
                         mazd//async-idle-timer nil
                         #'load-next))))))
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
