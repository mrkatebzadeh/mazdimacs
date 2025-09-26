;;; mazd-depends.el --- DEPENDS -*- lexical-binding: t; -*-

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

(defvar mazd//deferred-packages-alist '(t))

(dolist (keyword '(:depends :after-call))
  (push keyword use-package-deferring-keywords)
  (setq use-package-keywords
        (use-package-list-insert keyword use-package-keywords :after)))

(defalias 'use-package-normalize/:depends #'use-package-normalize-symlist)

(defvar mazd//incremental-load-progress 0
  "Tracks the number of packages loaded incrementally.")

(defvar mazd//incremental-load-total 1
  "Total number of packages to load incrementally. Set this dynamically.")

(defvar mazd//current-loading-package ""
  "The current package being loaded.")

(defun mazd//incremental-load-update (package-name)
  "Update the mode-line progress indicator."
  (setq mazd//incremental-load-progress (1+ mazd//incremental-load-progress))
  (setq mazd//current-loading-package package-name)
  (force-mode-line-update))

(defun mazd//incremental-load-mode-line ()
  "Return a string displaying the incremental package loading progress.
If `mazd//incremental-load-total` is 0, return an empty string."
  (if (zerop mazd//incremental-load-total)
      ""
    (if (>= mazd//incremental-load-progress mazd//incremental-load-total)
        ""
      (format " ⟪Incremental %d/%d: %s⟫"
	      mazd//incremental-load-progress
              mazd//incremental-load-total
              mazd//current-loading-package))))

;; Add to mode-line
;; (setq-default mode-line-format
;; (append mode-line-format '((:eval (mazd//incremental-load-mode-line)))))

(defvar mazd//incremental-packages '(t)
  "A list of packages to load incrementally after startup. Any large packages
here may cause noticeable pauses, so it's recommended you break them up into
sub-packages. For example, `org' is comprised of many packages, and might be
broken up into:

  (mazd//load-packages-incrementally
   '(calendar find-func format-spec org-macs org-compat
     org-faces org-entities org-list org-pcomplete org-src
     org-footnote org-macro ob org org-clock org-agenda
     org-capture))")

(defvar mazd//incremental-idle-timer 0.75
  "How long (in idle seconds) in between incrementally loading packages.")

(defvar mazd//incremental-first-idle-timer (if (daemonp) 0 2.0)
  "How long (in idle seconds) until incremental loading starts.

Set this to nil to disable incremental loading at startup.
Set this to 0 to load all incrementally deferred packages immediately at
`mazd//after-init-hook'.")

(defun use-package-handler/:depends (name _keyword targets rest state)
  (use-package-concat
   `((mazd//load-packages-incrementally
      ',(if (equal targets '(t))
            (list name)
          (append targets (list name)))))
   (use-package-process-keywords name rest state)))


(defun mazd//load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.

If NOW is non-nil, PACKAGES will be marked for incremental loading next time
Emacs is idle for `mazd//incremental-first-idle-timer' seconds (falls back to
`mazd//incremental-idle-timer'), then in `mazd//incremental-idle-timer' intervals
afterwards."

  (setq mazd//incremental-load-total (length (cl-remove-duplicates mazd//incremental-packages)))
  (let* ((gc-cons-threshold most-positive-fixnum)
         (first-idle-timer (or mazd//incremental-first-idle-timer
                               mazd//incremental-idle-timer)))

    (if (not now)
        (cl-callf append mazd//incremental-packages packages)
      (while packages
        (let ((req (pop packages))
              idle-time)
          (if (featurep req)
	      (mazd//incremental-load-update (symbol-name req))
            (condition-case-unless-debug e
                (and
                 (or (null (setq idle-time (current-idle-time)))
                     (< (float-time idle-time) first-idle-timer)
                     (not
                      (while-no-input
                        ;; (message "start:iloader: Loading %s (%d left)" req (length packages))
                        (let ((default-directory mazd//emacs-dir)
                              (inhibit-message t)
                              (file-name-handler-alist
                               (list (rassq 'jka-compr-handler file-name-handler-alist))))
                          (require req nil t)
			  (mazd//incremental-load-update (symbol-name req))
                          t))))
                 (push req packages))
              (error
               (message "Error: failed to incrementally load %S because: %s" req e)
               (setq packages nil)))
            (if (null packages)
		()
	      (run-at-time (if idle-time
			       mazd//incremental-idle-timer
                             first-idle-timer)
                           nil #'mazd//load-packages-incrementally
                           packages t)
              (setq packages nil)
	      ;; after started up, reset GC threshold to normal.
	      (run-with-idle-timer 4 nil
				   (lambda ()
				     "Clean up gc."
				     (setq gc-cons-threshold  67108864) ; 64M
				     (setq gc-cons-percentage 0.1) ; original value
				     (garbage-collect)))
	      )))))))

(defun mazd//load-packages-incrementally-h ()
  "Begin incrementally loading packages in `mazd//incremental-packages'.

If this is a daemon session, load them all immediately instead."
  (when (numberp mazd//incremental-first-idle-timer)
    (if (zerop mazd//incremental-first-idle-timer)
        (mapc #'require (cdr mazd//incremental-packages))
      (run-with-idle-timer mazd//incremental-first-idle-timer
                           nil #'mazd//load-packages-incrementally
                           (cdr mazd//incremental-packages) t))))

(add-hook 'emacs-startup-hook #'mazd//load-packages-incrementally-h)

(provide 'mazd-depends)
;;; mazd-depends.el ends here
