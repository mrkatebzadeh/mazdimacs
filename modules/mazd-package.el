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

;;; Code:

;; -*- lexical-binding: t -*-
(eval-when-compile
  (require 'mazd-func))
(require 'cl-lib)

(eval-and-compile
(setq straight-repository-branch "develop"
      straight-check-for-modifications '(check-on-save)
      straight-use-package-version 'ensure
      straight-use-package-by-default t
      straight-recipes-gnu-elpa-use-mirror t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage)))

;; =============================================
;; Setup use-package
;; =============================================

(eval-and-compile
  (straight-use-package '(use-package
                           :type git
                           :host github
                           :repo "raxod502/use-package"))

  (straight-use-package '(el-patch
                          :type git
                          :host github
                          :repo "raxod502/el-patch")))

(eval-when-compile (require 'el-patch))

(eval-and-compile
  (setq el-patch-use-aggressive-defvar t)
  (defvar el-patch--patches (make-hash-table))

  (with-eval-after-load 'el-patch
    (el-patch-deftype evil-define-command
		      :classify el-patch-classify-function
		      :locate el-patch-locate-function
		      :declare ((doc-string 3)
				(indent defun)))
    (el-patch-deftype evil-define-motion
		      :classify el-patch-classify-function
		      :locate el-patch-locate-function
		      :declare ((doc-string 3)
				(indent defun)))
    (el-patch-deftype evil-define-text-object
		      :classify el-patch-classify-function
		      :locate el-patch-locate-function
		      :declare ((doc-string 3)
				(indent defun)))
    (el-patch-deftype evil-define-operator
		      :classify el-patch-classify-function
		      :locate el-patch-locate-function
		      :declare ((doc-string 3)
				(indent defun))))

  (el-patch-feature use-package)

  (with-eval-after-load 'use-package
    (setq use-package-always-ensure t
          use-package-always-defer t)

    (define-advice straight-use-package-ensure-function
        (:around (old-fun &rest args) y-or-n-p-always-t)
      (cl-letf* (((symbol-function #'y-or-n-p) (lambda (_prompt) t)))
        (apply old-fun args)))

    (el-patch-defun use-package-handler/:ensure (name keyword ensure rest state)
		    (let* ((body (use-package-process-keywords name rest
							       ;; Here we are conditionally updating the marker
							       ;; value for deferred installation; this will be
							       ;; checked later by `:config'. For more information
							       ;; see `use-package-handler/:defer-install'.
							       (if (eq (plist-get state :defer-install)
								       :defer-install)
								   (plist-put state :defer-install :ensure)
								 state))))
		      ;; We want to avoid installing packages when the `use-package'
		      ;; macro is being macro-expanded by elisp completion (see
		      ;; `lisp--local-variables'), but still do install packages when
		      ;; byte-compiling to avoid requiring `package' at runtime.
		      (el-patch-add
		       (when (and (bound-and-true-p byte-compile-current-file)
				  (not (plist-get state :defer-install)))
			 ;; Eval when byte-compiling,
			 (funcall use-package-ensure-function
				  name ensure state :byte-compile)))
		      (cond
		       ((plist-get state :defer-install)
			(push
			 `(puthash ',name '(,ensure . ,state)
				   use-package--deferred-packages)
			 body)
			(push `(,use-package-pre-ensure-function
				',name ',ensure ',state)
			      body))
		       (el-patch-remove
			((bound-and-true-p byte-compile-current-file)
			 ;; Eval when byte-compiling,
			 (funcall use-package-ensure-function
				  name ensure state :byte-compile)))
		       ;;  or else wait until runtime.
		       (t (el-patch-wrap 2
					 (unless (and (not ensure)
						      (eq use-package-ensure-function
							  'straight-use-package-ensure-function))
					   (push `(,use-package-ensure-function
						   ',name ',ensure ',state :ensure)
						 body)))))
		      body)))

  (autoload 'use-package-install-deferred-package "use-package"
    "Install a package whose installation has been deferred.
NAME should be a symbol naming a package (actually, a feature).
This is done by calling `use-package-ensure-function' is called
with four arguments: the key (NAME) and the two elements of the
cons in `use-package--deferred-packages' (the value passed to
`:ensure', and the `state' plist), and a keyword providing
information about the context in which the installation is
happening. (This defaults to `:unknown' but can be overridden by
providing CONTEXT.)

Return t if the package is installed, nil otherwise. (This is
determined by the return value of `use-package-ensure-function'.)
If the package is installed, its entry is removed from
`use-package--deferred-packages'. If the package has no entry in
`use-package--deferred-packages', do nothing and return t.")

  (el-patch-defvar use-package--deferred-packages (make-hash-table)
		   "Hash mapping packages to data about their installation.

The keys are not actually symbols naming packages, but rather
symbols naming the features which are the names of \"packages\"
required by `use-package' forms. Since
`use-package-ensure-function' could be set to anything, it is
actually impossible for `use-package' to determine what package
is supposed to provide the feature being ensured just based on
the value of `:ensure'.

Each value is a cons, with the car being the the value passed to
`:ensure' and the cdr being the `state' plist. See
`use-package-install-deferred-package' for information about how
these values are used to call `use-package-ensure-function'."))

(use-package hydra
             :ensure t
  :init
  (autoload 'hydra-default-pre "hydra"))

(use-package s
             :ensure t)
(use-package f
             :ensure t)
(use-package restart-emacs
             :ensure t)

(use-package exec-path-from-shell
             :ensure t
:config
(setq exec-path-from-shell-check-startup-files nil)

:init
(eval-when-compile
  (with-demoted-errors "Load error: %s"
    (require 'exec-path-from-shell)))

(when (memq window-system '(mac ns))
  (setq exec-path
        (or (eval-when-compile
              (require 'cl-lib)
              (exec-path-from-shell-initialize)
              (cl-remove-duplicates exec-path :test #'string=))
            exec-path))))

(defvar idle-jobs nil
"Symbols which need to be autoloaded.")

(defvar idle-job-timer (run-with-idle-timer 0.1 t 'idle-job-run-next))

(defun idle-job-run-next ()
  "Load symbols from `idle-require-symbols' until input occurs."
  (while (and idle-jobs
              (not (input-pending-p)))
    (cl-letf* ((old-load (symbol-function #'load))
               ((symbol-function #'load)
                (lambda (file &optional noerror _nomessage &rest args)
                  (apply old-load
                         file
                         noerror
                         (not (eq debug-on-error 'startup))
                         args))))
      (with-demoted-errors "Idle job error: %s"
        (funcall (pop idle-jobs))))))

(defun idle-job-add-require (sym &optional append)
  (cl-letf ((fun (lambda ()
                   (let ((start-time (current-time))
                         (verbose mazd//flag-debug-init))
                     (unless (require sym nil t)
                       (message "failed to load %s" sym))

                     (when verbose
                       (message "%.3f loaded %s"
                                (float-time (time-subtract
                                             (current-time)
                                             start-time))
                                sym))))))
    (if append
        (setq idle-jobs (append idle-jobs (list fun)))
      (push fun idle-jobs))))

(defun idle-job-add-function (sym &optional append)
  (cl-letf ((fun (lambda ()
                   (let ((start-time (current-time))
                         (verbose mazd//flag-debug-init))
                     (funcall sym)
                     (when verbose
                       (message "%.3f ran %S"
                                (float-time (time-subtract
                                             (current-time)
                                             start-time))
                                sym))))))
    (if append
        (setq idle-jobs (append idle-jobs (list fun)))
      (push fun idle-jobs))))

(idle-job-add-require 'magit)

(defmacro mazd//load-magit-submodule (sym)
  `(idle-job-add-function
    (mazd//defun-as-value
     ,(intern (format "mazd//lazy-load-%s" (symbol-name (cadr sym)))) ()
     (cl-letf* ((old-require (symbol-function #'require))
                ((symbol-function #'require)
                 (lambda (feature &optional filename noerror)
                   (unless (eq feature 'magit)
                     (funcall old-require
                              feature
                              filename
                              noerror)))))
       (unless (require ,sym nil t)
         (message "failed to load %s" ,sym))))))

(mazd//load-magit-submodule 'magit-bookmark)
(mazd//load-magit-submodule 'magit-submodule)
(mazd//load-magit-submodule 'magit-blame)
(mazd//load-magit-submodule 'magit-stash)
(mazd//load-magit-submodule 'magit-bisect)
(mazd//load-magit-submodule 'magit-push)
(mazd//load-magit-submodule 'magit-pull)
(mazd//load-magit-submodule 'magit-fetch)
(mazd//load-magit-submodule 'magit-clone)
(mazd//load-magit-submodule 'magit-remote)
(mazd//load-magit-submodule 'magit-commit)
(mazd//load-magit-submodule 'magit-sequence)
(mazd//load-magit-submodule 'magit-notes)
(mazd//load-magit-submodule 'magit-worktree)
(mazd//load-magit-submodule 'magit-tag)
(mazd//load-magit-submodule 'magit-merge)
(mazd//load-magit-submodule 'magit-reset)
(mazd//load-magit-submodule 'magit-files)
(mazd//load-magit-submodule 'magit-refs)
(mazd//load-magit-submodule 'magit-status)

(idle-job-add-require 'package)
(idle-job-add-require 'magit-repos)
(idle-job-add-require 'magit-apply)
(idle-job-add-require 'magit-wip)
(idle-job-add-require 'magit-log)
(idle-job-add-require 'magit-diff)
(idle-job-add-require 'smerge-mode)
(idle-job-add-require 'magit-core)
(idle-job-add-require 'magit-autorevert)
(idle-job-add-require 'magit-margin)
(idle-job-add-require 'magit-branch)
(idle-job-add-require 'magit-mode)
(idle-job-add-require 'magit-base)
(idle-job-add-require 'yasnippet)

(idle-job-add-require 'avy)

(idle-job-add-require 'expand-region)
(idle-job-add-require 'er-basic-expansions)
(idle-job-add-require 'expand-region-core)
(idle-job-add-require 'expand-region-custom)
(idle-job-add-require 'dired)
(idle-job-add-require 'dired-loaddefs)
(idle-job-add-require 'recentf)
(idle-job-add-require 'url)
(idle-job-add-require 'url-privacy)
(idle-job-add-require 'url-expand)
(idle-job-add-require 'url-history)
(idle-job-add-require 'mailcap)


(provide 'mazd-package)
;;; mazd//package.el ends here
