;;; mazd//core.el --- Core  -*- lexical-binding: t; -*-

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

(eval-when-compile
  (require 'mazd-vars))

;; Initialize package.el
(require 'package)

(setq use-package-expand-minimally t
      ;; use-package is a macro. Don't let the macro expands into
      ;; codes with too much of irrelevant checks.
      ;; Straight is my package manager, don't let package.el get
      ;; involved.
      ;; use-package-always-defer t
      ;; This is a useful trick to speed up your startup time. Only
      ;; use `require' when it's necessary. By setting the
      ;; `use-package-always-defer' option to t, use-package won't
      ;; call `require' in all cases unless you explicitly include
      ;; :demand t'. This will prevent unnecessary package loading and
      ;; speed up your Emacs startup time.
      straight-check-for-modifications nil ;;'(find-at-startup)
      ;; This is a useful trick to further optimize your startup
      ;; time. Instead of using `straight-check-for-modifications' to
      ;; check if a package has been modified, you can manually
      ;; rebuild the package by `straight-rebuild-package' when you
      ;; know its source code has changed. This avoids the overhead of
      ;; the check. Make sure you know what you are doing here when
      ;; setting this option.
      debug-on-error nil)

(setq package--init-file-ensured t
      ad-redefinition-action 'accept
      cursor-type 'box
      create-lockfiles nil
      frame-title-format "ε %b [%m]"
      jit-lock-defer-time 0.04
      jit-lock-stealth-nice 0.1
      jit-lock-stealth-time 0.2
      jit-lock-stealth-verbose nil
      jka-compr-use-shell nil
      left-margin-width 0
      max-lisp-eval-depth 5000
      max-specpdl-size 10000
      mode-line-end-spaces nil
      mazd//flag-debug-init (eq debug-on-error 'startup)
      process-adaptive-read-buffering nil
      right-margin-width 0
      ring-bell-function 'ignore
      initial-scratch-message ""
      frame-inhibit-implied-resize t
      initial-major-mode 'fundamental-mode
      select-enable-clipboard t
      user-full-name "M.R. Siavash Katebzadeh"
      user-mail-address "mr.katebzadeh@gmail.com"
      package-user-dir (expand-file-name "elpa" mazd//packages-dir)
      package-gnupghome-dir (expand-file-name "gpg" mazd//packages-dir)
      help-window-select t
      x-stretch-cursor t
      package-archives
      `(("gnu"          . "https://elpa.gnu.org/packages/")
	("melpa"        . "https://melpa.org/packages/")
	)
      package-archive-priorities
      '(("melpa" . -1)
	("gnu" . -3))
      )

;;; Basic configs
(setq warning-minimum-level :emergency)
(setq eshell-directory-name mazd//eshell-dir)
(setq pcache-directory (concat mazd//cache-dir "/var/pcache"))
(setq transient-history-file (concat mazd//cache-dir "/transient/history.el"))
(setq srecode-map-save-file (concat mazd//cache-dir "/srecode-map.el"))
(setq projectile-cache-file (concat mazd//cache-dir "/projectile.cache"))
					; stop creating backup~ files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)

(let ((backup-dir mazd//backup-dir)
      (auto-saves-dir mazd//autosave-dir))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))
(setq backup-by-copying t
      delete-old-versions t
      version-control t
      kept-new-versions 5
      kept-old-versions 2)
(setq custom-file (concat mazd//backup-dir "custom.el"))
(load custom-file 'noerror)

(add-to-list 'load-path mazd//lisp-dir)

(defun byte-recompile-config (&optional arg)
  (interactive "p")
  "Recompile this Emacs configuration.
If passed a non-nil or called interactively with a C-u, also recompile
files with (apparently) up to date bytecodes."
  (let* ((force (if (called-interactively-p 'any)
                    (and (integerp arg) (= arg 4))
                  arg))
         (init-el-error
          (let ((init-elc (locate-user-emacs-file "init.elc"))
                (init-el (locate-user-emacs-file "init.el")))
            (when force (delete-file init-elc))
            (when (or (not (file-exists-p init-elc))
                      (time-less-p (file-attribute-modification-time
                                    (file-attributes init-elc))
                                   (file-attribute-modification-time
                                    (file-attributes init-el))))
              (not (byte-compile-file (locate-user-emacs-file "init.el"))))))
         (modules-result (byte-recompile-directory
                          (locate-user-emacs-file "modules/")
                          0
                          force))
         (modules-error (when (string-match-p
                               (rx "failed")
                               modules-result)
                          modules-result)))
    (or init-el-error modules-error)))

(defun emergency-fix-config ()
  "Non-destructively reset the config to whatever git is tracking."
  (interactive)
  (when (fboundp 'mazd//package-rebuild-autoloads)
    (mazd//package-rebuild-autoloads))
  (let ((default-directory user-emacs-directory)
        (module-dir (locate-user-emacs-file "modules")))
    (shell-command "git stash")
    (shell-command "git clean -ffXd :/")
    (shell-command "git pull --rebase -X histogram")
    (with-demoted-errors "Emergency fix delete error: %s"
      (mapc (lambda (file) (delete-file file t))
            (append
             (list (locate-user-emacs-file "init.elc"))
             (file-expand-wildcards (concat module-dir "/*.elc"))))
      (delete-directory (locate-user-emacs-file "elpa") t t))
    (byte-recompile-config)
    (package-initialize)
    (mazd//ensure-packages-are-installed (bound-and-true-p mzd//required-packages))
    (restart-emacs)))

(defun really-kill-emacs ()
  "Like `kill-emacs', but ignores `kill-emacs-hook'."
  (interactive)
  (let (kill-emacs-hook)
    (kill-emacs)))

(defun brutally-kill-emacs ()
  "Use `call-process' to send ourselves a KILL signal."
  (interactive)
  (call-process "kill" nil nil nil "-9" (number-to-string (emacs-pid))))



(package-initialize)

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
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)

(straight-use-package 'use-package)

;(setq use-package-compute-statistics t)
(require 'auth-source)
(setq auth-sources '("~/.netrc"))
(defun mazd//lookup-password (&rest keys)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

;; gcmh
(use-package gcmh
  :ensure t
  :init
  (setq gcmh-verbose             t
        gcmh-lows-cons-threshold #x800000
        gcmh-high-cons-threshold #x800000
        gcmh-idle-delay          300)
  :config
  (gcmh-mode))

;; esup
(use-package esup
  :defer t
  :ensure t
  :defer t)

(provide 'mazd-core)
;;; mazd//core.el ends here
