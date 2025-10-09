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

(eval-when-compile (require 'cl-lib))

(setq use-package-expand-minimally t)
(setq enable-local-variables :all)


(setq
 ;; package--init-file-ensured t
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
 initial-scratch-message t
 load-prefer-newer t
 frame-inhibit-implied-resize t
 initial-major-mode 'fundamental-mode
 select-enable-clipboard t
 user-full-name "M.R. Siavash Katebzadeh"
 user-mail-address "mr.katebzadeh@gmail.com"
 package-user-dir (expand-file-name "elpa" mazd//packages-dir)
 package-gnupghome-dir (expand-file-name "gpg" mazd//packages-dir)
 help-window-select t
 x-stretch-cursor t
 )

(setq auto-mode-case-fold nil)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq inhibit-compacting-font-caches t)
(setq read-process-output-max (* 64 1024))
(setq redisplay-skip-fontification-on-input t)

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

(setq warning-minimum-level :emergency)
(setq eshell-directory-name mazd//eshell-dir)
(setq pcache-directory (concat mazd//cache-dir "/var/pcache"))
(setq transient-history-file (concat mazd//cache-dir "/transient/history.el"))
(setq srecode-map-save-file (concat mazd//cache-dir "/srecode-map.el"))
(setq projectile-cache-file (concat mazd//cache-dir "/projectile.cache"))
(setq confirm-kill-emacs 'y-or-n-p)
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

(make-directory (concat mazd//cache-dir "/undo-tree") t)
(setq undo-tree-history-directory-alist
      `(("." . ,(concat mazd//cache-dir "/undo-tree"))))

(add-to-list 'load-path mazd//lisp-dir)
(require 'auth-source)
(setq auth-sources '("~/.netrc"))
(defun mazd//lookup-password (&rest keys)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(when (file-exists-p mazd//autoload-file)
  (load mazd//autoload-file nil t))
;; ;; esup
;; (use-package esup
;;   :defer t
;;   :ensure t
;;   :defer t)

(provide 'mazd-core)
;;; mazd//core.el ends here
