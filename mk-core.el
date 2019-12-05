;;; mk-core.el --- Core  -*- lexical-binding: t; -*-

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

;; Constants
(defconst mk-version "0.0.1"
  "Current version of MK Emacs.")

;; Variables
(defvar mk-local-dir (concat mk-emacs-dir ".local/")
  "Root directory for local storage.
Use this as a storage location for this system's installation of MK Emacs.
These files should not be shared across systems. By default, it is used by
`mk-etc-dir' and `mk-cache-dir'. Must end with a slash.")

(defvar mk-etc-dir (concat mk-local-dir "etc/")
  "Directory for non-volatile local storage.
Use this for files that don't change much, like server binaries, external
dependencies or long-term shared data. Must end with a slash.")

(defvar mk-cache-dir (concat mk-local-dir "cache/")
  "Directory for volatile local storage.
Use this for files that change often, like cache files. Must end with a slash.")

(defvar mk-packages-dir (concat mk-local-dir "packages/")
  "Where package.el and quelpa plugins (and their caches) are stored.
Must end with a slash.")


;; Uncomment the following line if you got “no match” error after `M-x package-install` on a package on MacOS
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Initialize package.el
(require 'package)
(setq package--init-file-ensured t
      initial-scratch-message ""
      tab-width 4
      select-enable-clipboard t
      user-full-name "M.R. Siavash Katebzadeh"
      user-mail-address "mr.katebzadeh@gmail.com"
      package-user-dir (expand-file-name "elpa" mk-packages-dir)
      package-gnupghome-dir (expand-file-name "gpg" mk-packages-dir)
      package-enable-at-startup nil
      help-window-select t
      package-archives
      `(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("org"          . "https://orgmode.org/elpa/"))
      package-archive-priorities
      '(("melpa" . -1)
	("gnu" . -3)))
(package-initialize)
;; Bootstrap `use-package'
(setq-default use-package-always-ensure t ; Auto-download package if not exists
              use-package-always-defer t ; Always defer load package to speed up startup
              use-package-verbose nil ; Don't report loading details
              use-package-expand-minimally t  ; make the expanded code as minimal as possible
              use-package-enable-imenu-support t) ; Let imenu finds use-package definitions
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

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

;; general
(use-package general
  :defer t
  :config
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  (general-override-mode))

;; which-key
(use-package which-key
  :defer t
  :init
  (which-key-mode))

;; evil
(use-package evil
  :ensure t
  :init
  (setq evil-search-module 'evil-search
	evil-ex-complete-emacs-commands nil
	evil-vsplit-window-right t
	evil-split-window-below t
	evil-shift-round nil
	evil-want-C-u-scroll t
	evil-default-cursor t
	evil-want-integration nil
	evil-want-keybinding nil)
  ;; This has to be before we invoke evil-mode due to:
  ;; https://github.com/cofi/evil-leader/issues/10
  (use-package evil-leader
    :init (global-evil-leader-mode))
  (evil-mode 1))

;; evil-collection
(use-package evil-collection
  :ensure t
  :init
  (evil-collection-init))

;; esup
(use-package esup
  :defer t)

;; Esc
(global-set-key [escape] 'keyboard-escape-quit)

;; leader file
(general-define-key
 :prefix "SPC"
 :states '(normal visual motion)
 :keymaps 'override
 "" '(nil :which-key "My lieutenant general prefix")
 "f" '(:ignore t :which-key "Files")
 "C" '(:ignore t :which-key "Config Files")
 "o" '(:ignore t :which-key "Org")
 "a" '(:ignore t :which-key "Applications")
 "g" '(:ignore t :which-key "Magit")
 "m" '(:ignore t :which-key "EMMS")
 "l" '(:ignore t :which-key "Local Bindings")
 "b" '(:ignore t :which-key "Buffers")
 "s" '(:ignore t :which-key "Search")
 "S" '(:ignore t :which-key "Shell")
 "h" '(:ignore t :which-key "Help!")
 "v" '(:ignore t :which-key "Volume")
 "w" '(:ignore t :which-key "Windows")
 "q" '(:ignore t :which-key "Quit")
 "t" '(:ignore t :which-key "Toggles")

 "x" 'helm-M-x
 ";" 'eval-expression
 )

;; Exit/restart/reboot/shutdown
(general-define-key
 :prefix "SPC q"
 :states '(normal visual motion)
 :keymaps 'override
 "q" 'kill-emacs
 "Q" 'delete-frame)

(provide 'mk-core)
;;; mk-core.el ends here
