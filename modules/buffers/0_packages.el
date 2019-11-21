;;; packages.el --- Buffers -*- lexical-binding: t; -*-

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

;;; avy
(use-package avy
  :ensure t)

;;; undo-tree
(use-package undo-tree
  :ensure t)

;;; smart-hungry-delete
(use-package smart-hungry-delete
  :ensure t
  :defer nil)

;;; expand-region
(use-package expand-region
  :ensure t)

;;; paren-face
(use-package paren-face
  :ensure t)

;;; evil-surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;;; projectile
(use-package projectile
  :ensure t)

;;; nlinum-relative
(use-package nlinum-relative
  :ensure t
  :init
  (nlinum-relative-setup-evil))

;;; emojify
(use-package emojify
  :ensure t)

;;; google-this
(use-package google-this
  :ensure t)

;;; FIXMEe
(use-package fixmee
  :ensure t
  :after button-lock
  :init
  (global-fixmee-mode 1))

;;; aggressive-indent
(use-package aggressive-indent
  :ensure t
  :hook ((css-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (js-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode)))

;;; electric-operator
(use-package electric-operator
  :ensure t
  :delight
  :hook (python-mode . electric-operator-mode))

;;; rainbow-mode
(use-package rainbow-mode
  :ensure t
  :delight
  :hook (prog-mode))

;;; simple
(use-package simple
  :ensure nil
  :hook (before-save . delete-trailing-whitespace))

;;; recentf
(use-package recentf
  :ensure t
  :defer 8
  :init (recentf-mode)
  :custom
  (recentf-exclude (list "COMMIT_EDITMSG"
                         "~$"
                         "/scp:"
                         (expand-file-name mk-backup-dir)
                         (expand-file-name mk-local-dir)
                         (expand-file-name (concat mk-emacs-dir "emms/"))
                         "/ssh:"
                         "/sudo:"
                         "/tmp/"))
  (recentf-max-menu-items 15)
  (recentf-max-saved-items 200)
  (recentf-save-file (concat mk-backup-dir "recentf"))
  :config (run-at-time nil (* 5 60) 'recentf-save-list))

;;; docker-tramp
(use-package docker-tramp
  :ensure t
  :defer 6)

;;; format-all-buffer
(use-package format-all
  :ensure t
  :defer 6)

;;; packages.el ends here
