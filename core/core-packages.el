;;; core-packages.el --- init file -*- lexical-binding: t; -*-

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
;; Core packages will be installed from here

;;; gcmh
(use-package gcmh
  :ensure t
  :init
  (setq gcmh-verbose             t
        gcmh-lows-cons-threshold #x800000
        gcmh-high-cons-threshold #x800000
        gcmh-idle-delay          300)
  :config
  (gcmh-mode))

;;; outshine
(use-package outshine
  ;; Easier navigation for source files, especially this one.
  :bind (:map outshine-mode-map
	      ("<S-iso-lefttab>" . outshine-cycle-buffer))
  :hook (emacs-lisp-mode . outshine-mode)
  :config
  (advice-add 'outshine-narrow-to-subtree :before
	      (lambda (&rest args) (unless (outline-on-heading-p t)
				     (outline-previous-visible-heading 1))))
  ;;; outshine
  (let ((kmap outline-minor-mode-map))
    (define-key kmap (kbd "M-RET") 'outshine-insert-heading)
    (define-key kmap (kbd "<backtab>") 'outshine-cycle-buffer)))

  ;;; hydra
(use-package hydra
  :ensure t)

;;; general
(use-package general
  :ensure t
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

;;; which-key
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;;; evil
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

;;; evil-collection
(use-package evil-collection
  :ensure t
  :init
  (evil-collection-init))

;;; evil-magit
(use-package evil-magit
  :ensure t)

;;; restart-emacs
(use-package restart-emacs
  :ensure t)

;;; esup
(use-package esup
  :ensure t)

;;; update all packages
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
	auto-package-update-interval 7)
  (auto-package-update-maybe))

(require 'core-bindings)

(provide 'core-packages)
;;; core-packages.el ends here
