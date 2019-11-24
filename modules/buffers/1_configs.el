;;; config.el --- Buffers -*- lexical-binding: t; -*-

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

;;; ibuffer interactive mode
;; set it as default
(defalias 'list-buffers 'ibuffer-other-window)

(defvar *protected-buffers* '("*scratch*" "*Messages*")
  "Buffers that cannot be killed.")

(defun mk-protected-buffers ()
  "Protects some buffers from being killed."
  (dolist (buffer *protected-buffers*)
    (with-current-buffer buffer
      (emacs-lock-mode 'kill))))

(add-hook 'after-init-hook #'mk-protected-buffers)
;; better look
(setq ibuffer-saved-filter-groups
  (quote (("default"
    ("dired" (mode . dired-mode))
    ("org" (name . "^.*org$"))
    ("web" (or (mode . web-mode) (mode . js2-mode)))
    ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
    ("mu4e" (name . "\*mu4e\*"))
    ("IRC" (mode . erc-mode))
    ("programming" (or
      (mode . python-mode)
      (mode . c++-mode)))
    ("emacs" (or
    (name . "^\\*scratch\\*$")
    (name . "^\\*Messages\\*$")))
    ))))
(add-hook 'ibuffer-mode-hook
   (lambda ()
   (ibuffer-auto-mode 1)
   (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-expert t)

;;; avy
(avy-setup-default)

;;; undo-tree
(with-eval-after-load 'undo-tree
  (global-undo-tree-mode))

;;; smart-hungry-delete
(with-eval-after-load 'smart-hungry-delete
  (smart-hungry-delete-add-default-hooks))

;;; y-n-p
(fset 'yes-or-no-p 'y-or-n-p)

;;; projectile
(with-eval-after-load 'projectile
  (setq projectile-globally-ignored-directories
        '(".bzr"
          ".ensime_cache"
          ".eunit"
          ".fslckout"
          ".git"
          ".hg"
          ".idea"
          ".stack-work"
          ".svn"
          ".tox"
          ".clangd"
          ".ccls-cache"
          "READONLY"
          "_FOSSIL_"
          "_darcs"
          "blaze-bin"
          "blaze-genfiles"
          "blaze-google3"
          "blaze-out"
          "blaze-testlogs"
          "node_modules"
          "third_party"
	  "backup"
          "vendor"))
  (setq projectile-completion-system 'helm
	projectile-enable-caching t
	projectile-switch-project-action 'helm-projectile-find-file)
  (projectile-global-mode))

;;; nlinum-relative
(with-eval-after-load 'nlinum-relative
  (setq nlinum-relative-redisplay-delay 0
	nlinum-relative-current-symbol ""
	nlinum-relative-offset 1))

;;; emojify
(with-eval-after-load 'emojify
  (add-hook 'after-init-hook #'global-emojify-mode))

;;; defaults
;; browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;;kill buffer
(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; man
(require 'man)
(set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
(set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)

;;; configs.el ends here
