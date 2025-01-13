;;; mazd//window.el --- Window  -*- lexical-binding: t; -*-

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

(use-package ace-window
  :ensure t
  :defer t
  :init
  (global-set-key [remap other-window] 'ace-window))

(use-package imenu
  :ensure t
  :defer t)

(use-package imenu-list
  :ensure t
  :defer t
  :init
  (setq imenu-list-focus-after-activation t
	imenu-list-auto-resize t))

(use-package zoom
  :ensure t
  :defer t
  :init
					;  (zoom-mode t)
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618))))

(use-package darkroom
  :ensure t
  :defer t)

;;; config

(windmove-default-keybindings)
(winner-mode 1)

;;; bindings

(leader
  "wv" 'evil-window-vsplit
  "ws" 'evil-window-split
  "wd" 'evil-window-delete
  "wh" 'evil-window-left
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  "wl" 'evil-window-right)

(leader
  "ti" 'imenu-list
  "tI" 'helm-imenu
  "td" 'darkroom-mode
  "tz" 'zoom-mode)

(provide 'mazd//window)
;;; mazd//window.el ends here
