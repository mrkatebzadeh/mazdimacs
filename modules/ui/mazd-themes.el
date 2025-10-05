;;; mazd//themes.el --- Theme -*- lexical-binding: t; -*-

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

(use-package anisochromatic
  :vc (:url "https://github.com/isomatter-labs/anisochromatic-emacs")
  :disabled t
  :ensure (:host github :repo "isomatter-labs/anisochromatic-emacs")
  :config
  ;; Disable any other active themes
  ;; (mapc #'disable-theme custom-enabled-themes)
  ;; Load anisochromatic as default
  ;; (load-theme 'anisochromatic t)
  )

(use-package catppuccin-theme
  :ensure t
  ;; :disabled t
  :config
  (setq catppuccin-flavor 'frappe)
  (load-theme 'catppuccin :no-confirm)
  (setq window-divider-default-right-width 1)
  (setq window-divider-default-places 'right-only)
  (window-divider-mode 1)
  (set-face-foreground 'window-divider "black")
  (set-face-background 'window-divider "black")
  (set-face-foreground 'window-divider-first-pixel "black")
  (set-face-background 'window-divider-first-pixel "black")
  (set-face-foreground 'window-divider-last-pixel "black")
  (set-face-background 'window-divider-last-pixel "black")
  )

(provide 'mazd-themes)
;;; mazd//themes.el ends here
