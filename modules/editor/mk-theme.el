;;; mk-theme.el --- Theme -*- lexical-binding: t; -*-

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


;; catppuccin-theme
(use-package catppuccin-theme
  :defer t
  :ensure t
  )

(defun mk-refresh-theme ()
  "Function to load catppuccin theme"
  (interactive)
  (load-theme 'catppuccin :no-confirm)
  )
(call-interactively 'mk-refresh-theme)

(defvar doom-flavor '((rosewater . "#d4c4b4")
		      (flamingo . "#6b757f")
		      (pink . "#7c9f85")
		      (mauve . "#c4865e")
		      (red . "#c67f79")
		      (maroon . "#9b7575")
		      (peach . "#c5a47b")
		      (yellow . "#c3c0a2")
		      (green . "#359b75")
		      (teal . "#5e8a83")
		      (sky . "#5c6d7a")
		      (sapphire . "#51797a")
		      (blue . "#c69f99")
		      (lavender . "#7c9dab")
		      (text . "#cee2cd")
		      (subtext1 . "#c1c7c6")
		      (subtext0 . "#b1b8b7")
		      (overlay2 . "#777d7e")
		      (overlay1 . "#484e4f")
		      (overlay0 . "#767676")
		      (surface2 . "#3b4242")
		      (surface1 . "#454e4e")
		      (surface0 . "#292f2f")
		      (base . "#23282b")
		      (mantle . "#1d2124")
		      (crust . "#17191b")))

(let ((existing-flavor (assoc 'doom catppuccin-flavor-alist)))
  (if existing-flavor
      (setcdr existing-flavor doom-flavor)
    (add-to-list 'catppuccin-flavor-alist (cons 'doom doom-flavor))))

(let ((flavor #'(lambda (sym) (alist-get sym catppuccin-flavor-alist))))
  (define-catppuccin-flavor 'doom (funcall flavor 'doom)))


(setq catppuccin-flavor-list '(frappe mocha macchiato latte doom))
(defun update-telephone-line-theme (selected-flavor)
  (set-face-attribute 'telephone-line-evil nil
		      :foreground (catppuccin-get-color 'text (intern selected-flavor))
		      :weight 'bold
		      :inherit 'mode-line)

  (set-face-attribute 'telephone-line-evil-insert nil
		      :background (catppuccin-get-color 'green (intern selected-flavor))
		      :inherit 'telephone-line-evil)

  (set-face-attribute 'telephone-line-evil-normal nil
		      :background (catppuccin-get-color 'red (intern selected-flavor))
		      :inherit 'telephone-line-evil)

  (set-face-attribute 'telephone-line-evil-visual nil
		      :background (catppuccin-get-color 'peach (intern selected-flavor))
		      :inherit 'telephone-line-evil)

  (set-face-attribute 'telephone-line-evil-replace nil
		      :background (catppuccin-get-color 'base (intern selected-flavor))
		      :inherit 'telephone-line-evil)

  (set-face-attribute 'telephone-line-evil-motion nil
		      :background (catppuccin-get-color 'blue (intern selected-flavor))
		      :inherit 'telephone-line-evil)

  (set-face-attribute 'telephone-line-evil-operator nil
		      :background (catppuccin-get-color 'mauve (intern selected-flavor))
		      :inherit 'telephone-line-evil)

  (set-face-attribute 'telephone-line-evil-emacs nil
		      :background (catppuccin-get-color 'lavender (intern selected-flavor))
		      :inherit 'telephone-line-evil)

  (set-face-attribute 'telephone-line-evil-god nil
		      :background (catppuccin-get-color 'sky (intern selected-flavor))
		      :inherit 'telephone-line-evil)
  )
(defun mk-list-themes ()
  "List available Catppuccin flavors and apply the selected one."
  (interactive)
  (let* ((selected-flavor (completing-read "Select a flavor: " (mapcar 'symbol-name catppuccin-flavor-list))))
    (setq catppuccin-flavor (intern selected-flavor))
    (update-telephone-line-theme selected-flavor)
    (mk-refresh-theme)
    (message "Applied Catppuccin flavor: %s" selected-flavor)))

(setq catppuccin-flavor 'doom)
(update-telephone-line-theme "doom")

(mk-refresh-theme)
(leader
  "tT" 'mk-list-themes)


(provide 'mk-theme)
;;; mk-theme.el ends here
