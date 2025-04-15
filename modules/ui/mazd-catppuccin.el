;;; mazd-catppuccin.el --- Catppuccin -*- lexical-binding: t; -*-

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

(use-package catppuccin-theme
  :ensure t
  :config
  (dolist (theme '(catppuccin-latte catppuccin-frappe catppuccin-macchiato catppuccin-mocha))
    (add-to-list 'custom-known-themes theme))
  )

(defun mazd//refresh-theme ()
  "Function to load catppuccin theme"
  (interactive)
  (load-theme 'catppuccin :no-confirm)
  )
(call-interactively 'mazd//refresh-theme)

(defvar doom-flavor '((rosewater . "#d4c4b4")
		      (flamingo . "#6b757f")
		      (pink . "#7c9f85")
		      (mauve . "#c4865e")
		      (red . "#c97f79")
		      (maroon . "#9b7575")
		      (peach . "#c5a47b")
		      (yellow . "#c3c0a2")
		      (green . "#359b75")
		      (teal . "#5e8a83")
		      (sky . "#5c6d7a")
		      (sapphire . "#51797a")
		      (blue . "#009f99")
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
		      (mantle . "#23282b")
		      (crust . "#23282b")))

(catppuccin-set-color 'base "#303446" 'frappe)
(catppuccin-set-color 'mantle "#303446" 'frappe)
(catppuccin-set-color 'crust "#303446" 'frappe)


(catppuccin-set-color 'base "#1e1e2e" 'mocha)
(catppuccin-set-color 'mantle "#1e1e2e" 'mocha)
(catppuccin-set-color 'crust "#1e1e2e" 'mocha)


(catppuccin-set-color 'base "#24273a" 'macchiato)
(catppuccin-set-color 'mantle "#24273a" 'macchiato)
(catppuccin-set-color 'crust "#24273a" 'macchiato)

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

(defgroup mazd//code-faces nil
  "Faces for highlighting code.")

;;;###autoload
(defface mazd//font-lock-constructor-face
  '((default :inherit font-lock-type-face :slant oblique))
  "Face for creating new instances."
  :group 'mazd//code-faces)

;;;###autoload
(defface mazd//font-lock-global-var-face
  '((default :inherit font-lock-constant-face :weight bold))
  "Face for constants."
  :group 'mazd//code-faces)

;;;###autoload
(defface mazd//font-lock-function-call-face
  '((default :foreground "#000080" :weight semi-bold))
  "Face for function call"
  :group 'mazd//code-faces)

;;;###autoload
(defface mazd//font-lock-method-call-face
  '((default :inherit mazd//font-lock-function-call-face :weight semi-bold))
  "Face for method call"
  :group 'mazd//code-faces)

;;;###autoload
(defface mazd//font-lock-command-face
  '((default :foreground "#000080"))
  "Face for method call"
  :group 'mazd//code-faces)

;;;###autoload
(defface mazd//font-lock-special-var-face
  '((default :inherit font-lock-variable-name-face :weight bold))
  "Face for special var"
  :group 'mazd//code-faces)

;;;###autoload
(defface mazd//font-lock-argument-face
  '((default :foreground "#8b7765"))
  "Face for argument"
  :group 'mazd//code-faces)

;;;###autoload
(defface mazd//font-lock-argument-keyword-face
  '((default :foreground "#8b7765"))
  "Face for keyword"
  :group 'mazd//code-faces)

;;;###autoload
(defface mazd//font-lock-parameter-face
  ;; #8b7765
  '((default :foreground "#8b7765"))
  "Face for parameter"
  :group 'mazd//code-faces)


;;;###autoload
(defun mazd//setup-code-faces (selected-flavor)
  (set-face-attribute 'font-lock-property-use-face nil :slant 'oblique)
  (set-face-attribute 'font-lock-misc-punctuation-face nil :foreground "#b03060" :weight 'semi-bold)

  (set-face-attribute 'mazd//font-lock-constructor-face nil
                      :foreground (catppuccin-get-color 'blue (intern selected-flavor))
                      :slant 'oblique)

  (set-face-attribute 'mazd//font-lock-global-var-face nil
                      :foreground (catppuccin-get-color 'teal (intern selected-flavor))
                      :weight 'bold)

  (set-face-attribute 'mazd//font-lock-function-call-face nil
                      :foreground (catppuccin-get-color 'lavender (intern selected-flavor))
                      :weight 'semi-bold)

  (set-face-attribute 'mazd//font-lock-method-call-face nil
                      :foreground (catppuccin-get-color 'lavender (intern selected-flavor))
                      :weight 'semi-bold)

  (set-face-attribute 'mazd//font-lock-command-face nil
                      :foreground (catppuccin-get-color 'blue (intern selected-flavor)))

  (set-face-attribute 'mazd//font-lock-special-var-face nil
                      :foreground (catppuccin-get-color 'mauve (intern selected-flavor))
                      :weight 'bold)

  (set-face-attribute 'mazd//font-lock-argument-face nil
                      :foreground (catppuccin-get-color 'rosewater (intern selected-flavor)))

  (set-face-attribute 'mazd//font-lock-argument-keyword-face nil
                      :foreground (catppuccin-get-color 'rosewater (intern selected-flavor)))

  (set-face-attribute 'mazd//font-lock-parameter-face nil
                      :foreground (catppuccin-get-color 'rosewater (intern selected-flavor)))
  )



(defun mazd//apply-theme ()
  "Apply the current theme stored in `mazd//theme`."
  (setq catppuccin-flavor (intern mazd//theme))

  (when (bound-and-true-p telephone-line-mode)
    (update-telephone-line-theme mazd//theme))

  (mazd//setup-code-faces mazd//theme)
  (mazd//refresh-theme)
  (message "Applied Catppuccin flavor: %s" mazd//theme))

(defun mazd//list-catppuccin-themes ()
  "List available Catppuccin flavors and apply the selected one."
  (interactive)
  (let* ((selected-flavor (completing-read "Select a flavor: " (mapcar 'symbol-name catppuccin-flavor-list))))
    (setq mazd//theme selected-flavor)
    (mazd//apply-theme)))

(mazd//apply-theme)

(leader
  "uT" 'consult-themes
  "uc" 'mazd//list-catppuccin-themes)

(provide 'mazd-catppuccin)
;;; mazd-catppuccin.el ends here
