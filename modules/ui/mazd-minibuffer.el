;;; mazd-minibuffer.el --- Minibuffer -*- lexical-binding: t; -*-

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
(use-package vertico
  :ensure t
  :config
  (general-define-key
   :keymaps 'vertico-map
   "<left>" #'vertico-directory-up
   "DEL"    #'vertico-directory-delete-char)
  :custom
  (vertico-count 10)
  :init (vertico-mode)
  )

(use-package vertico-posframe
  :disabled t
  :ensure t
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-parameters
	'((left-fringe . 8)
          (right-fringe . 8))
	)
  (setq vertico-posframe-border-width 3)
  (setq vertico-posframe-border '((t (:background "#323445"))))
  )

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless flex))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles orderless flex)))))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))

  :init

  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
	  (which-key--hide-popup-ignore-command)
	(which-key--show-keymap
	 (if (eq (plist-get (car targets) :type) 'embark-become)
	     "Become"
	   (format "Act on %s '%s'%s"
		   (plist-get (car targets) :type)
		   (embark--truncate-target (plist-get (car targets) :target))
		   (if (cdr targets) "…" "")))
	 (if prefix
	     (pcase (lookup-key keymap prefix 'accept-default)
	       ((and (pred keymapp) km) km)
	       (_ (key-binding prefix 'accept-default)))
	   keymap)
	 nil nil t (lambda (binding)
		     (not (string-suffix-p "-argument" (cdr binding))))))))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
	   (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
	      :around #'embark-hide-which-key-indicator)
  (setq embark-indicators (delq #'embark-mixed-indicator embark-indicators))
  (push #'embark-which-key-indicator embark-indicators)
  )

(use-package embark-consult
  :defer t
  :ensure nil
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :ensure t
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  )

(use-package vertico-multiform
  :disabled t
  :defer t
  :ensure nil
  :hook (after-init . vertico-multiform-mode)
  :init
  (setq vertico-multiform-commands
	'()))


(use-package mini-frame
  :disabled t
  :ensure t
  :config
  (setq mini-frame-show-parameters
        `((left . 1.5)
          (top . 1.0)
          (right . 1.5)
          (width . 1.0)
          (height . 12)
          (left-fringe . 15)
          (right-fringe . 15)
          (child-frame-border-width . 1)
          (internal-border-width . 0)
	  (foreground-color . "#cee2cd")
          (background-color . "#5c6d7a")
	  ))


  (setq mini-frame-ignore-commands
        '("edebug-eval-expression" debugger-eval-expression))

  (setq mini-frame-internal-border-color "#7c9dab")
  ;; (setq mini-frame-resize 'not-set)
  (setq mini-frame-resize nil)
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (overlay-put (make-overlay (point-min) (+ (point-min) 1))
                           'before-string
                           (propertize "\n" 'face `(:extend t
                                                            :height 0.5)))))
  (mini-frame-mode 1)
  )

(use-package nerd-icons-completion
  :config
  :after marginalia
  :ensure (:host github :repo "rainstormstudio/nerd-icons-completion")
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))

(provide 'mazd-minibuffer)
;;; mazd-minibuffer.el ends here
