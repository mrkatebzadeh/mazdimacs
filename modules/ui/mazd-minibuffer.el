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
  :defer t
  :ensure t
  :general
  (:keymaps 'vertico-map
	    "<left>" #'vertico-directory-delete-char
	    "DEL" #'vertico-directory-delete-char)
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 10) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

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

;; `orderless' completion style.
(use-package orderless
  :defer t
  :after vertico
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package embark
  ;; :disabled t
  :ensure t
  :defer t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
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

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  ;; :disabled t
  :defer t
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :defer t
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  :init

  (marginalia-mode))

(use-package vertico-multiform
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

(provide 'mazd-minibuffer)
;;; mazd-minibuffer.el ends here
