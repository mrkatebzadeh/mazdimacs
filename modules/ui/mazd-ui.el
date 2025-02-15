;;; mazd//ui.el --- UI -*- lexical-binding: t; -*-

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

(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)

  ;; linenumber
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers 'relative)
  ;; Font
  (set-face-attribute 'default nil
                      :family mazd//font-default-family
                      :height mazd//font-default-size
                      :weight 'normal
                      :width 'normal)
  (defun mazd//evil-word-syntax-setup ()
    "Treat underscores as part of a word in Evil mode."
    (modify-syntax-entry ?_ "w"))

  (add-hook 'after-change-major-mode-hook #'mazd//evil-word-syntax-setup)
  ;; highlight current line
  (global-hl-line-mode +1)
  ;; smooth scroll
  (setq scroll-step 1
	scroll-conservatively 10000
	auto-window-vscroll nil)
  (setq use-file-dialog nil
	use-dialog-box nil
	inhibit-startup-screen t
	inhibit-startup-echo-area-message user-login-name
	inhibit-default-init t
	initial-scratch-message nil)
  (setq default-frame-alist
	(append (list
	         '(min-height . 1)
		 '(height     . 45)
	         '(min-width  . 1)
		 '(width      . 81)
		 '(horizontal-scroll-bars)
		 '(vertical-scroll-bars)
		 '(internal-border-width . 12)
		 '(left-fringe    . 1)
		 '(right-fringe   . 1)
		 '(tool-bar-lines . 0)
		 '(menu-bar-lines . 0))))
  (add-to-list 'default-frame-alist '(undecorated-round . t))

  (defun mazd//text-mode-setup ()
    "Enable visual line mode when editing text files."
    (visual-line-mode t))

  (add-hook 'text-mode-hook 'mazd//text-mode-setup)
  )

(use-package ligature
  :defer t
  :ensure t
  :config
  ;; Enable the www ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))

  ;; Enable ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
				       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
				       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
				       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
				       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
				       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
				       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
				       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
				       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
				       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  :init
  (global-ligature-mode t)
  )



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

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package embark
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

(use-package spacious-padding
  :ensure t
  :defer t
  :hook
  (after-init .  spacious-padding-mode)
  :init
  (setq spacious-padding-subtle-mode-line nil))

;; SVG tags, progress bars & icons
(use-package svg-lib
  :defer t
  :ensure nil)

;; Replace keywords with SVG tags
(use-package svg-tag-mode
  :defer t
  :ensure nil)

(use-package page-break-lines
  :defer t
  :ensure t
  :hook (prog-mode . page-break-lines-mode))

(use-package nerd-icons
  :defer t
  :ensure t
  )

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)

  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-hud nil)
  (setq doom-modeline-window-width-limit 85)

  (doom-modeline-def-segment mazd-incremental-load
    "A segment displaying the incremental package loading progress."
    (if (>= mazd//incremental-load-progress mazd//incremental-load-total)
	(progn
          ;; Schedule removal after 3 seconds
          (run-at-time 3 nil
                       (lambda ()
			 (doom-modeline-def-modeline 'mazd-custom-line
                           '(bar matches buffer-info remote-host buffer-position parrot selection-info)
                           '(misc-info minor-modes input-method buffer-encoding major-mode process vcs check))
			 (doom-modeline-set-modeline 'mazd-custom-line 'default)
			 (force-mode-line-update)))
          (propertize " [✔]" 'face 'success))  ;; Show checkmark when done
      (propertize (format " %s [%d/%d]"
                          (or mazd//current-loading-package "")
                          mazd//incremental-load-progress
                          mazd//incremental-load-total)
                  'face 'warning)))

  (doom-modeline-def-modeline 'mazd-custom-line
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(mazd-incremental-load misc-info minor-modes input-method buffer-encoding major-mode process vcs check))

  (doom-modeline-set-modeline 'mazd-custom-line 'default)
  )

(use-package telephone-line
  :disabled t
  :ensure t
  :init
  (telephone-line-mode t)
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
	telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
	telephone-line-primary-right-separator 'telephone-line-cubed-right
	telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (setq telephone-line-height 18
	telephone-line-evil-use-short-tag nil)
  )


(defun mazd//increase-font-size ()
  "Increase font size by 1."
  (interactive)
  (set-face-attribute 'default nil :height (+ (face-attribute 'default :height) 10)))

(defun mazd//decrease-font-size ()
  "Decrease font size by 1."
  (interactive)
  (set-face-attribute 'default nil :height (- (face-attribute 'default :height) 10)))

(defun mazd//reset-font ()
  "Reset font size and font family to default values."
  (interactive)
  (set-face-attribute 'default nil
                      :family mazd//font-default-family
                      :height mazd//font-default-size
                      :weight 'normal
                      :width 'normal)
  (message "Font reset to %s with size %d" mazd//font-default-family mazd//font-default-size))

(defun mazd//choose-font ()
  "Prompt user to select a font from available system fonts and apply it."
  (interactive)
  (let ((font (completing-read "Choose font: " (font-family-list))))
    (set-frame-font font t t)
    (message "Font set to: %s" font)))

(defun mazd//toggle-transparency ()
  "Toggle background transparency between `mazd//alpha-variable` and 100."
  (interactive)
  (if (and (not (eq (frame-parameter nil 'alpha) 100))
	   (not (eq (frame-parameter nil 'alpha) nil)))
      (set-frame-parameter (selected-frame) 'alpha 100)
    (set-frame-parameter (selected-frame) 'alpha
			 mazd//alpha-variable)))

(leader
  "u" '(:ignore t :which-key "UI")
  "uf" '(:ignore t :which-key "Font")
  "ut" 'mazd//toggle-transparency
  "ufi" 'mazd//increase-font-size
  "ufd" 'mazd//decrease-font-size
  "ufr" 'mazd//reset-font
  "uff" 'mazd//choose-font
  )

(provide 'mazd-ui)
;;; mazd//ui.el ends here
