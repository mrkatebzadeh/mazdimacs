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

  (defun mazd//evil-word-syntax-setup ()
    "Treat underscores as part of a word in Evil mode."
    (modify-syntax-entry ?- "w")
    (modify-syntax-entry ?_ "w"))

  (add-hook 'after-change-major-mode-hook #'mazd//evil-word-syntax-setup)
  ;; highlight current line
  (global-hl-line-mode +1)
  (setq jit-lock-defer-time 0)
  (setq font-lock-maximum-decoration t)

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

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package spacious-padding
  :disabled t
  :ensure t
  :defer t
  :hook
  (after-init .  spacious-padding-mode)
  :init
  (setq spacious-padding-subtle-mode-line nil)
  (setq spacious-padding-widths
	'( :internal-border-width 10
           :header-line-width 4
           :mode-line-width 5
           :tab-line-width 4
           :right-divider-width 10
           :scroll-bar-width 2
           :fringe-width 2))
  )

(use-package page-break-lines
  :defer t
  :ensure t
  :hook (prog-mode . page-break-lines-mode))

(use-package nerd-icons
  :defer t
  :ensure t
  )

(use-package svg-lib
  :defer t
  :vc(:url "https://github.com/rougier/svg-lib")
  :ensure nil
  )

;; Replace keywords with SVG tags
(use-package svg-tag-mode
  :defer t
  :vc(:url "https://github.com/rougier/svg-tag-mode")
  :ensure nil
  )

(defun mazd//increase-font-size ()
  "Increase font size by 1."
  (interactive)
  (setq mazd//font-size (+ mazd//font-size 10))
  (mazd//apply-font-properties))

(defun mazd//decrease-font-size ()
  "Decrease font size by 1."
  (interactive)
  (setq mazd//font-size (- mazd//font-size 10))
  (mazd//apply-font-properties))

(defun mazd//reset-font ()
  "Reset font size and font family to default values."
  (interactive)
  (setq mazd//font-family mazd//font-default-family)
  (setq mazd//font-size mazd//font-default-size)
  (mazd//apply-font-properties)
  (mazd//log "Font reset to %s with size %d" mazd//font-default-family mazd//font-default-size))

(defun mazd//choose-font ()
  "Prompt user to select a font from available system fonts and apply it."
  (interactive)
  (let ((font (completing-read "Choose font: " (font-family-list))))
    (setq mazd//font-family font)
    (mazd//apply-font-properties)
    (mazd//log "Font set to: %s" font)))

(defun mazd//apply-font-properties ()
  "Apply the current values of font properties."
  (set-face-attribute 'default nil
                      :family mazd//font-family
                      :height mazd//font-size
                      :weight 'normal
                      :width 'normal))

(defun mazd//toggle-transparency ()
  "Toggle background transparency between `mazd//alpha-variable` and 100."
  (interactive)
  (if (and (not (eq (frame-parameter nil 'alpha) 100))
	   (not (eq (frame-parameter nil 'alpha) nil)))
      (set-frame-parameter (selected-frame) 'alpha 100)
    (set-frame-parameter (selected-frame) 'alpha
			 mazd//alpha-variable)))

(mazd//apply-font-properties)

(leader
  "u" '(:ignore t :which-key "UI")
  "uf" '(:ignore t :which-key "Font")
  "ut" 'mazd//toggle-transparency
  "uT" 'consult-theme
  "ufi" 'mazd//increase-font-size
  "ufd" 'mazd//decrease-font-size
  "ufr" 'mazd//reset-font
  "uff" 'mazd//choose-font
  )

(provide 'mazd-ui)
;;; mazd//ui.el ends here
