;;; mazd//buffer.el --- Buffer  -*- lexical-binding: t; -*-

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

;; solaire-mode is an aesthetic plugin designed to visually distinguish "real" buffers
;; (i.e. file-visiting code buffers where you do most of your work) from "unreal" buffers
;;(like popups, sidebars, log buffers, terminals, etc) by giving the latter a slightly
;; different -- often darker -- background

(use-package ultra-scroll
  :vc ( :url "https://github.com/jdtsmith/ultra-scroll.git")
  :ensure nil
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(use-package nerd-icons-ibuffer
  :ensure t
  :defer t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :init (setq nerd-icons-ibuffer-icon 'mazd//icon))

(use-package ibuffer-project
  :ensure t
  :defer t
  :hook (ibuffer . (lambda ()
                     "Group ibuffer's list by project."
                     (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
                     (unless (eq ibuffer-sorting-mode 'project-file-relative)
                       (ibuffer-do-sort-by-project-file-relative))))
  :init (setq ibuffer-project-use-cache t)
  :config
  (defun my-ibuffer-project-group-name (root type)
    "Return group name for project ROOT and TYPE."
    (if (and (stringp type) (> (length type) 0))
        (format "%s %s" type root)
      (format "%s" root)))
  (if (mazd//icons-displayable)
      (progn
        (advice-add #'ibuffer-project-group-name :override #'my-ibuffer-project-group-name)
        (setq ibuffer-project-root-functions
              `((ibuffer-project-project-root . ,(nerd-icons-octicon "nf-oct-repo" :height 1.2 :face ibuffer-filter-group-name-face))
                (file-remote-p . ,(nerd-icons-codicon "nf-cod-radio_tower" :height 1.2 :face ibuffer-filter-group-name-face)))))
    (progn
      (advice-remove #'ibuffer-project-group-name #'my-ibuffer-project-group-name)
      (setq ibuffer-project-root-functions
            '((ibuffer-project-project-root . "Project")
              (file-remote-p . "Remote"))))))

(use-package save-place
  :ensure nil
  :hook (after-init . save-place-mode)
  :init
  (setq save-place-file (concat mazd//cache-dir "/places"))
  )

(use-package solaire-mode
  :disabled nil
  :ensure t
  :config
  (solaire-global-mode +1))

(use-package diminish
  :ensure t
  )

(use-package centaur-tabs
  :ensure t
  :defer t
  :demand
  :custom
  (centaur-tabs-icon-type 'nerd-icons)
  :hook
  (dired-mode . centaur-tabs-local-mode)
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  :config
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-gray-out-icons 'buffer)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-height 20)
  ;; (setq centaur-tabs-set-bar 'left)
  (setq x-underline-at-descent-line t)
  (setq centaur-tabs-set-close-button t)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker "⦿")
  (setq centaur-tabs-show-new-tab-button nil)

  (defun centaur-tabs-hide-tab (x)
    "Do no to show buffer X in tabs."
    (let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       (window-dedicated-p (selected-window))

       ;; Buffer name not match below blacklist.
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*company" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p "*tramp" name)
       (string-prefix-p " *Mini" name)
       (string-prefix-p "*help" name)
       (string-prefix-p "*straight" name)
       (string-prefix-p " *temp" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p " *which" name)

       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name)))
       )))
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

  Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
  All buffer name start with * will group to \"Emacs\".
  Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
      ;; "Remote")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
			  help-mode))
       "Help")
      ((memq major-mode '(org-mode
			  org-agenda-clockreport-mode
			  org-src-mode
			  org-agenda-mode
			  org-beamer-mode
			  org-indent-mode
			  org-bullets-mode
			  org-cdlatex-mode
			  org-agenda-log-mode
			  diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  (centaur-tabs-mode t)
  )

(use-package avy
  :ensure t
  :defer t
  :config
  (avy-setup-default))

(use-package undo-tree
  :ensure t
  :disabled t
  :defer t)

(use-package smart-hungry-delete
  :ensure t
  :config
  (smart-hungry-delete-add-default-hooks)
  )

(use-package expand-region
  :ensure t
  :defer t)

(use-package paren-face
  :ensure t
  :defer t)

(use-package emojify
  :ensure t
  :defer t
  :config
  (add-hook 'after-init-hook #'global-emojify-mode)
  )

(use-package fixmee
  :ensure t
  :defer t
  :after button-lock
  :init
  (global-fixmee-mode 1))

(use-package aggressive-indent
  :ensure t
  :defer t
  :hook ((css-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (js-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode)))

(use-package electric-operator
  :ensure t
  :defer t
  :delight
  :hook (python-mode . electric-operator-mode))

(use-package simple
  :ensure nil
  :hook (before-save . delete-trailing-whitespace))

(use-package hl-todo
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'hl-todo-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (emacs-lisp-mode . rainbow-delimiters-mode)
  )

(use-package highlight-indent-guides
  :ensure t
  :defer t
  :config
  (setq highlight-indent-guides-responsive 'nil)
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-enabled nil)
  )

(use-package tree-sitter-langs
  :ensure t
  :defer t
  )

(use-package tree-sitter
  :defer t
  :config
  (require 'tree-sitter-langs)
  (add-hook 'rust-mode-hook #'tree-sitter-mode)
  (add-hook 'clang-mode-hook #'tree-sitter-mode)
  (add-hook 'nix-mode-hook #'tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package format-all
  :ensure t
  :defer t
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq format-all-show-errors 'never)
  (setq-default format-all-formatters
                '(("C"     (astyle "--mode=c"))
		  ("Nix"     (nixpkgs-fmt))
		  ("Rust" (rustfmt "--edition" "2021"))
                  ("Shell" (shfmt "-i" "4" "-ci")))))

;;; config
(defalias 'list-buffers 'ibuffer-other-window)

(defvar *protected-buffers* '("*scratch*" "*Messages*")
  "Buffers that cannot be killed.")

(defun mazd//protected-buffers ()
  "Protects some buffers from being killed."
  (dolist (buffer *protected-buffers*)
    (with-current-buffer buffer
      (emacs-lock-mode 'kill))))

(add-hook 'after-init-hook #'mazd//protected-buffers)

(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("Files" (mode . dired-mode))
	       ("Org" (name . "^.*org$"))
	       ("Web" (or (mode . web-mode) (mode . js2-mode)))
	       ("Shell" (or (mode . eshell-mode) (mode . shell-mode)))
	       ("Mail" (name . "\*mu4e\*"))
	       ("IRC" (mode . erc-mode))
	       ("Programming" (or
			       (mode . python-mode)
			       (mode . cc-mode)
			       (mode . c++-mode)))
	       ("Emacs" (or
			 (name . "^\\*scratch\\*$")
			 (name . "^\\*Messages\\*$")))
	       ))))
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-auto-mode 1)
	    (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-expert t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; highlight matches
(show-paren-mode 1)

(defun mazd//toggle-line-numbers ()
  "Cycle through absolute, relative, and no line numbers."
  (interactive)
  (cond
   ((not display-line-numbers)
    (setq display-line-numbers 't)
    (mazd//log "Absolute line numbers"))
   ((eq display-line-numbers 't)
    (setq display-line-numbers 'relative)
    (mazd//log "Relative line numbers"))
   ((eq display-line-numbers 'relative)
    (setq display-line-numbers nil)
    (mazd//log "Line numbers off"))))

;;; bindings
(leader
  "bd" 'kill-current-buffer
  "bD" 'kill-buffer
  "bB" 'ibuffer
  "bw" 'evil-write
  "bu" 'undo-tree-visualize)

(leader
  "bb" 'consult-buffer
  )

(leader
  "se" 'er/expand-region
  "sa" 'avy-goto-char)

(mazd//after smart-hungry-delete
  (general-define-key
   :prefix "<backspace>"
   :states '(insert)
   :keymaps 'override
   "" 'smart-hungry-delete-backward-char))

(leader
  "hg" 'google-this
  "hG" 'google-this-search)

(leader
  "tR" 'auto-revert-mode
  "tb" 'tool-bar-mode
  "th" 'highlight-indent-guides-mode
  "tp" 'smartparens-mode
  "tn" 'mazd//toggle-line-numbers
  "tf" 'format-all-mode
  "tr" 'rainbow-delimiters-mode)

(leader
  "/" 'comment-line)

(provide 'mazd-buffer)
;;; mazd//buffer.el ends here
