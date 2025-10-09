;;; mazd-treemacs.el --- Treemacs -*- lexical-binding: t; -*-

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

(use-package treemacs
  :async (:priority high :packages(treemacs))
  :ensure t
  :defer t
  :init
  (defconst treemacs-no-load-time-warnings t)
  :config
  (defun mazd//treemacs-padding-setup ()
    "Add top padding, header, left/right margins, and node indentation in Treemacs."
    (interactive)
    (when (eq major-mode 'treemacs-mode)
      (setq left-margin-width 2
            right-margin-width 1)
      (set-window-buffer (selected-window) (current-buffer))
      (setq header-line-format " Files")))

  (add-hook 'treemacs-mode-hook #'mazd//treemacs-padding-setup)

  (defun mazd//treemacs-darker-window-bg ()
    "Set Treemacs window background to a slightly darker shade of the current theme."
    (when (featurep 'treemacs)
      (let* ((bg (face-background 'default nil t))
             (darker (color-darken-name bg 10)))
	(set-face-background 'treemacs-window-background-face darker))))

  (add-hook 'treemacs-mode-hook #'mazd//treemacs-darker-window-bg)
  (add-hook 'after-load-theme-hook #'mazd//treemacs-darker-window-bg)

  (evil-define-key 'normal treemacs-mode-map
    (kbd "d") 'treemacs-delete-file
    (kbd "a") 'treemacs-create-file
    (kbd "A") 'treemacs-create-dir
    (kbd "R") 'treemacs-refresh
    (kbd "U") 'treemacs-root-up
    (kbd "<") 'treemacs-decrease-width
    (kbd ">") 'treemacs-increase-width
    (kbd "H") 'treemacs-toggle-show-dotfiles
    (kbd "<tab>") 'treemacs-RET-action
    (kbd "<RET>") #'treemacs-RET-action
    (kbd "r") 'treemacs-rename-file)
  (treemacs-project-follow-mode 1)

  (defun mazd//treemacs-disable-line-numbers ()
    "Disable line numbers in Treemacs mode."
    (display-line-numbers-mode -1))

  (add-hook 'treemacs-mode-hook 'mazd//treemacs-disable-line-numbers)

  (setq treemacs-collapse-dirs                 0
	treemacs-deferred-git-apply-delay      0.1
	treemacs-display-in-side-window        t
	treemacs-eldoc-display                 t
	treemacs-follow-after-init             t
	treemacs-git-command-pipe              ""
	treemacs-goto-tag-strategy             'refetch-index
	treemacs-indentation                   1
	treemacs-indentation-string            " "
	treemacs-max-git-entries               5000
	treemacs-missing-project-action        'ask
	treemacs-persist-file                  (concat mazd//cache-dir "treemacs-persist")
	treemacs-show-cursor                   nil
	treemacs-show-hidden-files             nil
	treemacs-sorting                       'alphabetic-asc
	treemacs-space-between-root-nodes      t
	treemacs-width                         40)

  (treemacs-follow-mode t)
  (treemacs-fringe-indicator-mode t)
  (treemacs-define-RET-action 'file-node-closed #'treemacs-visit-node-in-most-recently-used-window)
  (treemacs-define-RET-action 'file-node-open #'treemacs-visit-node-in-most-recently-used-window)
  (pcase (cons (not (null (executable-find "git")))
	       (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  )

(use-package treemacs-nerd-icons
  :defer t
  :ensure t
  :config
  (treemacs-load-theme "nerd-icons")
  )

(mazd//after (lsp-treemacs treemacs)
  (require 'treemacs-nerd-icons))

(use-package treemacs-evil
  :ensure t
  :after (treemacs evil)
  :defer t
  )

(use-package lsp-treemacs
  :ensure t
  :after (treemacs)
  :custom
  (lsp-treemacs-theme "nerd-icons-ext"))

(use-package lsp-treemacs-nerd-icons
  :ensure (:host github :repo "velnbur/lsp-treemacs-nerd-icons")
  :defer t
  :init
  (mazd//after lsp-treemacs
    (require 'lsp-treemacs-nerd-icons)))

(use-package  treemacs-projectile
  :ensure t
  :after(:all treemacs projectile)
  )


(use-package treemacs-icons-dired
  :ensure t
  :after (treemacs dired)
  :defer t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit)
  :defer t)

(leader
  "fe" 'treemacs)

(leader
  "lt" 'lsp-treemacs-errors-list)

(provide 'mazd-treemacs)
;;; mazd-treemacs.el ends here
