;;; mazd-dired.el --- dired -*- lexical-binding: t; -*-

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

(defun mazd//kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
	  (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
	    (kill-buffer buffer)))
	(buffer-list)))

(defun mazd//dired-create-file-or-directory ()
  "Create a new file or directory."
  (interactive)
  (let ((name (read-string "Create file/dir (end with / for dir): ")))
    (if (string-suffix-p "/" name)
        (make-directory name)
      (write-region "" nil name))
    (revert-buffer)))

(defun mazd//without-consult-completion (orig-fun &rest args)
  "Disable `completion-in-region-function` temporarily for ORIG-FUN."
  (let ((completion-in-region-function #'completion--in-region))
    (apply orig-fun args)))

(use-package dired
  :ensure nil
  :defer t
  :config
  (setq dired-kill-when-opening-new-dired-buffer t)
  (evil-collection-init 'dired)
  (if (string-equal system-type "darwin")
      ;; For macOS, compatible setting without --group-directories-first
      (setq dired-listing-switches "-alh")
    ;; For Linux or systems with GNU ls
    (setq dired-listing-switches "-alh --group-directories-first"))
  (setq insert-directory-program "ls")
  (setq dired-use-ls-dired nil)



  (advice-add 'dired-do-rename :around #'mazd//without-consult-completion)

  (evil-define-key 'normal dired-mode-map (kbd "/") 'dired-narrow
    (kbd "a") 'mazd//dired-create-file-or-directory
    (kbd ".") 'dired-jump-root
    (kbd "U") 'dired-up-directory
    (kbd "r") 'dired-do-rename
    (kbd "H") 'dired-omit-mode
    (kbd "P") 'peep-dired
    (kbd "t") 'dired-subtree-insert
    (kbd "T") 'dired-subtree-remove
    (kbd "q") 'mazd//kill-dired-buffers)
  (evil-define-key 'normal peep-dired-mode-map (kbd "<SPC>") 'peep-dired-scroll-page-down
    (kbd "C-<SPC>") 'peep-dired-scroll-page-up
    (kbd "<backspace>") 'peep-dired-scroll-page-up
    (kbd "j") 'peep-dired-next-file
    (kbd "k") 'peep-dired-prev-file)
  (add-hook 'peep-dired-hook 'evil-normalize-keymaps)
  :init
  (setq dired-auto-revert-buffer t
	dired-dwim-target t
	dired-hide-details-hide-symlink-targets nil
	;; Always copy/delete recursively
	dired-recursive-copies  'always
	dired-recursive-deletes 'top
	;; Where to store image caches
	image-dired-dir (concat mazd//cache-dir "image-dired/")
	image-dired-db-file (concat image-dired-dir "db.el")
	image-dired-gallery-dir (concat image-dired-dir "gallery/")
	image-dired-temp-image-file (concat image-dired-dir "temp-image")
	image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")))
(use-package dired-git-info
  :ensure t
  :defer t
  :after dired
  :config
  ;; (setq dgi-auto-hide-details-p nil)
  )

(use-package dired-rsync
  :defer t
  :after dired
  :ensure t
  )
(use-package dired-rsync-transient
  :defer t
  :after dired
  :ensure nil
  )

(use-package diredfl
  :ensure t
  :defer t
  :after dired
  :config
  (diredfl-global-mode))

(use-package peep-dired
  :ensure t
  :after dired
  :defer t
  :init
  (setq peep-dired-cleanup-on-disable t
	peep-dired-cleanup-eagerly t
	peep-dired-enable-on-directories t
	peep-dired-ignored-extensions '("mkv" "iso" "mp4")))

(use-package dired-narrow
  :ensure t
  :after dired
  :defer t)

(use-package dired-subtree
  :ensure nil
  :after dired
  :defer t)

(use-package nerd-icons-dired
  :defer t
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
	      (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  (defun mazd//dired-disable-line-numbers ()
    "Disable line numbers in dired-sidebar mode."
    (display-line-numbers-mode -1))

  (add-hook 'dired-sidebar-mode-hook 'mazd//dired-disable-line-numbers)


  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "  │")
  (setq dired-sidebar-theme 'all-the-icons)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  (setq dired-sidebar-window-fixed nil)
  (setq dired-sidebar-width 25)
  (setq dired-listing-switches "-alhv --group-directories-first")

  (evil-define-key 'normal dired-sidebar-mode-map (kbd "/") 'dired-narrow
    (kbd "RET") 'dired-sidebar-subtree-toggle
    (kbd "a") 'mazd//dired-create-file-or-directory
    (kbd ".") 'dired-sidebar-find-file
    (kbd "U") 'dired-sidebar-up-directory
    (kbd "r") 'dired-do-rename
    (kbd "H") 'dired-omit-mode
    (kbd "P") 'peep-dired
    (kbd "t") 'dired-subtree-insert
    (kbd "T") 'dired-subtree-remove
    (kbd "q") 'mazd//kill-dired-buffers)
  )

(defun mazd//sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  )

(defun mazd//project-switch-project ()
  "Switch to a project and open its root directory in `dired`."
  (interactive)
  (let ((project (project-prompt-project-dir)))
    (when project
      (dired project))))

(general-define-key
 :prefix "SPC k"
 :states '(normal visual motion)
 :keymaps 'dired-mode-map
 "g" 'dired-git-info-mode
 "r" 'dired-rsync
 "R" 'dired-rsync-transient
 )

(leader
  ;; "fe" 'mazd//sidebar-toggle
  "ad" 'dired)


(provide 'mazd-dired)
;;; mazd-dired.el ends here
