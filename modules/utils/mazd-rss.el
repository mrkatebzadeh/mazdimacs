;;; mazd//rss.el --- RSS -*- lexical-binding: t; -*-

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

(use-package elfeed
  :ensure t
  :defer t
  :init
  (setq elfeed-search-filter "@all")
  :config
  (defface elfeed-search-starred-title-face
    '((t :foreground "#f77"))
    "Marks a starred Elfeed entry.")
  (push '(starred elfeed-search-starred-title-face) elfeed-search-face-alist)
  (defalias 'elfeed-toggle-star
    (elfeed-expose #'elfeed-search-toggle-all 'starred))
  )

(use-package elfeed-org
  :ensure t
  :after elfeed
  :defer t
  :init (elfeed-org)
  :config
  (setq rmh-elfeed-org-files  (list (concat org-directory "/feed/emacs.org")
				    (concat org-directory "/feed/research.org"))))

;;;###autoload
(defun mazd//elfeed-star ()
  "Apply starred to all selected entries."
  (interactive )
  (let* ((entries (elfeed-search-selected))
	 (tag (intern "starred")))
    (cl-loop for entry in entries do (elfeed-tag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;;;###autoload
(defun mazd//elfeed-unstar ()
  "Remove starred tag from all selected entries."
  (interactive )
  (let* ((entries (elfeed-search-selected))
	 (tag (intern "starred")))
    (cl-loop for entry in entries do (elfeed-untag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;;;###autoload
(defun mazd//elfeed-show-starred ()
  (interactive)
  (bookmark-jump "elfeed-starred"))

;;;###autoload
(defun mazd//elfeed-show-all ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-all"))

;;;###autoload
(defun mazd//elfeed-show-emacs ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-emacs"))

;;;###autoload
(defun mazd//elfeed-show-daily ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-daily"))

;;;###autoload
(defun mazd//elfeed-show-network ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-network"))

;;;###autoload
(defun mazd//elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

;;;###autoload
(defun mazd//elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(leader
  "ae" 'elfeed)

(mazd//after elfeed
  (local-leader elfeed-search-mode-map
		"" '(:ignore t :which-key "Elfeed Mode")
		"a" 'elfeed-show-all
		"b" '(:ignore t :which-key "bookmarks")
		"be" 'mazd//elfeed-show-emacs
		"bd" 'mazd//elfeed-show-daily
		"bn" 'mazd//elfeed-show-network
		"bs" 'mazd//elfeed-show-starred
		"q" 'mazd//elfeed-save-db-and-bury
		"s" 'mazd//elfeed-star
		"S" 'mazd//elfeed-unstar
		"u" 'elfeed-update))

(provide 'mazd-rss)
;;; mazd//rss.el ends here
