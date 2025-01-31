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
  ;; face for starred articles
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

;; add a star
(defun mazd//elfeed-star ()
  "Apply starred to all selected entries."
  (interactive )
  (let* ((entries (elfeed-search-selected))
	 (tag (intern "starred")))
    (cl-loop for entry in entries do (elfeed-tag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;; remove a start
(defun mazd//elfeed-unstar ()
  "Remove starred tag from all selected entries."
  (interactive )
  (let* ((entries (elfeed-search-selected))
	 (tag (intern "starred")))
    (cl-loop for entry in entries do (elfeed-untag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;;shortcut to jump to starred bookmark
(defun mazd//elfeed-show-starred ()
  (interactive)
  (bookmark-jump "elfeed-starred"))

;;searches
(defun mazd//elfeed-show-all ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-all"))

(defun mazd//elfeed-show-emacs ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-emacs"))

(defun mazd//elfeed-show-daily ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-daily"))

(defun mazd//elfeed-show-network ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-network"))

;; makes sure elfeed reads index from disk before launching
(defun mazd//elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))

;;write to disk when quiting
(defun mazd//elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(leader
  "ae" 'elfeed)

(with-eval-after-load 'elfeed
  (general-define-key
   :prefix "SPC k"
   :states 'normal
   :keymaps 'elfeed-search-mode-map
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
