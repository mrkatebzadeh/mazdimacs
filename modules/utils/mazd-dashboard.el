;;; mazd//dashboard.el --- Dashboard -*- lexical-binding: t; -*-

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

(use-package dashboard
  :disabled t
  :ensure t
  :custom
  (dashboard-banner-logo-title "[-< True happiness can be found when two contrary powers cooperate together >-]")
  (dashboard-startup-banner (concat mazd//emacs-dir "logo.txt"))
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-items '(projects bookmarks ))
  (initial-scratch-message nil)
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (dashboard-setup-startup-hook)
  )

(with-eval-after-load 'dashboard
  (defcustom mazd//dashboard-homepage-footer-url
    "https://github.com/emacs-dashboard/emacs-dashboard"
    "URL to use for `dashboard-insert-homepage-footer'."
    :type '(string)
    :group 'dashboard)
  (defcustom mazd//dashboard-set-widget-binding t
    "If non-nil show keybindings in shortmenu widgets."
    :type 'boolean
    :group 'dashboard)

  (defcustom mazd//dashboard-shortmenu-functions
    `((recents   . recentf)
      (bookmarks . bookmark-jump)
      (projects
       . ,(if (eq dashboard-projects-backend 'project-el)
              'project-switch-project
            'projectile-switch-project))
      (agenda    . org-agenda))
    "Functions to me used by shortmenu widgets.
Possible values for list-type are: `recents', `bookmarks', `projects',
`agenda' ,`registers'."
    :type  '(alist :key-type symbol :value-type function)
    :group 'dashboard)

  ;;
  ;; Faces
  ;;

  (defface mazd//dashboard-bindings-face
    '((t (:inherit font-lock-constant-face)))
    "Face used for shortmenu widgets bindings."
    :group 'dashboard)

  ;;
  ;; Widget functions
  ;;

  (defun mazd//dashboard-insert-homepage-footer ()
    "Insert a homepage footer to go `mazd//dashboard-homepage-footer-url'."
    (widget-create 'item
                   :tag dashboard-footer-icon
                   :action
                   (lambda (&rest _)
                     (browse-url mazd//dashboard-homepage-footer-url))
                   :mouse-face 'highlight
                   :button-prefix ""
                   :button-suffix ""
                   :format "%[%t%]")
    (dashboard-center-text (- (point) 1) (point))
    (insert "\n"))

  (defun mazd//dashboard-insert-project-shortmenu (&rest _)
    "Insert project shortmenu widget."
    (require 'nerd-icons)
    (let* ((fn (alist-get 'projects mazd//dashboard-shortmenu-functions))
           (fn-keymap (format "\\[%s]" fn))
           (icon-name (alist-get 'projects dashboard-heading-icons))
           (icon (nerd-icons-octicon icon-name :face 'dashboard-heading))
	   )
      (if dashboard-display-icons-p
          (insert (format "%-3s" icon)))
      (widget-create 'item
                     :tag (format "%-30s" "Open project")
                     :action (lambda (&rest _)
                               (call-interactively
				(alist-get 'projects mazd//dashboard-shortmenu-functions)))
                     :mouse-face 'highlight
                     :button-face 'dashboard-heading
                     :button-prefix ""
                     :button-suffix ""
                     :format "%[%t%]")
      (if mazd//dashboard-set-widget-binding
          (insert (propertize (substitute-command-keys fn-keymap)
                              'face
                              'mazd//dashboard-bindings-face)))))

  (defun mazd//dashboard-insert-org-agenda-shortmenu (&rest _)
    "Insert `org-agenda' shortmenu widget."
    (let* ((fn (alist-get 'agenda mazd//dashboard-shortmenu-functions))
           (fn-keymap (format "\\[%s]" fn))
           (icon-name (alist-get 'agenda dashboard-heading-icons))
           (icon (nerd-icons-octicon icon-name :face 'dashboard-heading))
	   )
      (if dashboard-display-icons-p
          (insert (format "%-3s" icon)))
      (widget-create 'item
                     :tag (format "%-30s" "Open org-agenda")
                     :action (lambda (&rest _)
                               (call-interactively
				(alist-get 'agenda mazd//dashboard-shortmenu-functions)))
                     :mouse-face 'highlight
                     :button-face 'dashboard-heading
                     :button-prefix ""
                     :button-suffix ""
                     :format "%[%t%]")
      (if mazd//dashboard-set-widget-binding
          (insert (propertize (substitute-command-keys fn-keymap)
                              'face
                              'mazd//dashboard-bindings-face)))))

  (defun mazd//dashboard-insert-bookmark-shortmenu (&rest _)
    "Insert bookmark shortmenu widget."
    (let* ((fn (alist-get 'bookmarks mazd//dashboard-shortmenu-functions))
           (fn-keymap (format "\\[%s]" fn))
           (icon-name (alist-get 'bookmarks dashboard-heading-icons))
           (icon (nerd-icons-octicon icon-name :face 'dashboard-heading))
	   )
      (if dashboard-display-icons-p
          (insert (format "%-3s" icon)))
      (widget-create 'item
                     :tag (format "%-30s" "Jump to bookmark")
                     :action (lambda (&rest _)
                               (call-interactively
				(alist-get 'bookmarks mazd//dashboard-shortmenu-functions)))
                     :mouse-face 'highlight
                     :button-face 'dashboard-heading
                     :button-prefix ""
                     :button-suffix ""
                     :format "%[%t%]")
      (if mazd//dashboard-set-widget-binding
          (insert (propertize (substitute-command-keys fn-keymap)
                              'face
                              'mazd//dashboard-bindings-face)))))

  (defun mazd//dashboard-insert-recents-shortmenu (&rest _)
    "Insert recent files short menu widget."
    (let* ((fn (alist-get 'recents mazd//dashboard-shortmenu-functions))
           (fn-keymap (format "\\[%s]" fn))
           (icon-name (alist-get 'recents dashboard-heading-icons))
           (icon (nerd-icons-octicon icon-name :face 'dashboard-heading))
	   )
      (if dashboard-display-icons-p
	  (insert (format "%-3s" icon)))
      (widget-create 'item
                     :tag (format "%-30s" "Recently opened files")
                     :action (lambda (&rest _)
                               (call-interactively
				(alist-get 'recents mazd//dashboard-shortmenu-functions)))
                     :mouse-face 'highlight
                     :button-face 'dashboard-heading
                     :button-prefix ""
                     :button-suffix ""
                     :format "%[%t%]")
      (if mazd//dashboard-set-widget-binding
	  (insert (propertize (substitute-command-keys fn-keymap)
                              'face
                              'mazd//dashboard-bindings-face)))))
  (define-key dashboard-mode-map (kbd "TAB") 'widget-forward))
(provide 'mazd-dashboard)
;;; mazd//dashboard.el ends here
