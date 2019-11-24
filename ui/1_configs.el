;;; configs.el --- UI -*- lexical-binding: t; -*-

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

(setq inhibit-startup-message t)
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-banner-logo-title "[-< True happiness can be found when two contrary powers cooperate together >-]"
      dashboard-startup-banner (concat mk-emacs-dir "logo.png")
      dashboard-center-content t
      dashboard-set-heading-icons t
      dashboard-items '((recents  . 7)
			(projects . 5)
                        (agenda . 5)
                        (bookmarks . 10)))
(setq initial-scratch-message nil)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;;; parentheses
;; highlight matches
(show-paren-mode 1)

;; transparency
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '(85 . 50) '(100 . 100)))))

;; highlight-indent-guides
(with-eval-after-load 'highlight-indent-guides
  (setq highlight-indent-guides-method 'character))

;;; Font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 95
                    :weight 'normal
                    :width 'normal)

;;; highlight current line
(global-hl-line-mode +1)
;; bars
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;;; configs.el ends here
