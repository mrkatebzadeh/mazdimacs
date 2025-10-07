;;; mazd//checker.el --- Checker  -*- lexical-binding: t; -*-

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

(use-package flycheck
  :defer t
  :preface

  (defun mazd//flycheck-eldoc (callback &rest _ignored)
    "Print flycheck messages at point by calling CALLBACK."
    (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
      (mapc
       (lambda (err)
         (funcall callback
		  (format "%s: %s"
			  (let ((level (flycheck-error-level err)))
			    (pcase level
			      ('info (propertize "I" 'face 'flycheck-error-list-info))
			      ('error (propertize "E" 'face 'flycheck-error-list-error))
			      ('warning (propertize "W" 'face 'flycheck-error-list-warning))
			      (_ level)))
			  (flycheck-error-message err))
		  :thing (or (flycheck-error-id err)
			     (flycheck-error-group err))
		  :face 'font-lock-doc-face))
       flycheck-errors)))

  (defun mazd//flycheck-prefer-eldoc ()
    (add-hook 'eldoc-documentation-functions #'mazd//flycheck-eldoc nil t)
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
    (setq flycheck-display-errors-function nil)
    (setq flycheck-help-echo-function nil))

  :hook ((prog-mode . flycheck-mode)
	 (flycheck-mode . mazd//flycheck-prefer-eldoc))
  :config
  (delq 'new-line flycheck-check-syntax-automatically)
  (setq flycheck-idle-change-delay 1.0)
  (setq flycheck-buffer-switch-check-intermediate-buffers t)
  (setq flycheck-display-errors-delay 0.8)

  (setq flycheck-indication-mode 'left-margin)

  (require 'nerd-icons)
  (defcustom mazd//error-icon (format " %s " (nerd-icons-codicon "nf-cod-error"))
    "Nerd icon used for Flycheck error fringe and margin."
    :type 'string
    :group 'mazd)

  (defcustom mazd//warning-icon (format " %s " (nerd-icons-codicon "nf-cod-warning"))
    "Nerd icon used for Flycheck warning fringe and margin."
    :type 'string
    :group 'mazd)

  (defcustom mazd//info-icon (format " %s " (nerd-icons-codicon "nf-cod-info"))
    "Nerd icon used for Flycheck info fringe and margin."
    :type 'string
    :group 'mazd)

  (defcustom mazd//continuation-icon (nerd-icons-codicon "nf-cod-debug_stackframe")
    "Nerd icon used for Flycheck continuation in the margin."
    :type 'string
    :group 'mazd)

  (custom-set-faces
   '(flycheck-fringe-error   ((t (:foreground "#e78284" :weight bold))))
   '(flycheck-fringe-warning ((t (:foreground "#ef9f76" :weight bold))))
   '(flycheck-fringe-info    ((t (:foreground "#99d1db" :weight bold)))))

  (flycheck-define-error-level 'error
    :severity 100 :compilation-level 2
    :overlay-category 'flycheck-error-overlay
    :margin-spec (flycheck-make-margin-spec mazd//error-icon 'flycheck-fringe-error)
    :fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    :fringe-face 'flycheck-fringe-error
    :error-list-face 'flycheck-error-list-error)

  (flycheck-define-error-level 'warning
    :severity 10 :compilation-level 1
    :overlay-category 'flycheck-warning-overlay
    :margin-spec (flycheck-make-margin-spec mazd//warning-icon 'flycheck-fringe-warning)
    :fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    :fringe-face 'flycheck-fringe-warning
    :error-list-face 'flycheck-error-list-warning)

  (flycheck-define-error-level 'info
    :severity -10 :compilation-level 0
    :overlay-category 'flycheck-info-overlay
    :margin-spec (flycheck-make-margin-spec mazd//info-icon 'flycheck-fringe-info)
    :fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    :fringe-face 'flycheck-fringe-info
    :error-list-face 'flycheck-error-list-info)

  (setf (get 'error 'flycheck-margin-continuation)
	(flycheck-make-margin-spec mazd//continuation-icon 'flycheck-fringe-error))

  (flycheck-refresh-fringes-and-margins)
  )

(use-package flycheck-indicator
  :disabled t
  :defer t
  :ensure t
  :custom
  (flycheck-indicator-icon-error mazd//error-icon)
  (flycheck-indicator-icon-warning mazd//warning-icon)
  (flycheck-indicator-icon-info mazd//info-icon)
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-indicator-mode)
  :config
  (custom-set-faces
   '(flycheck-indicator-disabled ((t (:foreground "#8c8caa" :weight bold))))
   '(flycheck-indicator-running  ((t (:foreground "#f5a97f" :weight bold))))
   '(flycheck-indicator-success  ((t (:foreground "#8bd5ca" :weight bold))))
   '(flycheck-indicator-error    ((t (:foreground "#e78284" :weight bold))))
   '(flycheck-indicator-warning  ((t (:foreground "#ef9f76" :weight bold))))
   '(flycheck-indicator-info     ((t (:foreground "#99d1db" :weight bold))))
   )

  (defun flycheck-indicator--icons-formatter (info warnings errors)
    "Get colorized icons for INFO WARNINGS and ERRORS with padding."
    (propertize (concat
		 (when (> info 0)
                   (propertize (format " %c %s" flycheck-indicator-icon-info info)
                               'font-lock-face 'flycheck-indicator-info))
		 (when (> warnings 0)
                   (propertize (format " %c %s" flycheck-indicator-icon-warning warnings)
                               'font-lock-face 'flycheck-indicator-warning))
		 (when (> errors 0)
                   (propertize (format " %c %s" flycheck-indicator-icon-error errors)
                               'font-lock-face 'flycheck-indicator-error)))
		'help-echo (concat (when (> errors 0) (format "%s errors\n" errors))
                                   (when (> warnings 0) (format "%s warnings\n" warnings))
                                   (when (> info 0) (format "%s infos\n" info))
                                   "mouse-1: Check whether Flycheck can be used in this buffer.")
		'local-map (let ((map (make-sparse-keymap)))
                             (define-key map [mode-line mouse-1]
					 'flycheck-verify-setup)
                             map)
		'mouse-face 'mode-line-highlight))

  )

(setq ispell-dictionary "en_US")
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--encoding=utf-8"))


(use-package flyspell
  :defer t
  :custom
  (flyspell-abbrev-p t)
  (flyspell-default-dictionary "en_US")
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  :hook (text-mode . flyspell-mode)
  )

(use-package flyspell-correct
  :ensure t
  :after (flyspell)
  :defer t
  :init
  (setq flyspell-correct-interface #'flyspell-correct)
  :config
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  (define-key flyspell-mouse-map [mouse-3] #'undefined)
  )

(use-package langtool
  :defer t
  :commands (langtool-check
             langtool-check-done
             langtool-show-message-at-point
             langtool-correct-buffer)
  :delight
  :custom
  (langtool-default-language "en")
  (langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")
  (langtool-java-bin "/usr/bin/java")
  (langtool-mother-tongue "en-US"))

;;; bindings
(leader
 "ts" 'flyspell-mode
 "tl" 'langtool-check
 "tc" 'global-flycheck-mode)

(leader
 "" '(:ignore t :which-key "Checker")
 "Cl" 'consult-flyspell
 "Cs" 'flyspell-correct-at-point)

(provide 'mazd-checker)
;;; mazd//checker.el ends here
