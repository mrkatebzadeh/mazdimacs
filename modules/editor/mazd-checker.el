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

(use-package flymake
  :defer t
  :ensure nil
  :config ; (Optional) For fix bad icon display (Only for left margin)
  (advice-add #'flymake--indicator-overlay-spec
              :filter-return
              (lambda (indicator)
                (concat indicator
                        (propertize " "
                                    'face 'default
                                    'display `((margin left-margin)
                                               (space :width 3))))))
  (setq-default left-margin-width 2 right-margin-width 0)
  :custom
  (flymake-indicator-type 'margins)
  (flymake-margin-indicators-string
   `((error ,(nerd-icons-codicon "nf-cod-warning") compilation-error)
     (warning ,(nerd-icons-faicon "nf-fa-warning") compilation-warning)
     (note ,(nerd-icons-faicon "nf-fa-circle_info") compilation-info))))

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

  :hook ((flycheck-mode . mazd//flycheck-prefer-eldoc))
  )

(use-package flycheck-eglot
  :ensure t
  :defer t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(setq ispell-dictionary "en")
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--encoding=utf-8"))


(use-package flyspell
  :defer t
  :custom
  (flyspell-abbrev-p t)
  (flyspell-default-dictionary "en_US")
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  :init
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))
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


(provide 'mazd-checker)
;;; mazd//checker.el ends here
