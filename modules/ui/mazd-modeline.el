;;; mazd-modeline.el --- Modeline -*- lexical-binding: t; -*-

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

(use-package doom-modeline
  :disabled t
  :ensure t
  :init
  (doom-modeline-mode 1)

  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-hud nil)
  (setq doom-modeline-window-width-limit 85)

  (doom-modeline-def-segment mazd-incremental-load
    "A segment displaying the incremental package loading progress."
    (if (>= mazd//incremental-load-progress mazd//incremental-load-total)
	(progn
	  ;; Schedule removal after 3 seconds
	  (run-at-time 3 nil
		       (lambda ()
			 (doom-modeline-def-modeline 'mazd-custom-line
			   '(bar matches buffer-info remote-host buffer-position parrot selection-info)
			   '(misc-info minor-modes input-method buffer-encoding major-mode process vcs check))
			 (doom-modeline-set-modeline 'mazd-custom-line 'default)
			 (force-mode-line-update)))
	  (propertize " [✔]" 'face 'success))  ;; Show checkmark when done
      (propertize (format " %s [%d/%d]"
			  (or mazd//current-loading-package "")
			  mazd//incremental-load-progress
			  mazd//incremental-load-total)
		  'face 'warning)))

  (doom-modeline-def-modeline 'mazd-custom-line
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(mazd-incremental-load misc-info minor-modes input-method buffer-encoding major-mode process vcs check))

  (doom-modeline-set-modeline 'mazd-custom-line 'default)
  )

(use-package telephone-line
  ;; :disabled t
  :ensure t
  :init
  (telephone-line-mode t)
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
	telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
	telephone-line-primary-right-separator 'telephone-line-cubed-right
	telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (setq telephone-line-height 18
	telephone-line-evil-use-short-tag nil)
  )

(provide 'mazd-modeline)
;;; mazd-modeline.el ends here
