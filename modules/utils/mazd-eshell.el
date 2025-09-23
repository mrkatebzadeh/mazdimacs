;;; mazd//eshell.el --- Eshell  -*- lexical-binding: t; -*-

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

(use-package sh-script
  :ensure nil
  :defer t
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package aweshell
  :vc (:url "https://github.com/manateelazycat/aweshell.git")
  :ensure nil
  :defer t
  :commands (aweshell-toggle aweshell-dedicated-toggle)
  )


(defun mazd//update-ticket()
  (interactive)
  (let ((command (format
		  "echo %s | kinit %s@%s && cd %s && pssh -h %s -l %s 'echo %s | kinit; aklog -force'"
		  staff-password
		  "s1691546"
		  staff-realm
		  staff-phd
		  staff-hosts
		  "s1691546"
		  staff-password)))
    (shell-command command)))

(use-package vterm
  :ensure t
  :defer t
  :init
  (defvar mazd//vterm-buffer-name "*vterm-toggle*"
    "Name of the toggleable vterm buffer.")

  (defun mazd//vterm-toggle ()
    "Toggle a vterm window at the bottom of the frame, 25% height."
    (interactive)
    (require 'vterm)
    (let* ((buf (or (get-buffer mazd//vterm-buffer-name)
                    (with-current-buffer (generate-new-buffer mazd//vterm-buffer-name)
                      (vterm-mode)
                      (current-buffer))))
           (win (get-buffer-window buf)))
      (if (and win (window-live-p win))
          (delete-window win)
	(display-buffer-in-side-window
	 buf
	 '((side . bottom)
           (window-height . 0.25)))
	(select-window (get-buffer-window buf)))))
  :bind
  (("C-\\" . mazd//vterm-toggle))
  )

(leader
  "S" '(:ignore t :which-key "Shell")
  "Ss" 'aweshell-toggle
  "Sn" 'aweshell-new
  "Sa" 'aweshell-dedicated-toggle
  "St" 'eshell-toggle
  "Su" 'mazd//update-ticket
  )

(provide 'mazd-eshell)
;;; mazd//eshell.el ends here
