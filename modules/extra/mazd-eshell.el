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
  :ensure (:host github :repo "manateelazycat/aweshell")
  :defer t
  :commands (aweshell-toggle aweshell-dedicated-toggle)
  )

;;;###autoload
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
  :hook (vterm-mode . hide-mode-line-mode)
  :bind
  (("C-\\" . mazd//vterm-toggle))
  :preface
  (when noninteractive
    (advice-add #'vterm-module-compile :override #'ignore)
    (provide 'vterm-module))
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000)
  )

(defvar mazd//vterm-buffer-name "*vterm-toggle*"
  "Name of the toggleable vterm buffer.")

;;;###autoload
(defun mazd//vterm-toggle ()
  "Toggle a vterm window at the bottom of the frame, 25% height."
  (interactive)
  (require 'vterm)
  (let* ((buf (or (get-buffer mazd//vterm-buffer-name)
                  (with-current-buffer (generate-new-buffer mazd//vterm-buffer-name)
                    (vterm-mode)
                    ;; Disable line numbers in vterm
                    (when (bound-and-true-p display-line-numbers-mode)
		      (display-line-numbers-mode -1))
                    (when (fboundp 'linum-mode)
		      (linum-mode -1))
                    (current-buffer))))
         (win (get-buffer-window buf)))
    (if (and win (window-live-p win))
        (delete-window win)
      (display-buffer
       buf
       '((display-buffer-reuse-window
	  display-buffer-below-selected)
	 (window-height . 0.25)))
      (select-window (get-buffer-window buf)))))

(leader
  "s" '(:ignore t :which-key "Shell")
  "ss" 'mazd//vterm-toggle
  "sS" 'aweshell-toggle
  "sn" 'aweshell-new
  "sa" 'aweshell-dedicated-toggle
  "st" 'eshell-toggle
  "su" 'mazd//update-ticket
  )

(provide 'mazd-eshell)
;;; mazd//eshell.el ends here
