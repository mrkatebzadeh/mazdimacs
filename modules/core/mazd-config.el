;;; mazd//config.el --- Config  -*- lexical-binding: t; -*-

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

(use-package try
  :ensure t
  :defer t)

(defun mazd//init-file ()
  "Open init.el file."
  (interactive)
  ;; (find-file (concat mazd//emacs-dir "init.el"))
  (find-file "~/.config/emacs/init.el")
  )


(defun mazd//kill-emacs ()
  "Check for unsaved file buffers before exiting Emacs."
  (interactive)
  (let ((unsaved-buffers
         (cl-loop for buf in (buffer-list)
                  for filename = (buffer-file-name buf)
                  when (and filename (buffer-modified-p buf))
                  collect buf)))
    (if unsaved-buffers
        (if (y-or-n-p (format "There are %d unsaved file buffers. Save them before exiting? " (length unsaved-buffers)))
            (progn
              (save-some-buffers t)
              (kill-emacs))
          (mazd//err "Exit aborted."))
      (kill-emacs))))

;;; bindigs
(leader
  "q" '(:ignore t :which-key "Quit")
  "qq" 'mazd//kill-emacs
  "qQ" 'delete-frame)

(leader
  "c" '(:ignore t :which-key "Configs")
  "cc" 'mazd//init-file
  "ct" 'try)


(provide 'mazd-config)
;;; mazd//config.el ends here
