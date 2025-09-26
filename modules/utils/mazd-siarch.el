;;; mazd//siarch.el --- Siarch -*- lexical-binding: t; -*-

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

(defvar siarch-dir (expand-file-name ".siarch" (getenv "HOME"))
  "Directory where the siarch script is located.")

;;;###autoload
(defun mazd//siarch-nix-file ()
  "Open flake.nix file."
  (interactive)
  (find-file (concat siarch-dir "/flake.nix")))


;;;###autoload
(defvar siarch-hostname-params
  '((SiAir . "macbookair")
    (SiAir.local . "macbookair")
    (shiraz . "shiraz"))
  "Mapping of hostnames to parameters for ./siarch.sh.")

;;;###autoload
(defun mazd//siarch-rebuild ()
  "Run ./siarch.sh with different arguments based on the selected hostname."
  (interactive)
  (let* ((hostnames (mapcar 'car siarch-hostname-params)) ; List of hostnames
         (hostname (completing-read "Select hostname: " hostnames nil t)) ; Prompt user to select
         (param (cdr (assoc (intern hostname) siarch-hostname-params)))
         (command (when param
                    (concat "cd " siarch-dir " && ./siarch.sh " param)))
         (buf "*Siarch-output*"))
    (if command
        (progn
          ;; Create or reuse the buffer
          (let ((existing-buffer (get-buffer buf)))
            (when existing-buffer
              (kill-buffer existing-buffer)))
          (async-shell-command command buf)

          (let ((win (get-buffer-window buf)))
            (unless win
              (setq win (split-window (selected-window) nil 'below)))

            (set-window-buffer win buf)
            (with-selected-window win
              (setq mode-line-format nil)
              (fit-window-to-buffer win)
              (set-window-dedicated-p win t))))
      (mazd//err "Hostname not recognized: %s" hostname))))

;;;###autoload
(defun mazd//siarch-cancel ()
  "Kill the *Siarch-output* buffer and its window."
  (interactive)
  (let ((buf "*Siarch-output*"))
    (if (get-buffer buf)
        (progn
          (let ((win (get-buffer-window buf)))
            (when win
              (delete-window win)))
          (kill-buffer buf))
      (mazd//err "Buffer *Siarch-output* does not exist."))))

(leader
  "cs" '(:ignore t :which-key "Siarch")
  "cso" 'mazd//siarch-nix-file
  "csb" 'mazd//siarch-rebuild
  "csc" 'mazd//siarch-cancel)

(provide 'mazd-siarch)
;;; mazd//siarch.el ends here
