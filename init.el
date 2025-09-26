;;; init.el --- init file -*- lexical-binding: t; -*-

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

(setq debug-on-error nil)

(when (boundp 'read-process-output-max)
  (setq process-adaptive-read-buffering nil
        read-process-output-max (* 24 1024 1024)))

(setq mazd//emacs-started nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (mazd//log "[Emacs loaded in %s with %d garbage collections.]"
		       (format "%.2f seconds"
			       (float-time
				(time-subtract after-init-time before-init-time)))
		       gcs-done)
            (setq mazd//emacs-started t)))

(defvar mazd//extra-paths
  '("/usr/local/bin"
    "/usr/local/texlive/2019basic/bin/x86_64-darwin"
    "/Library/TeX/texbin"
    "/run/current-system/sw/bin"
    "~/.nix-profile/bin"
    "/opt/homebrew/bin"
    "~/.local/bin"
    "~/.cargo/bin")
  "Directories to add to exec-path and PATH.")

(defun mazd//path-append (dir)
  "Add DIR to exec-path and append to PATH if DIR exists and is not already present."
  (let* ((dir (directory-file-name (expand-file-name dir)))
         (path (getenv "PATH"))
         (paths (and path (split-string path path-separator t))))
    (when (file-directory-p dir)
      (add-to-list 'exec-path dir)
      (unless (member dir paths)
        (setenv "PATH" (if path (concat path path-separator dir) dir))))))

(dolist (d mazd//extra-paths)
  (mazd//path-append d))


(add-to-list 'load-path (locate-user-emacs-file "modules/"))
(defun mazd//load-modules-with-progress ()
  "Load specified modules in order, displaying a progress bar."
  (let ((modules '("core" "ui" "editor" "langs" "utils" "completion" "org"))
        (total 7)
        (count 0))
    (dolist (mod modules)
      (setq count (1+ count))
      (let* ((dir (expand-file-name mod (locate-user-emacs-file "modules/")))
             (entry-file (expand-file-name "init.el" dir)))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir)
          (when (file-exists-p entry-file)
            (mazd//log "[%s] Loading %s..." (mazd//generate-progress-bar count total) mod)
            (load entry-file))))))
  (mazd//log "All modules loaded!"))

(defun mazd//generate-progress-bar (current total)
  "Generate a simple text-based progress bar."
  (let* ((width 20)
         (progress (floor (* (/ (float current) total) width)))
         (bar (concat (make-string progress ?█)
                      (make-string (- width progress) ?-))))
    (format "%s %d/%d" bar current total)))

(mazd//load-modules-with-progress)

(provide 'init)

;;; init.el ends here
