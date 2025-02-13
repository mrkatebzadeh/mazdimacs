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

;;; Path vars
;;(setq user-emacs-directory (file-name-directory load-file-name))
(message "Starting Mazdimacs")

;;; Increase the CPU processing restrictions
(when (boundp 'read-process-output-max)
  (setq process-adaptive-read-buffering nil
        read-process-output-max (* 24 1024 1024)))

;;; Speed up startup
(let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "[Emacs initialized in %.3fs]" elapsed)))))

;; Load directory function
(defun load-directory (dir)
  (let ((load-it (lambda (f)
                   (let ((file-path (concat (file-name-as-directory dir) f)))
                     (message "Loading: %s" file-path)
                     (load-file file-path)
                     (message "Finished loading: %s" file-path)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(add-to-list 'load-path (locate-user-emacs-file "modules/"))
(defun mazd//load-modules-with-progress ()
  "Load specified modules in order, displaying a progress bar."
  (let ((modules '("core" "ui" "editor" "langs" "utils" "completion"))
        (total 6)
        (count 0))
    (dolist (mod modules)
      (setq count (1+ count))
      (let* ((dir (expand-file-name mod (locate-user-emacs-file "modules/")))
             (entry-file (expand-file-name "init.el" dir)))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir)
          (when (file-exists-p entry-file)
            (message "[%s] Loading %s..." (mazd//generate-progress-bar count total) mod)
            (load entry-file))))))
  (message "All modules loaded!"))

(defun mazd//generate-progress-bar (current total)
  "Generate a simple text-based progress bar."
  (let* ((width 20)
         (progress (floor (* (/ (float current) total) width)))
         (bar (concat (make-string progress ?█)
                      (make-string (- width progress) ?-))))
    (format "%s %d/%d" bar current total)))

(mazd//load-modules-with-progress)

(add-to-list 'exec-path "/usr/local/bin/")
(add-to-list 'exec-path "/usr/local/texlive/2019basic/bin/x86_64-darwin/")
(add-to-list 'exec-path "/Library/TeX/texbin/")
(add-to-list 'exec-path "/run/current-system/sw/bin")
(add-to-list 'exec-path "~/.nix-profile/bin/")
(add-to-list 'exec-path "/opt/homebrew/bin/")

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/"))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2019basic/bin/x86_64-darwin/"))
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
(setenv "PATH" (concat (getenv "PATH") ":/run/current-system/sw/bin"))
(setenv "PATH" (concat (getenv "PATH") ":~/.nix-profile/bin/"))
(setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin/"))


;; after started up, reset GC threshold to normal.
(run-with-idle-timer 4 nil
                     (lambda ()
                       "Clean up gc."
                       (setq gc-cons-threshold  67108864) ; 64M
                       (setq gc-cons-percentage 0.1) ; original value
                       (garbage-collect)))
(provide 'init)

;;; init.el ends here
