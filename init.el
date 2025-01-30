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
(eval-when-compile (require 'cl-lib))

(let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "[Emacs initialized in %.3fs]" elapsed)))))

;;; Increase gc threshold to speedup starting up
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

;; Load directory function
(defun load-directory (dir)
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))
		 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(defun load-modules (dir)
  (mapcar 'load (directory-files-recursively dir "")))

(eval-and-compile
  (add-to-list 'load-path (locate-user-emacs-file "modules/"))
  )

(defun mazd//require-config-module-maybe-byte-compile (feature)
  (let* ((gc-cons-threshold 800000)
	 (modules-dir (locate-user-emacs-file "modules/"))
         (basename (symbol-name feature))
         (source (expand-file-name (concat basename ".el") modules-dir))
         (dest (expand-file-name (concat basename ".elc") modules-dir)))
    (when (or (not (file-exists-p dest))
              (file-newer-than-file-p source dest))
      (message "Byte-compiling %s..." basename)
      (if (if (and nil (require 'async nil t))
              (async-get
               (async-start
                `(lambda ()
                   (add-to-list 'load-path
                                (locate-user-emacs-file "modules/"))
                   (setq load-prefer-newer t)
                   (require 'config-core)
                   (require 'config-package)
                   (byte-compile-file ,source))))
            (require feature)
            (byte-compile-file source))
          (message "Byte-compiling %s...done" basename)
        (message "Byte-compiling %s...failed" basename))))
  (require feature))

(defun mazd//require-config-module-force-byte-compile (feature)
  "Force recompilation of FEATURE before loading it."
  (let* ((gc-cons-threshold 800000)
         (modules-dir (locate-user-emacs-file "modules/"))
         (basename (symbol-name feature))
         (source (expand-file-name (concat basename ".el") modules-dir)))
    (if (file-exists-p source)
        (progn
          (message "Forcing byte-compilation of %s..." basename)
          (if (byte-compile-file source)
              (message "Byte-compiling %s...done" basename)
            (message "Byte-compiling %s...failed" basename)))
      (message "Source file %s.el not found in modules directory!" basename)))
  (require feature))

(defun mazd//maybe-byte-compile-init-el ()
  (let ((init-elc (concat (file-name-sans-extension user-init-file)
                          ".elc")))
    (when (and (file-newer-than-file-p user-init-file init-elc)
               (not (equal (file-name-extension user-init-file) "eln")))
      (byte-compile-file user-init-file)
      (when (and (fboundp 'restart-emacs)
                 (y-or-n-p (format "%s was newer than %s. Restart?"
                                   user-init-file
                                   init-elc)))
        (restart-emacs)))))

(defun mazd//force-byte-compile ()
  (interactive)
  (dolist (feature '(
		     mazd-vars
		     mazd-core
		     mazd-func
		     mazd-package
		     mazd-key
		     mazd-ui
		     mazd-config
		     mazd-buffer
		     mazd-theme
		     mazd-file
		     mazd-window
		     mazd-dashboard
		     mazd-corfu
		     mazd-elisp
		     mazd-consult
		     mazd-git
		     mazd-eglot
		     mazd-eshell
		     mazd-help
		     mazd-checker
		     mazd-org
		     mazd-python
		     mazd-latex
		     mazd-clang
		     mazd-rust
		     mazd-nix
		     mazd-ai
		     mazd-calc
		     mazd-calendar
		     mazd-company
		     mazd-docker
		     mazd-docs
		     mazd-email
		     mazd-irc
		     mazd-ledger
		     mazd-media
		     mazd-music
		     mazd-power
		     mazd-rfc
		     mazd-search
		     mazd-snippet
		     ))
    (mazd//require-config-module-force-byte-compile feature)))

(global-set-key (kbd "C-c b") #'mazd//force-byte-compile)

(defmacro mazd//require-config-module (feature)
  `(if (fboundp 'mazd//require-config-module-maybe-byte-compile)
       (mazd//require-config-module-maybe-byte-compile ,feature)
     (require ,feature)))

(add-hook 'after-init-hook #'mazd//maybe-byte-compile-init-el)

(message "[               ] vars")
(mazd//require-config-module 'mazd-vars)
;; (load mazd//vars-file)

(message "[               ] core")
(mazd//require-config-module 'mazd-core)
;; (load mazd//core-file)

(message "[               ] functions")
(mazd//require-config-module 'mazd-func)

(message "[               ] packages")
(mazd//require-config-module 'mazd-package)

(message "[=              ] keys")
(mazd//require-config-module 'mazd-key)
;; (load mazd//key-file)

(message "[=              ] ui")
(mazd//require-config-module 'mazd-ui)
;; (load mazd//ui-file)

(message "[==             ] config")
(mazd//require-config-module 'mazd-config)

(message "[==             ] buffer")
(mazd//require-config-module 'mazd-buffer)

(message "[===            ] theme")
(mazd//require-config-module 'mazd-theme)

(message "[===            ] file")
(mazd//require-config-module 'mazd-file)

(message "[====           ] window")
(mazd//require-config-module 'mazd-window)

(message "[====           ] dashboard")
(mazd//require-config-module 'mazd-dashboard)

(message "[=====          ] corfu")
(mazd//require-config-module 'mazd-corfu)

(message "[======         ] elisp")
(mazd//require-config-module 'mazd-elisp)

(message "[======         ] consult")
(mazd//require-config-module 'mazd-consult)

(message "[=======        ] git")
(mazd//require-config-module 'mazd-git)

(message "[========       ] eglot")
(mazd//require-config-module 'mazd-eglot)

(message "[========       ] eshell")
(mazd//require-config-module 'mazd-eshell)

(message "[=========      ] checker: help")
(mazd//require-config-module 'mazd-help)

(message "[=========      ] checker: checker")
(mazd//require-config-module 'mazd-checker)

(message "[==========     ] org")
(mazd//require-config-module 'mazd-org)

(message "[===========    ] lang: python")
(mazd//require-config-module 'mazd-python)

(message "[===========    ] lang: latex")
(mazd//require-config-module 'mazd-latex)

(message "[===========    ] lang: c/c++")
(mazd//require-config-module 'mazd-clang)

(message "[===========    ] lang: rust")
(mazd//require-config-module 'mazd-rust)

(message "[===========    ] lang: nix")
(mazd//require-config-module 'mazd-nix)

(message "[============   ] app: ai")
(mazd//require-config-module 'mazd-ai)

(message "[============   ] app: calc")
(mazd//require-config-module 'mazd-calc)

(message "[============   ] app: calendar")
(mazd//require-config-module 'mazd-calendar)

(message "[============   ] app: company")
(mazd//require-config-module 'mazd-company)

(message "[============   ] app: docker")
(mazd//require-config-module 'mazd-docker)

(message "[============   ] app: docs")
(mazd//require-config-module 'mazd-docs)

(message "[============   ] app: email")
(mazd//require-config-module 'mazd-email)

(message "[============   ] app: irc")
(mazd//require-config-module 'mazd-irc)

(message "[============   ] app: ledger")
(mazd//require-config-module 'mazd-ledger)

(message "[============   ] app: media")
(mazd//require-config-module 'mazd-media)

(message "[============   ] app: music")
(mazd//require-config-module 'mazd-music)

(message "[============   ] app: power")
(mazd//require-config-module 'mazd-power)

(message "[============   ] app: rfc")
(mazd//require-config-module 'mazd-rfc)

(message "[============   ] app: search")
(mazd//require-config-module 'mazd-search)

(message "[============   ] app: snippet")
(mazd//require-config-module 'mazd-snippet)

(message "[=============  ] tramp")
(mazd//require-config-module 'mazd-tramp)

;; (load-modules mazd//modules-dir)
;;; run server
(require 'server)
(unless (server-running-p)
  (server-start))
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
