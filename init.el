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
                   (let ((file-path (concat (file-name-as-directory dir) f)))
                     (message "Loading: %s" file-path)
                     (load-file file-path)
                     (message "Finished loading: %s" file-path)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(add-to-list 'load-path (locate-user-emacs-file "modules/"))

(message "[               ] vars")
(require 'mazd-vars)
;; (load mazd//vars-file)

(message "[               ] core")
(require 'mazd-core)
;; (load mazd//core-file)

(message "[               ] functions")
(require 'mazd-func)

(message "[               ] packages")
(require 'mazd-package)

(message "[=              ] keys")
(require 'mazd-key)
;; (load mazd//key-file)

(message "[=              ] ui")
(require 'mazd-ui)
;; (load mazd//ui-file)

(message "[==             ] config")
(require 'mazd-config)

(message "[==             ] buffer")
(require 'mazd-buffer)

(message "[===            ] theme")
(require 'mazd-theme)

(message "[===            ] file")
(require 'mazd-file)

(message "[====           ] window")
(require 'mazd-window)

(message "[====           ] dashboard")
(require 'mazd-dashboard)

(message "[=====          ] corfu")
(require 'mazd-corfu)

(message "[======         ] elisp")
(require 'mazd-elisp)

(message "[======         ] consult")
(require 'mazd-consult)

(message "[=======        ] git")
(require 'mazd-git)

(message "[========       ] eglot")
(require 'mazd-eglot)

(message "[========       ] eshell")
(require 'mazd-eshell)

(message "[=========      ] checker: help")
(require 'mazd-help)

(message "[=========      ] checker: checker")
(require 'mazd-checker)

(message "[==========     ] org")
(require 'mazd-org)

(message "[===========    ] lang: python")
(require 'mazd-python)

(message "[===========    ] lang: latex")
(require 'mazd-latex)

(message "[===========    ] lang: c/c++")
(require 'mazd-clang)

(message "[===========    ] lang: rust")
(require 'mazd-rust)

(message "[===========    ] lang: zig")
(require 'mazd-zig)

(message "[===========    ] lang: nix")
(require 'mazd-nix)

(message "[============   ] app: ai")
(require 'mazd-ai)

(message "[============   ] app: calc")
(require 'mazd-calc)

(message "[============   ] app: calendar")
(require 'mazd-calendar)

(message "[============   ] app: company")
(require 'mazd-company)

(message "[============   ] app: docker")
(require 'mazd-docker)

(message "[============   ] app: docs")
(require 'mazd-docs)

(message "[============   ] app: email")
(require 'mazd-email)

(message "[============   ] app: irc")
(require 'mazd-irc)

(message "[============   ] app: ledger")
(require 'mazd-ledger)

(message "[============   ] app: media")
(require 'mazd-media)

(message "[============   ] app: music")
(require 'mazd-music)

(message "[============   ] app: power")
(require 'mazd-power)

(message "[============   ] app: rfc")
(require 'mazd-rfc)

(message "[============   ] app: search")
(require 'mazd-search)

(message "[============   ] app: snippet")
(require 'mazd-snippet)

(message "[=============  ] tramp")
(require 'mazd-tramp)

(message "[==============] eshell")
(require 'mazd-tramp)
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
