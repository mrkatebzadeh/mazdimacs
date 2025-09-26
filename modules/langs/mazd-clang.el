;;; mazd//clang.el --- Clang  -*- lexical-binding: t; -*-

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

(use-package xcscope
  :ensure t
  :defer t
  :init
  (defun mazd//load-xcscope-and-setup ()
    (interactive)
    (unless (featurep 'xcscope)
      (require 'xcscope))
    (cscope-setup))
  )

(use-package srefactor
  :ensure t
  :defer t
  :config
  (semantic-mode 1))

(use-package google-c-style
  :ensure t
  :hook ((c++-mode) . google-set-c-style))
					;  (c-mode-common . google-make-newline-indent))

(use-package cmake-mode
  :ensure t
  :defer t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :ensure t
  :defer t
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package clang-format
  :ensure t
  :defer t)

(use-package clang-format+
  :vc (:url "https://github.com/SavchenkoValeriy/emacs-clang-format-plus")
  :ensure nil
  :defer t
  :commands (clang-format+-mode)
  :init
  (add-hook 'c-mode-common-hook #'clang-format+-mode)
  )

(use-package disaster
  :ensure t
  :defer t
  :commands (disaster disaster-objdump))

(use-package makefile-executor
  :ensure t
  :defer t
  :config
  (add-hook 'makefile-mode-hook 'makefile-executor-mode))

(use-package cpp-auto-include
  :defer t
  :ensure nil
  :commands (cpp-auto-include)
  :load-path (lambda () (concat mazd//lisp-dir "/cpp-auto-include")))

;;; config

(setq-default c-default-style "linux")

(setq mazd//cc-dap-is-active nil)

(setq gdb-many-windows nil)

(defun mazd//set-gdb-layout(&optional c-buffer)
  (if (not c-buffer)
      (setq c-buffer (window-buffer (selected-window)))) ;; save current buffer

  ;; from http://stackoverflow.com/q/39762833/846686
  (set-window-dedicated-p (selected-window) nil) ;; unset dedicate state if needed
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows) ;; clean all

  (let* (
         (w-source (selected-window)) ;; left top
         (w-gdb (split-window w-source nil 'right)) ;; right bottom
         (w-locals (split-window w-gdb nil 'above)) ;; right middle bottom
         (w-stack (split-window w-locals nil 'above)) ;; right middle top
         (w-breakpoints (split-window w-stack nil 'above)) ;; right top
         (w-io (split-window w-source (floor(* 0.9 (window-body-height)))
                             'below)) ;; left bottom
         )
    (set-window-buffer w-io (gdb-get-buffer-create 'gdb-inferior-io))
    (set-window-dedicated-p w-io t)
    (set-window-buffer w-breakpoints (gdb-get-buffer-create 'gdb-breakpoints-buffer))
    (set-window-dedicated-p w-breakpoints t)
    (set-window-buffer w-locals (gdb-get-buffer-create 'gdb-locals-buffer))
    (set-window-dedicated-p w-locals t)
    (set-window-buffer w-stack (gdb-get-buffer-create 'gdb-stack-buffer))
    (set-window-dedicated-p w-stack t)

    (set-window-buffer w-gdb gud-comint-buffer)

    (select-window w-source)
    (set-window-buffer w-source c-buffer)
    ))
(defadvice gdb (around args activate)
  "Change the way to gdb works."
  (setq global-config-editing (current-window-configuration)) ;; to restore: (set-window-configuration c-editing)
  (let (
        (c-buffer (window-buffer (selected-window))) ;; save current buffer
        )
    ad-do-it
    (mazd//set-gdb-layout c-buffer))
  )
(defadvice gdb-reset (around args activate)
  "Change the way to gdb exit."
  ad-do-it
  (set-window-configuration global-config-editing))
;;; bindings

(general-define-key
 :prefix "SPC k"
 :states '(normal visual motion)
 :keymaps '(c-mode-map c++-mode-map)
 "g" '(:ignore t :which-key "goto")
 "gc" 'ccls/callee
 "gC" 'ccls/caller
 "gm" 'ccls/member
 "f" '(:ignore t :which-key "find")
 "h"  'cpp-auto-include
 "F"  'clang-format-buffer
 "d"  'cmake-objdump
 "D"  'mazd//cc-dap
 "G"  'gdb
 "m"  'cmake-make
 "b"  'cmake-build
 "M"  'cmake-make-clean
 "B"  'cmake-build-clean
 "s"  'srefactor-refactor-at-point
 "cc" 'mazd//load-xcscope-and-setup
 "cs" 'cscope-find-this-symbol
 "cd" 'cscope-find-global-definition
 "ct" 'cscope-find-this-text-string
 "cf" 'cscope-find-this-file)


(provide 'mazd-clang)
;;; mazd//clang.el ends here
