;;; mazd//ai.el --- AI -*- lexical-binding: t; -*-

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

;;; Code:

(use-package gptel
  :ensure t
  :defer t
  :init
  (setq gptel-api-key (string-trim (shell-command-to-string "pass openai/key")))
  :config
  (setq gptel-model "gpt-5-mini")
  (add-to-list 'gptel-directives
	       '(proofreader . "Act as a proofreader. Review text for spelling, grammar, or punctuation errors and suggest improvements.")))

(use-package gptel-magit
  :defer t
  :ensure t
  :hook (magit-mode . gptel-magit-install))

(use-package aidermacs
  :vc (:url "https://github.com/MatthewZMD/aidermacs")
  :config
  (defun pass-openai-key ()
    (string-trim (shell-command-to-string "pass openai/key")))
  (setenv "OPENAI_API_KEY" (pass-openai-key))
  :custom
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model "openai/gpt-5"))

(leader
  "aa" 'aidermacs-transient-menu
  "ag" 'gptel)

(provide 'mazd-ai)
;;; mazd//ai.el ends here
