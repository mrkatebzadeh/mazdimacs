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
(eval-when-compile
  (require 'mazd-vars)
  (require 'mazd-key)
  (require 'mazd-core))

(use-package gptel
  :ensure t
  :defer t
  :init
  (defun get-key ()
    (mazd//lookup-password :host "gemini")
    )
  (setq
   gptel-default-mode 'org-mode
   ;; gptel-model "gemini-pro"
   ;; gptel-backend (gptel-make-gemini "Gemini"
   ;; :key 'get-key
   ;; :stream t)
   )
  :config
  (setq gptel-model "gpt-4o")
  ;; (add-to-list 'gptel-directives '(proofreader . "I want you act as a proofreader. I will provide you texts and I would like you to review them for any spelling, grammar, or punctuation errors. Once you have finished reviewing the text, provide me with any necessary corrections or suggestions to improve the text."))
  )

(leader
  "ag" 'gptel)

(provide 'mazd-ai)
;;; mazd//ai.el ends here
