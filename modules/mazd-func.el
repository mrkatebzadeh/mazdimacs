;;; mazd//func.el --- File  -*- lexical-binding: t; -*-

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

;; -*- lexical-binding: t -*-
(eval-when-compile
  (require 'cl-lib)
  (require 'nadvice))

(defmacro mazd//require-config-module (feature)
  `(if (fboundp 'mazd//require-config-module-maybe-byte-compile)
       (mazd//require-config-module-maybe-byte-compile ,feature)
     (require ,feature)))

(defmacro mazd//defun-as-value (name arglist &optional docstring &rest body)
  (declare (doc-string 3) (indent 2))
  `(progn
     (defun ,name ,arglist ,docstring ,@body)
     #',name))

;; basically, a mapcar for macros
(defmacro mazd//generate-calls (operator arglists)
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (arglist) `(,(cadr operator) ,@arglist)) (cadr arglists))))

(defmacro mazd//generate-calls-single (operator arglist)
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (arg) `(,(cadr operator) (,@arg))) (cadr arglist))))

(defmacro mazd//onetime-setup (name &optional docstring &rest body)
  (declare (doc-string 3) (indent 1))
  (let ((func-name (intern (concat "mazd//"
                                   (symbol-name name)
                                   "-onetime-setup")))
        keyw hook condition after-hook)
    (unless (stringp docstring)
      (push docstring body)
      (setq docstring nil))
    (while (keywordp (setq keyw (car body)))
      (setq body (cdr body))
      (pcase keyw
        (`:hook (setq hook (pop body)))
        (`:after-hook (setq after-hook (pop body)))
        (`:condition (setq condition (pop body)))
        (_  (error (format "Unrecognized keyword")))))
    (cl-assert hook nil ":hook not specified!")
    `(progn
       (defun ,func-name nil ,docstring
              ,@(if condition
                    `((when ,condition
                        ,@body
                        (remove-hook ,hook #',func-name)))
                  `(,@body
                    (remove-hook ,hook #',func-name)))
              )
       ,@(if after-hook
             (let ((setup-func-name
                    (intern (concat "mazd//setup-"
                                    (symbol-name name)
                                    "-onetime-setup"))))
               `((defun ,setup-func-name
                     ()
                   ,(concat "setup mazd//"
                            (symbol-name name)
                            "-onetime-setup")
                   (add-hook ,hook #',func-name))
                 (add-hook ,after-hook #',setup-func-name)))
           `((add-hook ,hook #',func-name))))))

(provide 'mazd-func)
;;; mazd//func.el ends here
