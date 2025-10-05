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

(require 'nadvice)

(defmacro mazd//after (features &rest body)
  "Run BODY after loading FEATURE.
    Same as `with-eval-after-load', but there is no need to quote FEATURES."
  (declare (debug (sexp body)) (indent 1))
  (setf features (if (listp features) (nreverse features) (list features)))
  (let* ((module (pop features))
         (form `(with-eval-after-load
                    ,(if (stringp module)
                         module
                       `(quote ,module))
                  ,@body)))
    (while features
      (-let [module (pop features)]
        (setf form `(with-eval-after-load
                        ,(if (stringp module)
                             module
                           `(quote ,module))
                      ,form))))
    form))

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

(defmacro mazd//schedule (time repeat &rest body)
  "Schedule code to run after the given TIME in seconds.
Timer will repeat if REPEAT is `:repeat'.
BODY can either be raw lambda body or a function reference."
  (declare (indent 2))
  `(run-with-timer
    ,time ,(eq repeat :repeat)
    ,(pcase body
       (`((function ,_)) (car body))
       (_ `(lambda () ,@body)))))

(defmacro mazd//idle-schedule (time repeat &rest body)
  "Schedule code to run after the given idle TIME in seconds.
Timer will repeat if REPEAT is `:repeat'.
BODY can either be raw lambda body or a function reference."
  (declare (indent 2))
  `(run-with-idle-timer
    ,time ,(eq repeat :repeat)
    ,(pcase body
       (`((function ,_)) (car body))
       (_ `(lambda () ,@body)))))

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


(defun mazd//icons-displayable ()
  "Return non-nil if icons are displayable."
  (and mazd//icon
       (or (featurep 'nerd-icons)
	   (require 'nerd-icons nil t))))

(defun mazd//read-encrypted-file (file)
  "Decrypt a GPG-encrypted file and return its contents as a string."
  (let ((full-path (expand-file-name file)))
    (mazd//log "Trying to decrypt: %s" full-path)
    (if (not (file-exists-p full-path))
        (error "File does not exist: %s" full-path)
      (with-temp-buffer
        (let ((exit-code (call-process "gpg" nil t nil "--quiet" "--batch" "--decrypt" full-path)))
          (if (zerop exit-code)
              (buffer-string)
            (error "GPG decryption failed")))))))

(defun mazd//darken-color (color amount)
  "Return a darker variant of COLOR by AMOUNT (0–255)."
  (when (stringp color)
    (let ((rgb (color-name-to-rgb color)))
      (apply #'color-rgb-to-hex
             (mapcar (lambda (c)
		       (max 0 (- c (/ amount 255.0))))
                     rgb)))))

(defun mazd//lighten-color (color amount)
  "Return a lighter variant of COLOR by AMOUNT (0–255)."
  (when (stringp color)
    (let ((rgb (color-name-to-rgb color)))
      (apply #'color-rgb-to-hex
             (mapcar (lambda (c)
                       (min 1.0 (+ c (/ amount 255.0))))
                     rgb)))))

(defun mazd//get-bg-color ()
  "Return the current default background color as a hex string."
  (face-background 'default nil))

(defun mazd//autoload-generate ()
  "Generate a single autoload file for all modules in `mazd/modules`.

This function scans every `.el` file inside each module directory listed in
`mazd/modules` (under `mazd//module-dir`).  For each file, it looks for
`;;;###autoload` cookies and collects them into a unified autoload file
defined by `mazd//autoload-file`.

Logs progress with `mazd//log` and writes the resulting autoloads file to
disk, overwriting the old one."
  (interactive)
  (require 'autoload)
  (mazd//log "Generating autoloads...")
  (with-temp-buffer
    (dolist (module mazd//modules)
      (let ((dir (expand-file-name module mazd//modules-dir)))
        (dolist (file (directory-files-recursively dir "\\.el$"))
          (let ((generated-autoload-file mazd//autoload-file))
            (update-file-autoloads file t)))))
    (write-region (point-min) (point-max) mazd//autoload-file)))

(leader
 "m" '(:ignore t :which-key "Mazd")
 "mg" 'mazd//autoload-generate)

(provide 'mazd-func)
;;; mazd//func.el ends here
