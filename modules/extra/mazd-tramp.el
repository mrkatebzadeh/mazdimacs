;;; mazd-tramp.el --- Tramp -*- lexical-binding: t; -*-

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

(mazd//after password-cache
  (setq password-cache-expiry nil))

(mazd//after tramp-cache
  (setq tramp-persistency-file-name mazd//tramp))


(mazd//after tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-completion-use-auth-sources nil)
  ;; Define an rsyncx method analogous to scpx
  (add-to-list 'tramp-methods
               `("rsyncx"
                 (tramp-login-program "ssh")
                 (tramp-login-args
                  (("-l" "%u")
                   ("-p" "%p")
                   ("%c")
                   ("-e" "none")
                   ("-t" "-t")
                   ("-o" "RemoteCommand=\"%l\"")
                   ("%h")))
                 (tramp-async-args (("-q")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-login ("-l"))
                 (tramp-remote-shell-args ("-c"))
                 (tramp-copy-program
                  ,(cond
                    ((and (eq system-type 'darwin)
                          (file-executable-p "/opt/homebrew/bin/rsync"))
                     "/opt/homebrew/bin/rsync")
                    ((and (eq system-type 'darwin)
                          (file-executable-p "/usr/local/bin/rsync"))
                     "/usr/local/bin/rsync")
                    ((file-executable-p "/usr/bin/rsync") "/usr/bin/rsync") ;; Added support for /usr/bin/rsync
                    (t "rsync")))
                 (tramp-copy-args (("-t" "%k") ("-p") ("-r") ("-s") ("-c")))
                 (tramp-copy-env (("RSYNC_RSH") ("ssh") ("%c")))
                 (tramp-copy-keep-date t)
                 (tramp-copy-keep-tmpfile t)
                 (tramp-copy-recursive t)))
  (mazd//after consult-tramp
    (setq consult-tramp-method "rsyncx"))


  (define-advice tramp-read-passwd
      (:around (old-fun &rest args) disable-auth-sources )
    (let ((auth-sources))
      (apply old-fun args)))

  (setq tramp-backup-directory-alist `((,(rx (zero-or-more anything))
					. ,mazd//tramp-backup-directory)))

  )

;;;###autoload
(defun tramp-switch-method (method &optional file-name)
  (interactive (list
                (completing-read "Choose method: "
                                 (mapcar #'car tramp-methods))))
  (find-alternate-file
   (with-parsed-tramp-file-name (or file-name buffer-file-name) parsed
				(tramp-make-tramp-file-name
				 method
				 parsed-user
				 parsed-domain
				 parsed-host
				 parsed-port
				 parsed-localname
				 parsed-hop))))

(provide 'mazd-tramp)
;;; mazd-tramp.el ends here
