;;; configs.el --- Mail -*- lexical-binding: t; -*-

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

;;; mu4e
(defun mu4e-init()
  (interactive)
  (require 'mu4e)
  (mu4e))

(with-eval-after-load 'mu4e
  (setq mu4e-maildir "~/.local/share/mail"
	mu4e-get-mail-command "mbsync -a --verbose"
	mu4e-update-interval 300
	mu4e-view-show-images t
	mu4e-view-show-addresses t
	mu4e-enable-notifications nil
	mu4e-enable-mode-line t
	mu4e-headers-skip-duplicates t
	mu4e-change-filenames-when-moving t
	mu4e-view-image-max-width 800
	mu4e-completing-read-function 'completing-read
	mu4e-use-fancy-chars 't
	mu4e-view-show-images 't
	message-kill-buffer-on-exit 't
	mu4e-maildir-shortcuts
	'(("/gmail/inbox" . ?g)
	  ("/staff/inbox" . ?s)))
  (let ((dir "~/Downloads"))
    (when (file-directory-p dir)
      (setq mu4e-attachment-dir dir)))
  (add-hook 'mu4e-compose-mode-hook
	    (lambda () (use-hard-newlines t 'guess)))
  (setq mu4e-view-actions
        '(("capture message" . mu4e-action-capture-message)
          ("view in browser" . mu4e-action-view-in-browser)
          ("show this thread" . mu4e-action-show-thread)
	  ("View as pdf" . mu4e-action-view-as-pdf)))
  ;; default send
  (setq mu4e-sent-folder "/sent"
	mu4e-drafts-folder "/drafts"
	user-mail-address "mr.katebzadeh@gmail.com"
	smtpmail-default-smtp-server "smtp.gmail.com"
	smtpmail-smtp-server "smtp.gmail.com"
	smtpmail-smtp-service 587)
  ;; list of accounts
  (defvar mu4e-account-list
    '(("gmail"
       (mu4e-sent-folder "/gmail/[Gmail].Sent Mail")
       (mu4e-drafts-folder "/gmail/[Gmail].Drafts")
       (mu4e-trash-folder "/gmail/[Gmail].Trash")
       (user-mail-address "mr.katebzadeh@gmail.com")
       (smtpmail-smtp-user "mr.katebzadeh")
       (smtpmail-local-domain "gmail.com")
       (smtpmail-default-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-server "smtp.gmail.com")
       (user-full-name "M.R. Siavash Katebzadeh")
       (smtpmail-smtp-service 587)
       )
      ("staff"
       (mu4e-sent-folder "/staff/Sent Items")
       (mu4e-drafts-folder "/staff/Drafts")
       (mu4e-trash-folder "/staff/Trash")
       (user-mail-address "m.r.katebzadeh@ed.ac.uk")
       (smtpmail-smtp-user "s1691546")
       (smtpmail-local-domain "ed.ac.uk")
       (smtpmail-default-smtp-server "smtp.staffmail.ed.ac.uk ")
       (smtpmail-smtp-server "smtp.staffmail.ed.ac.uk ")
       (user-full-name "KATEBZADEH Siavash")
       (smtpmail-smtp-service 587)
       )))
  ;; set account
  (defun mu4e-set-account ()
    (let* ((account
            (if mu4e-compose-parent-message
                (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                  (string-match "/\\(.*?\\)/" maildir)
                  (match-string 1 maildir))
              (completing-read (format "Compose with account: (%s) "
                                       (mapconcat #'(lambda (var) (car var))
                                                  mu4e-account-list "/"))
                               (mapcar #'(lambda (var) (car var)) mu4e-account-list)
                               nil t nil nil (caar mu4e-account-list))))
           (account-vars (cdr (assoc account mu4e-account-list))))
      (if account-vars
          (mapc #'(lambda (var)
                    (set (car var) (cadr var)))
                account-vars)
        (error "No email account found"))))
  (add-hook 'mu4e-compose-pre-hook 'mu4e-set-account)
  ;; msmtp
  (setq message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/usr/bin/msmtp"
        user-full-name "Siavash Katebzadeh")
  (defun choose-msmtp-account ()
    (if (message-mail-p)
        (save-excursion
          (let*
              ((from (save-restriction
                       (message-narrow-to-headers)
                       (message-fetch-field "from")))
               (account
                (cond
                 ((string-match "mr.katebzadeh@gmail.com" from) "gmail")
                 ((string-match "mrkatebzadeh.com" from) "gmail")
                 ((string-match "m.r.katebzadeh@ed.ac.uk" from) "staff")
                 ((string-match "s1691546@ed.ac.uk" from) "staff")
                 ((string-match "s1691546@staffmail.ed.ac.uk" from) "staff"))))
            (setq message-sendmail-extra-arguments (list '"-a" account))))))
  (setq message-sendmail-envelope-from 'header)
  (add-hook 'message-send-mail-hook 'choose-msmtp-account)
  ;; signature
  (defun mu4e-choose-signature ()
    "Insert one of a number of sigs"
    (interactive)
    (let ((message-signature
	   (mu4e-read-option "Signature:"
			     '(("formal" .
				(concat
				 "Best regards,\n\n"
				 "Siavash Katebzadeh\n"
				 "Student, PhD in Computer Science\n"
				 "The University of Edinburgh\n"
				 "http://mr.katebzadeh.xyz\n"))
			       ("informal" .
				"Best regards,\nSiavash\n")))))
      (message-insert-signature)))
  (add-hook 'mu4e-compose-mode-hook
	    (lambda () (local-set-key (kbd "C-c C-w") #'mu4e-choose-signature)))
  (mu4e-maildirs-extension))

;;; alert
(with-eval-after-load 'mu4e-alert
  (mu4e-alert-set-default-style 'notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))

;;; gnus-dired
(with-eval-after-load 'gnus
  (defun gnus-dired-mail-buffers ()
    (let (buffers)
      (save-current-buffer
	(dolist (buffer (buffer-list t))
	  (set-buffer buffer)
	  (when (and (derived-mode-p 'message-mode)
		     (null message-sent-message-via))
	    (push (buffer-name buffer) buffers))))
      (nreverse buffers)))
  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))

;;; org-mu4e
  (require 'org-mu4e)
  (setq org-mu4e-link-query-in-headers-mode nil)

;;; configs.el ends here
