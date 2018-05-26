;; path to mu4e
(add-to-list 'load-path "/home/chris/mu/mu4e")

;; smtp
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials
      '(("imap.gmail.com" 587 nil nil))
      smtpmail-default-smtp-server "imap.gmail.com"
      smtpmail-smtp-server "imap.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

;; mu4e
(require 'mu4e)

(setq mu4e-maildir (expand-file-name "~/Maildir/gmail"))
(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-sent-folder   "/Sent Items")
(setq mu4e-trash-folder  "/Trash")
(setq message-signature-file "~/.emacs.d/.signature")

; get mail
(setq mu4e-get-mail-command "mbsync -c ~/.mbsyncrc gmail"
      mu4e-html2text-command "w3m -T text/html"
      mu4e-update-interval 120
      mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include nil)

(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
         ("/Sent Items"   . ?s)
         ("/Trash"       . ?t)
         ("/Drafts"    . ?d)))

;; show images
(setq mu4e-show-images t)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; general emacs mail settings; used when composing e-mail
;; the non-mu4e-* stuff is inherited from emacs/message-mode
(setq mu4e-reply-to-address "christoffer.samuelsson222@gmail.com"
    user-mail-address "christoffer.samuelsson222@gmail.com"
    user-full-name  "Christoffer Samuelsson")

;; spell check
(add-hook 'mu4e-compose-mode-hook
        (defun my-do-compose-stuff ()
           "My settings for message composition."
           (set-fill-column 72)
           (flyspell-mode)))
