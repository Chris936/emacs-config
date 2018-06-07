(setq inhibit-startup-message t)

;; Disable tool-bar.
(tool-bar-mode -1)

;; Melpa pack archives. Copied from melpa.org.
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package grep-a-lot
  :ensure t)

;; Grep macro - Recursivlely greps for a targeted word from 2 directories back.
(fset 'grep-targeted-word
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217826 67108896 134217830 134217847 134217848 103 114 101 112 return 25 32 46 46 47 32 45 82 return] 0 "%d")) arg)))

;; Function to load files
(defun load-if-exists (f)
  (if (file-readable-p f)
      (load-file f)))

;; Mu4e config. Comment out if email is not used within emacs.
(load-if-exists "~/.emacs.d/mu4e_config.el")

;; Custom key-map
(define-prefix-command '1-map)
(global-set-key (kbd "C-1") '1-map)

;; Custom keybindings
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x t") 'grep-targeted-word)

;; 1-map
(define-key 1-map (kbd "m") 'mu4e)

;; Org-mode
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Ace-window
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 1.7)))))
    ))

;; Ivy
(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))

;; Counsel
(use-package counsel
  :ensure t
  )

;; Swiper
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
    )

;; Avy
(use-package avy
  :ensure t
  :bind ("C-r" . avy-goto-char-2))

;; Auto-Complete
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    )
  (setq ac-auto-show-menu 0.1))
