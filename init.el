(menu-bar-mode -1) 
(toggle-scroll-bar -1) 
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(transient-mark-mode 1)
(setq backup-directory-alist '(("." . "~/.cache/emacs")))
(global-display-line-numbers-mode)

;;; theme
(when (display-graphic-p)
  (load-theme 'xresources t))

;;; Set up the package manager

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)))

;;; evil
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

;;; org-mode
(use-package org
  :ensure t
  :bind (("C-c l" . #'org-store-link)
	 ("C-c a" . #'org-agenda)
	 ("C-c c" . #'org-capture)) 
  :config
  (setq org-directory "~/Documents/notes")
  (setq org-log-done 'time))

;; which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))



















	       
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e5494adf200eeff1505839672150dde6053e086869189c381b1ce9b792dda3a8"
     default))
 '(org-agenda-files
   '("/home/baba/Documents/notes/ee3637.org"
     "/home/baba/Documents/notes/todo.org"))
 '(package-selected-packages '(evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
