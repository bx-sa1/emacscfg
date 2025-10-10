;;; -*- lexical-binding: t; -*-
(defun user-emacs-directory-expand (file)
    "Expand FILE path in user Emacs directory."
    (expand-file-name file user-emacs-directory))
(defun bx-sa1/open-init-file ()
  "Open this very file."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(add-to-list 'load-path (user-emacs-directory-expand "lisp"))
(add-to-list 'custom-theme-load-path (user-emacs-directory-expand "lisp"))

;; bootstrap elpaca
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (user-emacs-directory-expand "elpaca/"))
(defvar elpaca-builds-directory (user-emacs-directory-expand "builds/"))
(defvar elpaca-repos-directory (user-emacs-directory-expand "repos/"))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(require 'use-package)
(setq use-package-always-ensure 1)
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; defaults
(require 'sensible-defaults)
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

;; setup
(use-package emacs
  :ensure nil
  :init
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (window-divider-mode 1)
  (electric-pair-mode 1)
  (icomplete-vertical-mode 1)
  (fido-mode 1)
  (global-completion-preview-mode 1)
  (setq gc-cons-threshold 100000000
	read-process-output-max (* 1024 1024)) ; 1mb)
  (advice-add 'completion-at-point :after #'minibuffer-hide-completions)
  :custom
  (icomplete-in-buffer t)
  (help-at-pt-display-when-idle t)
  :bind (("C-c U" . #'insert-char)
	 ("C-c e" . #'bx-sa1/open-init-file)
	 ("C-c b n" . #'next-buffer)
	 ("C-c b p" . #'previous-buffer))
  :hook ((prog-mode-hook . display-line-numbers-mode)))

;; packages
(use-package eglot
  :ensure nil
  :hook (prog-mode . eglot-ensure)
  :bind (:map eglot-mode-map
	      ("C-c r n" . #'eglot-rename)
	      ("C-c r a" . #'eglot-code-actions)
	      ("C-c r f" . #'eglot-format)))

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-diagnostics-buffer)))

(use-package eldoc
  :ensure nil
  :init
  (global-eldoc-mode))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package meow
  :ensure t
  :init
  (defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("<SPC>" . meow-M-x))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . undo-tree-undo)
   '("U" . undo-tree-redo)
   '("v" . meow-visit)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))
  :config
  (meow-setup)
  (meow-global-mode 1))

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-history-directory-alist '(("." . (user-emacs-directory-expand "undo"))))
  (global-undo-tree-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-wal t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; (use-package nano-modeline
;;     :ensure t
;;     :init
;;     (nano-modeline-prog-mode t)
;;     :custom
;;     (nano-modeline-position 'nano-modeline-footer)
;;     :hook
;;     (prog-mode           . nano-modeline-prog-mode)
;;     (text-mode           . nano-modeline-text-mode)
;;     (org-mode            . nano-modeline-org-mode)
;;     (pdf-view-mode       . nano-modeline-pdf-mode)
;;     (mu4e-headers-mode   . nano-modeline-mu4e-headers-mode)
;;     (mu4e-view-mode      . nano-modeline-mu4e-message-mode)
;;     (elfeed-show-mode    . nano-modeline-elfeed-entry-mode)
;;     (elfeed-search-mode  . nano-modeline-elfeed-search-mode)
;;     (term-mode           . nano-modeline-term-mode)
;;     (xwidget-webkit-mode . nano-modeline-xwidget-mode)
;;     (messages-buffer-mode . nano-modeline-message-mode)
;;     (org-capture-mode    . nano-modeline-org-capture-mode)
;;     (org-agenda-mode     . nano-modeline-org-agenda-mode))

(use-package sudo-edit :ensure t)

(use-package dashboard
  :ensure t
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))

;; langs
(use-package markdown-mode
  :ensure t
  :magic "\\.md\\'")

(use-package rust-mode
  :defer t
  :ensure t
  :init
  (setq rust-mode-treesitter-derive t))

(use-package gdscript-mode
  :defer t
  :ensure (gdscript-mode
             :host github
             :repo "godotengine/emacs-gdscript-mode")
  :custom
  (gdscript-eglot-version 4.5))

(use-package sclang
  :defer t
  :ensure (sclang
	     :host github
	     :repo "supercollider/scel"
	     :files (:default "el/*.el")))

(use-package sclang-extensions
  :after sclang
  :defer t
  :ensure t
  :hook (sclang-mode-hook . sclang-extensions-mode))

(use-package csound-mode
  :ensure (csound-mode
	   :host github
	   :repo "hlolli/csound-mode")
  :mode (("\\.csd\\'" . csound-mode)
	 ("\\.orc\\'" . csound-mode)
	 ("\\.sco\\'" . csound-mode)
	 ("\\.udo\\'" . csound-mode)))

(use-package extempore-mode
  :ensure t
  :defer t)

(use-package meson-mode
  :defer t
  :ensure t)

(use-package slime
  :ensure t
  :hook (lisp-mode . slime-mode)
  :init
  (setq inferior-lisp-program "sbcl"))

;; custom
(setq custom-file (user-emacs-directory-expand "custom.el"))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))
