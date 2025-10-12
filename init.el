;;; -*- lexical-binding: t; -*-
(defun user-emacs-directory-expand (path)
    "Expand FILE path in user Emacs directory."
    (expand-file-name path user-emacs-directory))
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

(define-prefix-command 'ctl-z-map)
(global-set-key "\C-z" ctl-z-map)

;; setup
(use-package emacs
  :ensure nil
  :init
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (window-divider-mode 1)
  (electric-pair-mode 1)
  (transient-mark-mode 1)
  (delete-selection-mode 1)
  (show-paren-mode 1)
  (global-font-lock-mode 1)
  (global-auto-revert-mode 1)
  (global-hl-line-mode 1)
  (setq gc-cons-threshold 200000000 ;;Allow 20MB of memory before calling GC
	read-process-output-max (* 1024 1024) ;;1mb
	vc-follow-symlinks t ;;follow symlinks
	sentence-end-double-space nil ;;dont assume that sentences should have two spaces after periods
	require-final-newline t ;;append newline on file save
	confirm-kill-emacs 'y-or-n-p
	inhibit-startup-message t ;;inhibit startup message
	initial-scratch-message nil ;;clear scratch buffer
	show-paren-delay 0.0
	visible-bell t ;;flash screen on problematic operation
	backup-directory-alist `((".*" . ,(user-emacs-directory-expand "backup")))
	auto-save-file-name-transforms `((".*" ,(user-emacs-directory-expand "auto-save"))))
  (fset 'yes-or-no-p 'y-or-n-p)
  :custom
  (help-at-pt-display-when-idle t)
  :bind (("C-z U" . #'insert-char)
	 ("C-z e" . #'bx-sa1/open-init-file)
	 ("C-z b n" . #'next-buffer)
	 ("C-z b p" . #'previous-buffer))
  :hook ((prog-mode . display-line-numbers-mode)
	 (prog-mode . subword-mode)
	 (before-save . delete-trailing-whitespace)
	 (after-save . executable-make-buffer-file-executable-if-script-p)
	 (before-save . (lambda ()
              (when buffer-file-name
                (let ((dir (file-name-directory buffer-file-name)))
                  (when (and (not (file-exists-p dir))
                             (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                    (make-directory dir t))))))))

;; packages
(use-package icomplete
  :ensure nil
  :bind (:map icomplete-minibuffer-map
              ("C-n" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-v" . icomplete-vertical-toggle)
              ("RET" . icomplete-force-complete-and-exit))
  :hook
  (after-init . (lambda ()
                  (fido-mode -1)
                  ;; (icomplete-mode 1)
                  (icomplete-vertical-mode 1)
                  ))
  :config
  (setq tab-always-indent 'complete)  ;; Starts completion with TAB
  (setq icomplete-delay-completions-threshold 0)
  (setq icomplete-compute-delay 0)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-prospects-height 10)
  (setq icomplete-separator " . ")
  (setq icomplete-with-completion-tables t)
  (setq icomplete-in-buffer t)
  (setq icomplete-max-delay-chars 0)
  (setq icomplete-scroll t)
  (advice-add 'completion-at-point
              :after #'minibuffer-hide-completions))

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

(use-package god-mode
  :ensure t
  :bind (("<escape>" . god-mode-all)
	 (:map god-local-mode-map
	       ("z" . #'repeat))))

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char))

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-history-directory-alist `(("." . ,(user-emacs-directory-expand "undo"))))
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
