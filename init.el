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

;; setup
(use-package emacs
  :ensure nil
  :init
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  ;; (window-divider-mode 1)
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
	auto-save-file-name-transforms `((".*" ,(user-emacs-directory-expand "auto-save")))
	switch-to-buffer-obey-display-actions t)
  (fset 'yes-or-no-p 'y-or-n-p)
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  (enable-recursive-minibuffers t)
  (tab-always-complete 'complete)
  (help-at-pt-display-when-idle t)
  :bind (("C-c U" . #'insert-char)
	 ("C-c e" . #'bx-sa1/open-init-file))
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
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t)

(use-package dired
  :ensure nil
  :config
  (setq dired-kill-when-opening-new-dired-buffer t))

(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer))
  :config
  (setq ibuffer-saved-filter-groups
	'(("home"
	   ("Emacs" (and (filename         . "config/emacs*")
			 (visiting-file)))
	   ("Prog"  (and (filename         . "/home/me/prog*")
			 (visiting-file)))
	   ("Org"   (or  (file-extension    . "org")
			 (derived-mode      . org-mode)
			 (derived-mode      . org-agenda-mode)))
	   ("Mail"  (or  (derived-mode      . rmail-mode)
			 (derived-mode      . message-mode)))
	   ("Gnus"  (or  (derived-mode      . gnus-mode)
			 (saved             . "gnus")))
	   ("Net"   (or  (mode              . eww-mode)
			 (derived-mode      . rcirc-mode)
			 (mode              . elpher-mode)))
	   ("Music"      (name              . "*MPC"))
	   ("Dired" (or  (derived-mode      . dired-mode)
			 (derived-mode      . image-mode)))
	   ("Proc"       (process))
	   ("Stars"      (starred-name)))))
  :hook ((ibuffer-mode . (lambda ()
			   (ibuffer-switch-to-saved-filter-groups "home"))))

  )

(use-package ibuffer-sidebar
  :ensure t
  :bind (("C-c b" . ibuffer-sidebar-toggle-sidebar))
  :commands (ibuffer-sidebar-toggle-sidebar))

(use-package dired-sidebar
  :ensure t
  :bind (("C-c d" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'nerd-icons)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :config
  (setq completion-in-region-function #'consult-completion-in-region))

(use-package cape
  :ensure t
  :bind ("C-c p" . cape-prefix-map)
  :hook ((completion-at-point-functions . cape-dabbrev)
	 (completion-at-point-functions . cape-file)))

(use-package eglot
  :ensure nil
  :hook (prog-mode . eglot-ensure)
  :bind (:map eglot-mode-map
	      ("C-c r n" . #'eglot-rename)
	      ("C-c r a" . #'eglot-code-actions)
	      ("C-c r f" . #'eglot-format)))

(use-package dape
  :ensure t
  :hook
  (kill-emacs . dape-breakpoint-save)
  (after-init . dape-breakpoint-load)
  :custom
  ;; Turn on global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode +1)

  ;; Info buffers to the right
  ;; (dape-buffer-window-arrangement 'right)
  ;; Info buffers like gud (gdb-mi)
  ;; (dape-buffer-window-arrangement 'gud)
  ;; (dape-info-hide-mode-line nil)

  ;; Projectile users
  ;; (dape-cwd-function #'projectile-project-root)
  )

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

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-undo-system 'undo-tree)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :init (evil-collection-init))

(use-package avy
  :ensure t
  :bind ("M-g c" . avy-goto-char))

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-history-directory-alist `(("." . ,(user-emacs-directory-expand "undo"))))
  (global-undo-tree-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package base16-theme
  :ensure t
  :config
  (setq base16-theme-256-color-source 'colors)
  (load-theme 'base16-custom t)) ;;custom theme in lisp/base16-wal-theme.el

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package dashboard
  :ensure t
  :config
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))

(use-package transient
  :ensure t)

(use-package ligature
  :ensure t
  :init
  ;; Enable ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  :config
  (global-ligature-mode 't))

(use-package magit
  :after transient
  :ensure t)

;; langs
(use-package markdown-mode
  :ensure t
  :magic "\\.md\\'")

(use-package rust-mode
  :defer t
  :ensure t
  :init
  (setq rust-mode-treesitter-derive t))

(use-package haskell-mode
  :defer t
  :ensure t)

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
  :defer t
  :mode "\\.xtm\\'"
  :init
  (setq extempore-path "/path/to/extempore/"))

(use-package meson-mode
  :defer t
  :ensure t)

(use-package slime
  :ensure t
  :hook (lisp-mode . slime-mode)
  :init
  (setq inferior-lisp-program "sbcl"))

(use-package lua-mode
  :defer t
  :ensure t)

(use-package sfz-mode
  :defer t
  :ensure t)

;; custom
(setq custom-file (user-emacs-directory-expand "custom.el"))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

;;buffer layout
