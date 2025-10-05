;;; -*- lexical-binding: t; -*-
(defun bx-sa1/open-init-file ()
  "Open this very file."
  (interactive)
  (find-file "~/.config/emacs/init.el"))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; bootstrap straight.el
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(require 'use-package)

;; defaults
(require 'sensible-defaults)
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

;; setup
(use-package emacs
  :straight nil
  :init
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (electric-pair-mode 1)
  (icomplete-vertical-mode 1)
  (fido-mode 1)
  (global-completion-preview-mode 1)
  (setq gc-cons-threshold 100000000
	read-process-output-max (* 1024 1024)) ; 1mb)
  (advice-add 'completion-at-point :after #'minibuffer-hide-completions)
  :custom
  (icomplete-in-buffer t)
  :bind (("C-c ;" . #'execute-extended-command)
	 ("C-c U" . #'insert-char)
	 ("C-c e" . #'bx-sa1/open-init-file))
  :hook ((prog-mode-hook . display-line-numbers-mode)))

;; packages
(use-package eglot
  :straight nil
  :hook (prog-mode . eglot-ensure)
  :bind (:map eglot-mode-map
	      ("C-c r n" . #'eglot-rename)
	      ("C-c r a" . #'eglot-code-actions)
	      ("C-c r f" . #'eglot-format)))

(use-package flymake
  :straight nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-diagnostics-buffer)))

(use-package eldoc
  :straight nil
  :init
  (global-eldoc-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package meow
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
   '("?" . meow-cheatsheet))
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
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
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

(use-package which-key
  :config
  (which-key-mode))

(use-package ewal
  :init (setq ewal-use-built-in-always-p nil))

(use-package ewal-spacemacs-themes
  :init (progn
          (show-paren-mode +1)
          (global-hl-line-mode))
  :config (progn
            (load-theme 'ewal-spacemacs-modern t)
            (enable-theme 'ewal-spacemacs-modern)))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package sudo-edit)

;; langs
(use-package markdown-mode
  :magic "\\.md\\'")

(use-package rust-mode
  :defer t
  :init
  (setq rust-mode-treesitter-derive t))

(use-package gdscript-mode
  :defer t
  :straight (gdscript-mode
             :type git
             :host github
             :repo "godotengine/emacs-gdscript-mode")
  :custom
  (gdscript-eglot-version 4.5))


;; custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
