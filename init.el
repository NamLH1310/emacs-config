(defun enhanced-scroll-up ()
  (interactive)
  (scroll-up)
  (move-to-window-line nil))

(defun enhanced-scroll-down ()
  (interactive)
  (scroll-down)
  (move-to-window-line nil))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  ;; (meow-define-keys 'insert '("C-[" . meow-insert-exit))
  (meow-motion-overwrite-define-key '("j" . meow-next)
				    '("k" . meow-prev)
				    '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("k" . enhanced-scroll-down)
   '("j" . enhanced-scroll-up)
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
  (meow-normal-define-key '("0" . meow-expand-0)
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

(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path. If the new path's directories does not exist, create them."
  (let* (
        (backupRootDir "~/.emacs.d/emacs-backup/")
        (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, ➢ for example: “C:”
        (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))

(defun newline-under-cursor ()
  "1. move to end of the line.
  2. insert newline with index"
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package
  emacs
  :init (cond ((eq system-type 'darwin)
               ;; MacOS-specific code goes here.
	       (set-face-attribute 'default nil :height 150))
	      ((eq system-type 'gnu/linux)
               ;; Linux-specific code goes here.
	       (set-face-attribute 'default nil
				   :family "DejaVu Sans Mono"
				   :height 140)))
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (electric-pair-mode)
  (hl-line-mode)
  (setq inhibit-startup-screen t gc-cons-threshold 100000000 straight-use-package-by-default t
	make-backup-file-name-function 'my-backup-file-name enable-recursive-minibuffers t
	next-line-add-newlines t visible-bell nil custom-safe-themes t ring-bell-function 'ignore))

(use-package
  general
  :config (general-define-key "<C-return>" 'newline-under-cursor))

(use-package
    doom-modeline
  :config (doom-modeline-mode))

(use-package
    rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package 
  which-key 
  :config (which-key-mode))

(use-package
  all-the-icons)

(use-package
  move-text
  :config (move-text-default-bindings))

(use-package
  company
  :hook (after-init . global-company-mode))

(use-package
  projectile
  :after general
  :config (general-def projectile-mode-map "C-c p" 'projectile-command-map)
  (projectile-mode))

(use-package
  yasnippet
  :config (yas-reload-all)
  :hook (prog-mode . yas-minor-mode))

(use-package
  all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package
  multiple-cursors
  :hook (prog-mode . (lambda ()
		       (local-set-key (kbd "C-c m") 'mc/edit-lines))))

(use-package
  vterm
  :bind
  ("C-c t" . vterm))

(use-package
  magit)

(use-package
    flycheck
  :hook (lsp-mode . flycheck-mode))

(use-package
  company-box
  :hook (company-mode . company-box-mode))

(use-package
  modus-themes
  :init (setq modus-themes-italic-constructs t modus-themes-bold-constructs t modus-themes-region
	      '(bg-only no-extend))
  (modus-themes-load-themes)
  :config (load-theme 'modus-operandi t nil))

(use-package lsp-mode)

(use-package markdown-mode)

(use-package elisp-format)

(use-package
  sublimity
  :straight (:host github
		   :repo "zk-phi/sublimity")
  :init (require 'sublimity)
  (require 'sublimity-scroll)
  :config (sublimity-mode))

(use-package scala-mode
   :interpreter
    ("scala" . scala-mode))

(use-package
  sbt-mode
  :commands sbt-start
  sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition 'minibuffer-complete-word 'self-insert-command
			     minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package
  lsp-ui
  :init (setq lsp-ui-sideline-enable t lsp-ui-sideline-show-diagnostics t))

(use-package
  lsp-metals
  :ensure t
  :custom (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"))
  :hook (scala-mode . lsp))

(use-package
  exec-path-from-shell
  :config (dolist (var '("JAVA_HOME"))
	    (add-to-list 'exec-path-from-shell-variables var))
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(use-package
  counsel
  :after
  general
  :init
  (setq ivy-use-virtual-buffers t ivy-count-format "(%d/%d) ")
  (general-define-key "M-x" 'counsel-M-x)
  (general-define-key :prefix "C-x"
		      "b" 'ivy-switch-buffer "C-f" 'counsel-find-file)
  (general-define-key :prefix "C-c"
		      "c" 'counsel-compile "g" 'counsel-git "u" 'counsel-unicode-char)
  (general-define-key :prefix "C-h"
		      "f" 'counsel-describe-function "v" 'counsel-describe-variable)
  ;; (global-set-key (kbd "C-c l") 'counsel-find-library)
  ;; (global-set-key (kbd "C-c i") 'counsel-info-lookup-symbol)
  ;; (global-set-key (kbd "C-c j") 'counsel-set-variable)
  ;; (global-set-key (kbd "C-c k") 'counsel-rg)
  ;; (global-set-key (kbd "C-c n") 'counsel-fzf)
  ;; (global-set-key (kbd "C-x l") 'counsel-locate)
  ;; (global-set-key (kbd "C-c J") 'counsel-file-jump)
  (global-set-key (kbd "C-S-s") 'swiper-isearch)
  :config
  (general-def minibuffer-local-map "C-r" 'counsel-minibuffer-history)
  (ivy-mode))

(use-package amx
  :config
  (amx-mode))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package go-mode
  :init
  (setq gofmt-command "goimports")
  :hook
  (go-mode . (lambda ()
	       (lsp)
	       (setq tab-width 4)
	       (push '(">=" . ?≥) prettify-symbols-alist)
	       (push '("<=" . ?≤) prettify-symbols-alist)
	       (push '("!=" . ?≠) prettify-symbols-alist)
	       (prettify-symbols-mode 1))))

(use-package yaml-mode)

(use-package web-mode
  :hook
  (web-mode . (lambda () (setq tab-width 2)))
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))

(use-package js2-mode
  :hook (js-mode . (lambda () (setq js-indent-level 2) (js2-minor-mode))))

(use-package meow
  :config
  (meow-setup)
  (meow-global-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4a288765be220b99defaaeb4c915ed783a9916e3e08f33278bf5ff56e49cbc73" default)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 (custom-set-faces '(markdown-header-face-1 ((t
					      (:inherit modus-themes-heading-1
							:height 1.8))))
		   '(markdown-header-face-2 ((t
					      (:inherit modus-themes-heading-2
							:height 1.5))))
		   '(markdown-header-face-3 ((t
					      (:inherit modus-themes-heading-3
							:height 1.2))))))
