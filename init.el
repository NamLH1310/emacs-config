(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path. If the new path's directories does not exist, create them."
  (let* (
        (backupRootDir "~/.emacs.d/emacs-backup/")
        (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, ➢ for example: “C:”
        (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") ))
        )
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath
  )
)

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


(use-package emacs
  :init
  (setq inhibit-startup-screen t
	gc-cons-threshold 100000000
	straight-use-package-by-default t
	make-backup-file-name-function 'my-backup-file-name
	enable-recursive-minibuffers t
	modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-region '(bg-only no-extend))
  (set-face-attribute 'default nil :height 150)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (hl-line-mode 1)
  (electric-pair-mode 1)
  (define-key minibuffer-local-map [escape] 'keyboard-escape-quit)
  (define-key minibuffer-local-ns-map [escape] 'keyboard-escape-quit)
  (define-key minibuffer-local-completion-map [escape] 'keyboard-escape-quit)
  (define-key minibuffer-local-must-match-map [escape] 'keyboard-escape-quit)
  (define-key minibuffer-local-isearch-map [escape] 'keyboard-escape-quit)
  :hook
  (emacs-lisp-mode . (lambda ()
		       (prettify-symbols-mode 1)))
  :config
  (load-theme 'modus-operandi))

(add-hook 'after-init-hook 'global-company-mode)

(use-package evil
  :init
  (setq evil-want-C-i-jump nil
	evil-want-C-u-scroll t
	evil-want-keybinding nil)
  :config
  (evil-define-key '(normal visual) 'global (kbd "SPC :") 'execute-extended-command)
  (evil-define-key 'normal 'global (kbd "SPC b l") 'consult-buffer)
  (evil-define-key 'normal 'global (kbd "SPC SPC") 'affe-find)
  (evil-define-key 'normal 'global (kbd "SPC b k") '(lambda () (interactive) (set-buffer-modified-p nil) (kill-buffer nil)))
  (evil-define-key 'normal 'global (kbd "SPC `") '(lambda () (interactive) (switch-to-buffer nil)))
  (evil-define-key 'normal 'global (kbd "SPC d") '(lambda () (interactive) (dired nil)))
  (evil-define-key 'normal 'projectile-mode-map (kbd "SPC p") 'projectile-command-map)
  (evil-define-key 'normal 'lsp-mode (kbd "g d") 'lsp-find-definition)
  (evil-define-key 'visual 'global (kbd "g c") 'comment-region)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-org
  :after (evil org)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme '(navigation
					insert
					textobjects
					additional
					calendar)))))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"
	      markdown-header-scaling t))

(use-package doom-modeline
  :config
  (doom-modeline-mode 1))

(use-package all-the-icons)
(use-package go-mode
  :hook
  (go-mode . (lambda ()
	       (setq tab-width 4)
	       (push '(">=" . ?≥) prettify-symbols-alist)
	       (push '("<=" . ?≤) prettify-symbols-alist)
	       (push '("!=" . ?≠) prettify-symbols-alist)
	       (prettify-symbols-mode 1))))

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode 1))

(use-package tree-sitter-langs
  :requires tree-sitter)

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-resize t
	vertico-cycle t))


(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult)

(use-package savehist
  :init
  (savehist-mode))

(use-package command-log-mode
  :hook command-log-mode . (lambda () (interactive) (clm/toggle-command-log-buffer)))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package gotest)

(use-package projectile
  :config
  (projectile-mode 1))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((go-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package company)

(defun affe-orderless-regexp-compiler (input _type _ignorecase)
  (setq input (orderless-pattern-compiler input))
  (cons input (lambda (str) (orderless--highlight input str))))

(use-package affe)

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package lsp-metals
  :ensure t
  :custom
  ;; Metals claims to support range formatting by default but it supports range
  ;; formatting of multiline strings only. You might want to disable it so that
  ;; emacs can use indentation provided by scala-mode.
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"))
  :hook (scala-mode . lsp))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "JAVA_HOME"))

(use-package consult-lsp)

(custom-set-variables)

(custom-set-faces
 '(markdown-header-face-1 ((t (:inherit modus-themes-heading-1 :height 1.8))))
 '(markdown-header-face-2 ((t (:inherit modus-themes-heading-2 :height 1.5))))
 '(markdown-header-face-3 ((t (:inherit modus-themes-heading-3 :height 1.2)))))
