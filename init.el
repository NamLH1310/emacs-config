(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path. If the new path's directories does not exist, create them."
  (let* (
        (backupRootDir "~/.emacs.d/emacs-backup/")
        (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, ➢ for example: “C:”
        (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))

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
  :init (set-face-attribute 'default nil
			    :family "DejaVu Sans Mono"
			    :height 140)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (electric-pair-mode)
  (setq inhibit-startup-screen t gc-cons-threshold 100000000 straight-use-package-by-default t
	make-backup-file-name-function 'my-backup-file-name enable-recursive-minibuffers t
	next-line-add-newlines t visible-bell nil custom-safe-themes t ring-bell-function 'ignore)
  :config (global-set-key (kbd "C-`")
			  (lambda ()
			    (interactive)
			    (switch-to-buffer nil))))

(use-package doom-modeline
  :config
  (doom-modeline-mode))

(use-package rainbow-delimiters
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package which-key
  :config
  (which-key-mode))

(use-package all-the-icons)

(use-package move-text
  :config
  (move-text-default-bindings))

(use-package
  company
  :hook (after-init . global-company-mode))

(use-package
  projectile
  :config (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode))

(use-package
  yasnippet
  :config (yas-reload-all)
  :hook (prog-mode . yas-minor-mode))

(use-package all-the-icons-dired
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
  :init
  (setq ivy-use-virtual-buffers t ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "C-s") 'isearch-forward)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-h f") 'counsel-describe-function)
  (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  (global-set-key (kbd "C-c l") 'counsel-find-library)
  (global-set-key (kbd "C-c i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "C-c u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c j") 'counsel-set-variable)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c v") 'ivy-push-view)
  (global-set-key (kbd "C-c V") 'ivy-pop-view)
  (global-set-key (kbd "C-c c") 'counsel-compile)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c L") 'counsel-git-log)
  (global-set-key (kbd "C-c k") 'counsel-rg)
  (global-set-key (kbd "C-c n") 'counsel-fzf)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-c J") 'counsel-file-jump)
  (global-set-key (kbd "C-S-s") 'swiper-isearch)
  :config
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (ivy-mode))

(use-package general)

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
