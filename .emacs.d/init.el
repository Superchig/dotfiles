;; Set up all configuration that doesn't require packages

(global-set-key "\C-v" 'View-scroll-half-page-forward)
(global-set-key "\M-v" 'View-scroll-half-page-backward)
(global-set-key (kbd "C-h C-f") 'find-function)
;; (global-set-key [f7] 'ee)

(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")

(global-display-line-numbers-mode)
(global-hl-line-mode)
(setq blink-cursor-mode nil)
(setq column-number-mode t)

(setq vc-follow-symlinks t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(add-to-list 'default-frame-alist
	     '(font . "Iosevka 12")
	     '(font . "Inconsolata 12"))

(defun ee ()
  "Edit the Emacs configuration file."
  (interactive)
  (find-file user-init-file))

;; Set-up packages with straight.el, starting with bootstrap installation

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

(setq package-enable-at-startup nil)

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package smartparens
  :init
  (setq sp-base-key-bindings 'sp)
  :config
  (add-hook 'prog-mode-hook #'smartparens-mode)
  (require 'smartparens-config))

(use-package counsel)

(use-package swiper)

(use-package ivy
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package magit)

(use-package company
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (with-eval-after-load 'company
    (define-key company-mode-map (kbd "<tab>") #'company-indent-or-complete-common)
    (define-key company-active-map (kbd "M-/") #'company-complete)
    (define-key company-active-map
      (kbd "TAB")
      #'company-complete-common-or-cycle)
    (define-key company-active-map
      (kbd "<backtab>")
      (lambda ()
	(interactive)
	(company-complete-common-or-cycle -1)))
    (define-key company-active-map (kbd "M-.") #'company-show-location)
    (define-key company-active-map (kbd "RET") nil)))

(defun call-prog-mode-hook ()
  (interactive)
  (dolist (hook prog-mode-hook) (funcall hook)))

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (add-hook 'slime-repl-mode-hook 'call-prog-mode-hook))

(use-package slime-company
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy
	slile-company-after-completion 'slime-company-just-one-space))

; TODO(Chris): Install (and configure?) the lsp-mode package
