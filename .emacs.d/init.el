;; -*- lexical-binding: t; -*-

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

(setq ring-bell-function 'ignore)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(add-to-list 'default-frame-alist
	     '(font . "Iosevka 12"))

(add-to-list 'default-frame-alist
	     '(font . "Inconsolata 12"))

(add-to-list 'default-frame-alist
	     '(font . "Monaco Nerd Font 12"))

;; Automatically wrap isearch
;; https://stackoverflow.com/questions/285660/automatically-wrapping-i-search
;; Prevents issue where you have to press backspace twice when
;; trying to remove the first character that fails a search
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

(setq help-window-select t)

;; https://emacs.stackexchange.com/questions/37407/how-to-check-if-my-emacsclient-is-running-in-a-gui-window
(defun my-frame-behaviors (&optional frame)
  "Make frame- and/or terminal-local changes."
  (interactive)
  (with-selected-frame (or frame (selected-frame))
    (when window-system
      (global-unset-key (kbd "C-z"))
      (global-unset-key (kbd "C-x C-z")))))

;; Run in non-daemon Emacs...
(my-frame-behaviors)
;; ...and later, for new frames / emacsclient
;; (add-hook 'after-make-frame-functions #'my-frame-behaviours)

;; https://github.com/eriksvedang/.emacs.d/blob/9ba77a88788ffd191bd64f140ad4bbe0dc0dd11d/config.org
;; Notably, this doesn't seem to tell you what face (font?) is used for
;; the secondary monospace fonts used in between = in org-mode.
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; https://emacs.stackexchange.com/questions/5371/how-to-change-emacs-windows-from-vertical-split-to-horizontal-split
(defun window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(defun ee ()
  "Edit the Emacs configuration file."
  (interactive)
  ;; (find-file user-init-file)
  (find-function 'ee))

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

(use-package gruvbox-theme)

(load-theme 'gruvbox-light-medium t)

(use-package org
  :straight (:type built-in)
  :init
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)

  (add-hook 'org-mode-hook #'auto-fill-mode)

  :config
  (define-key org-mode-map (kbd "M-n") #'org-next-visible-heading)
  (define-key org-mode-map (kbd "M-p") #'org-previous-visible-heading))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package smartparens
  :hook ((lisp-mode . smartparens-strict-mode)
	 (emacs-lisp-mode . smartparens-strict-mode))
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
  ;; (global-set-key "\C-s" 'swiper)
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

(use-package prescient
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :after (counsel ivy)
  :config
  (ivy-prescient-mode)
  (setq ivy-prescient-retain-classic-highlighting t))

(use-package company-prescient
  :after (company)
  (company-prescient-mode))

;; (use-package vertico
;;   :init
;;   (vertico-mode)

;;   (savehist-mode)

;;   ;; Add prompt indicator to `completing-read-multiple'.
;;   ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
;;   (defun crm-indicator (args)
;;     (cons (format "[CRM%s] %s"
;;                   (replace-regexp-in-string
;;                    "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
;;                    crm-separator)
;;                   (car args))
;;           (cdr args)))
;;   (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;;   ;; Do not allow the cursor in the minibuffer prompt
;;   (setq minibuffer-prompt-properties
;;         '(read-only t cursor-intangible t face minibuffer-prompt))
;;   (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;   (setq enable-recursive-minibuffers t))

;; ;; Optionally use the `orderless' completion style.
;; (use-package orderless
;;   :init
;;   ;; Configure a custom style dispatcher (see the Consult wiki)
;;   ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
;;   ;;       orderless-component-separator #'orderless-escapable-split-on-spaceave)
;;   (setq completion-styles '(orderless basic)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles partial-completion)))))

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

(use-package sly
  :hook ((sly-mrepl . smartparens-strict-mode))
  :config
  (setq inferior-lisp-program "sbcl")
  (add-hook 'sly-mrepl-hook 'call-prog-mode-hook))

(use-package racket-mode
  :hook ((racket-repl-mode . smartparens-strict-mode)
	 (racket-mode . smartparens-strict-mode)
	 (racket-mode . racket-xp-mode))
  :config
  (add-hook 'racket-repl-mode-hook 'call-prog-mode-hook)
  (defface-racket my/racket-xp-unused-face
    '((t nil))
    "Face `racket-xp-mode` can use to highlight unused requires or definitions. Manually set to show no changes."
    "Unused Face")
  (setq racket-xp-unused-face my/racket-xp-unused-face))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner 'logo)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(use-package doom-modeline
  :disabled
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-minor-modes t))

; TODO(Chris): Install (and configure?) the lsp-mode package
