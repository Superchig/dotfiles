(menu-bar-mode -1)
(tool-bar-mode -1)
;; (toggle-scroll-bar -1)
(blink-cursor-mode -1)

(global-display-line-numbers-mode 1)
(global-hl-line-mode)
(electric-pair-mode 1)

(setq mac-command-modifier 'meta)

;; (ido-mode 1)

(defun insert-available-fonts ()
  "Insert a list of available fonts after the cursor, starting with a newline."
  (interactive)
  (let ((sorted-fonts (sort
		     (font-family-list)
		     #'string<)))
  (insert "\n")
  (dolist (font sorted-fonts)
    (insert font)
    (insert "\n"))))

(cond ((eq system-type 'darwin) (set-frame-font "MesloLGM Nerd Font Mono 14"))
      ((eq system-type 'gnu/linux (set-frame-font "Iosevka Nerd Font Mono 11"))))

(setq scroll-preserve-screen-position t)
(setq help-window-select t)

;; You can update the GNU ELPA keyring with `package-install gnu-elpa-keyring-update'
;; https://lists.gnu.org/archive/html/emacs-devel/2024-06/msg01157.html

(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package ace-window
  :ensure t
  :config
  (keymap-global-set "M-o" 'ace-window))

(use-package keycast
  :ensure t
  :config
  (keycast-tab-bar-mode 1))

(use-package magit
  :ensure t
  :config
  (transient-bind-q-to-quit))

(use-package paredit
  :ensure t
  :config
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  
  (keymap-set paredit-mode-map "C-<left>" 'paredit-backward-slurp-sexp)
  (keymap-set paredit-mode-map "C-<right>" 'paredit-forward-slurp-sexp)
  (keymap-set paredit-mode-map "C-M-<left>" 'paredit-backward-barf-sexp)
  (keymap-set paredit-mode-map "C-M-<right>" 'paredit-forward-barf-sexp))

(use-package sly
  :ensure t
  :config

  (defun my/sly-save-and-compile ()
    (interactive)
    (save-buffer)
    (sly-compile-defun))
  
  (keymap-set sly-editing-mode-map "C-c C-c" 'my/sly-save-and-compile)
  (setq sly-lisp-implementations '((sbcl ("/usr/bin/sbcl"))))
  (setq sly-description-autofocus t))

(use-package company
  :ensure t
  :config
  (global-company-mode 1))

;; (defun scroll-half-page-down ()
;;   "Scroll down half the page."
;;   (interactive)
;;   (scroll-down (/ (window-text-height) 2)p))

;; (defun scroll-half-page-up ()
;;   "Scroll up half the page."
;;   (interactive)
;;   (scroll-up (/ (window-text-height) 2)))

;; (global-set-key (kbd "C-v") 'scroll-half-page-up)
;; (global-set-key (kbd "M-v") 'scroll-half-page-down)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(keymap-set help-mode-map "b" 'help-go-back)
(keymap-set help-mode-map "f" 'help-go-forward)

;; (add-hook 'prog-mode-hook
;; 	  (lambda ()
;; 	    (setq require-final-newline t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1" "2b20b4633721cc23869499012a69894293d49e147feeb833663fdc968f240873" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" default))
 '(package-selected-packages
   '(magit keycast paredit ace-window gnu-elpa-keyring-update vertico company sly doom-themes which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
