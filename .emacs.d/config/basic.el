;;;; BASIC BUILT-IN EMACS SETTINGS

;; Line Numbers
(global-linum-mode 1)
(column-number-mode 1)

;; Add package_scripts to load-path
(add-to-list 'load-path "~/.emacs/package_scripts/")
(autoload 'lpc-mode  "lpc-mode" t)

;; MELPA setup
(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
			 '("melpa3" . "http://melpa-stable.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Save emacs sessions
(desktop-save-mode 1)

;; Smoother scrolling
(setq scroll-margin 1
	  scroll-step 1
	  scroll-conservatively 10000
	  scroll-preserve-screen-position 1)

;; Unique buffer names
(setq uniquify-buffer-name-style 'forward)

;; Prettify-Symbols-Mode
(add-hook 'global-prettify-symbols-mode-hook
		  (lambda ()
			(push '("<=" . ?≤) prettify-symbols-alist)
			(push '(">=" . ?≥) prettify-symbols-alist)
			(push '("->" . ?→) prettify-symbols-alist)
			(push '("<-" . ?←) prettify-symbols-alist)))

(global-prettify-symbols-mode)

;; Disable tool-bar-mode and scroll-bar-mode
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; Rebind C-x k so that it closes the window of the buffer it kills.
(global-set-key (kbd "C-x k") 'kill-buffer-and-window)

;; Server-start
(server-start)

;; Recognize zsh files and use zsh-flavor sh-mode
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zshrc\\'" . sh-mode))

(setq js-indent-level 2)

;; Font
;; (set-face-attribute 'default nil :font "Terminus-8")
;; (set-frame-font "Terminus-8" nil t)
