(menu-bar-mode -1)
(tool-bar-mode -1)
;; (toggle-scroll-bar -1)
(blink-cursor-mode -1)

(global-display-line-numbers-mode 1)
(electric-pair-mode 1)

(recentf-mode 1)

(setq ring-bell-function 'ignore)

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

;; On macOS, you can also use change Emacs' font rendering with:
;; `defaults write org.gnu.Emacs AppleFontSmoothing -int 0'
;; `defaults delete org.gnu.Emacs AppleFontSmoothing'
;; Remember to restart
(cond ((eq system-type 'darwin) (set-frame-font "MesloLGM Nerd Font Mono 14"))
      ((eq system-type 'gnu/linux) (set-frame-font "Iosevka Nerd Font Mono 11")))

(setq scroll-preserve-screen-position t)
(setq help-window-select t)
(setq custom-safe-themes t)

(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'text-mode-hook 'hl-line-mode)

(defun my/term-mode-hook ()
  (display-line-numbers-mode -1)
  (hl-line-mode -1))
(add-hook 'term-mode-hook 'my/term-mode-hook)

(defun install-straight-el ()
  (interactive)
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
    (load bootstrap-file nil 'nomessage)))

(install-straight-el)
(setq straight-use-package-by-default t)

(use-package doom-themes
  :ensure t
  :config

  (defun disable-all-themes ()
    "Load all currently loaded themes."
    (interactive)
    (dolist (theme custom-enabled-themes)
      (disable-theme theme)))

  (defun switch-theme (theme)
    "Disable all existing themes and then load a new theme."
    (interactive
     (list
      (intern (completing-read "Load custom theme: "
                               (mapcar #'symbol-name
				       (custom-available-themes))))))
    (disable-all-themes)
    (load-theme theme t))
  
  (cond ((eq system-type 'darwin) (print "No theme set in macOS"))
	((eq system-type 'gnu/linux) (load-theme 'doom-one t))))

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

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package company
  :ensure t
  :config
  (global-company-mode 1))

(use-package paredit
  :ensure t
  :config
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  
  (keymap-set paredit-mode-map "C-<left>" 'paredit-backward-slurp-sexp)
  (keymap-set paredit-mode-map "C-<right>" 'paredit-forward-slurp-sexp)
  (keymap-set paredit-mode-map "C-M-<left>" 'paredit-backward-barf-sexp)
  (keymap-set paredit-mode-map "C-M-<right>" 'paredit-forward-barf-sexp))

(use-package sly
  :ensure t
  :config

  (add-to-list 'exec-path
	       (format "%s/.roswell/bin" (getenv "HOME")))

  (defun add-to-path (dir)
    (add-to-list 'exec-path dir)
    (setenv "PATH"
	    (format "%s:%s" (getenv "PATH") dir)))

  (when (eq system-type 'darwin)
    (add-to-path "/opt/homebrew/bin"))
  
  (defun my/sly-save-and-compile ()
    (interactive)
    (save-buffer)
    (sly-compile-defun))
  
  (keymap-set sly-editing-mode-map "C-c C-c" 'my/sly-save-and-compile)
  (setq sly-lisp-implementations
	'((qlot-ros-sbcl ("qlot" "exec" "ros" "run" "--" "--dynamic-space-size" "4Gb"))))
  (setq sly-description-autofocus t))

(use-package geiser
  :ensure t)

(use-package geiser-guile
  :ensure t
  :after geiser)

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
