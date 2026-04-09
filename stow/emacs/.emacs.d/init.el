(menu-bar-mode -1)
(tool-bar-mode -1)
;; (toggle-scroll-bar -1)
(blink-cursor-mode -1)

(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))

(global-display-line-numbers-mode 1)
(electric-pair-mode 1)

(recentf-mode 1)

(setq ring-bell-function 'ignore)

(setq mac-command-modifier 'meta)

(setq scroll-conservatively 10000)
(setq scroll-step 1)

(setq inhibit-startup-screen t)

(keymap-global-set "C-c i"
		   (lambda ()
		     (interactive)
		     (find-file "~/.emacs.d/init.el")))

(setq Info-default-directory-list '("~/.emacs.d/info"))

(use-package org
  :config
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-agenda-files (list (concat org-directory "/agenda/")
			       org-default-notes-file)))


(keymap-global-set "C-c c" 'org-capture)

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

(defvar my-monospace-font
  ;; On macOS, you can also use change Emacs' font rendering with:
  ;; `defaults write org.gnu.Emacs AppleFontSmoothing -int 0'
  ;; `defaults delete org.gnu.Emacs AppleFontSmoothing'
  ;; Remember to restart
  (cond
   ;; ((eq system-type 'darwin) "MesloLGM Nerd Font Mono 14")
   ((eq system-type 'darwin) "Iosevka Nerd Font Mono 16")
   ((eq system-type 'gnu/linux) "Iosevka Nerd Font Mono 12")))

(set-frame-font my-monospace-font)

(setq scroll-preserve-screen-position t)
(setq help-window-select t)
(setq custom-safe-themes t)

(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'text-mode-hook 'hl-line-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

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

  (defvar my-emacs-theme
    (cond ((eq system-type 'darwin) 'doom-one)
	  ((eq system-type 'gnu/linux) 'doom-one)))

  (defvar my-emacs-evil-theme 'doom-badger)

  (load-theme my-emacs-theme))

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package hotfuzz
  :ensure t
  :config
  (setq completion-styles '(hotfuzz)))

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

(use-package forge
  :ensure t
  :after magit cond-let)

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
  (when (locate-file "sbcl" exec-path)
    (add-to-list 'sly-lisp-implementations
		 '(sbcl ("sbcl"))))
  (when (locate-file "qlot" exec-path)
    (add-to-list 'sly-lisp-implementations
		 '(qlot-ros-sbcl ("qlot" "exec" "ros" "run" "--" "--dynamic-space-size" "4Gb"))))
  (setq sly-description-autofocus t))

(use-package geiser
  :ensure t)

(use-package geiser-guile
  :ensure t
  :after geiser)

(use-package geiser-chez
  :ensure t
  :after geiser)

(use-package sicp
  :ensure t)

(use-package elfeed
  :ensure t
  :config
  ;; https://campaignwiki.org/osr/
  ;; https://rootr.ing/
  ;; https://www.newskeeper.io/tools/youtube-rss
  (setq elfeed-feeds
	'("https://beyondfomalhaut.blogspot.com/feeds/posts/default"
	  "https://arbiterofworlds.substack.com/feed"

	  "https://alldeadgenerations.blogspot.com/feeds/posts/default"
	  "https://www.bastionland.com/rss.xml"
	  "https://widdershinswanderings.bearblog.dev/feed/"
	  "https://www.prismaticwasteland.com/?format=rss"

	  "https://goblinpunch.blogspot.com/feeds/posts/default"
	  "https://grognardia.blogspot.com/feeds/posts/default"
	  "https://dreamingdragonslayer.wordpress.com/feed"))

  ;; (set-face-font 'shr-text my-monospace-font)
  ;; (set-face-font 'shr-h3 "Times New Roman")

  (setq browse-url-browser-function 'browse-url-generic)
  (setq browse-url-generic-program (cond ((eq system-type 'darwin) "waterfox")
					 ((eq system-type 'gnu/linux) "firefox")))

  (keymap-set elfeed-search-mode-map "<return>" 'my-elfeed-open-html-in-browser)
  (keymap-set elfeed-search-mode-map "C-<return>" 'elfeed-search-show-entry)

  (setq elfeed-db-directory "/usr/local/mnt/Ventoy/elfeed/")

  ;; The filter should by default show all articles
  (setq elfeed-search-filter "")

  (defvar my-elfeed-temporary-files nil)

  (defun my-elfeed-rm-temporary-files ()
    (interactive)
    "Remove all temporary files described in `my-elfeed-temporary-files'"
    (while-let ((file (pop my-elfeed-temporary-files)))
      (delete-file file)))

  (defun my-elfeed-html-template (body title)
    (concat
     "<!DOCTYPE html>"
     "<html lang=\"en-US\">"
     
     "<head>"
     "<meta charset=\"utf-8\" />"
     "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
     "<title>"
     title
     "</title>"
     "
<style>
body {
  max-width: 80ch;
  margin-left: auto;
  margin-right: auto;

  // line-height: 1.5rem;
}
</style>
"
     "</head>"

     "<body>"
     body
     "</body>"
     
     "</html>"))

  (add-hook 'kill-emacs-hook 'my-elfeed-rm-temporary-files)

  (defun my-elfeed-open-html-in-browser (entry)
    "Open this Elfeed HTML article in your browser."
    (interactive (list (elfeed-search-selected :ignore-region)))
    (when (elfeed-entry-p entry)
      (with-temp-buffer	
	(let* ((sha (elfeed-ref-id (elfeed-entry-content entry)))
	       (first-two (substring sha 0 2))
	       (html-body (elfeed-slurp
			   (concat elfeed-db-directory "data/" first-two "/" sha)))
	       (_ (insert (my-elfeed-html-template html-body
						   (elfeed-entry-title entry))))
	       (tmp-file-name
		(make-temp-file "article" nil ".html" (buffer-string)))
	       (_ (add-to-list 'my-elfeed-temporary-files tmp-file-name))
	       (_ (browse-url tmp-file-name))))))))

(use-package minimail
  :ensure t
  :config
  (setq mail-user-agent 'minimail)

  (defvar my-minimail-accounts-file
    (concat (file-name-directory user-init-file)
	    "minimail-accounts.el"))

  (if (file-exists-p my-minimail-accounts-file)
      (setq minimail-accounts
	    (with-temp-buffer
	      (insert-file-contents my-minimail-accounts-file)
	      (let (($contents (buffer-string)))
		(eval (car (read-from-string $contents)))))))
  
  ;; https://www.emoses.org/posts/emacs-custom-auth-source/
  ;; (setq minimail-accounts
  ;; 	'((gmail ;; This can be any symbol you like to identify the account
  ;;        :mail-address "somebody@gmail.com"
  ;;        :incoming-url "imaps://imap.gmail.com"
  ;;        :outgoing-url "smtps://smtp.gmail.com")
  ;;       (work ;; Assuming Evil Corp. uses "Google Workspace" as email provider
  ;;        :mail-address "webmaster@evilcorp.com"
  ;;        :incoming-url "imaps://imap.gmail.com"
  ;;        :outgoing-url "smtps://smtp.gmail.com"
  ;;        :signature (file "~/work/.signature"))
  ;;       (uni
  ;;        :mail-address "somebody@math.niceuni.edu"
  ;;        ;; Include a username in the server URLs if it doesn't match
  ;;        ;; your email address.
  ;;        ;; Use `imap' and `smtp' as URL scheme if your server only
  ;;        ;; supports STARTTLS.
  ;;        :incoming-url "imap://username@imap.niceuni.edu"
  ;;        :outgoing-url "smtp://username@smtp.niceuni.edu")))
  )

(use-package evil
  :ensure t
  :after doom-themes
  :config

  (add-hook 'evil-mode-hook 'my-evil-mode-hook)

  (defun my-evil-mode-hook ()
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (if evil-mode
	(load-theme my-emacs-evil-theme t)
      (load-theme my-emacs-theme t)))

  (defun enable-evil-mode ()
    (interactive)
    (evil-mode 1))

  (defun disable-evil-mode ()
    (interactive)
    (evil-mode -1))
  
  (keymap-global-set "C-c v" 'enable-evil-mode)
   
  (keymap-global-set "C-c e" 'disable-evil-mode))

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))

(use-package ghostel
  :ensure t)

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
