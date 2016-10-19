;;;; Plugins installation and configuration
;; Testing out the usage of MELPA to transfer packages, if some don't work, change package-install to el-get or
;; add a (require 'package-name)
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Company Mode activation and Autocomplete disabled
(package-install 'company)
(global-company-mode)

(global-set-key (kbd "<backtab>") 'company-complete-common)

;; Rebind C-x C-b to buffer-menu instead of list-buffers
(global-set-key "\C-x\C-b" 'buffer-menu)

;; Evil-mode activation
(package-install 'evil)
(require 'evil)

(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

;; Keychords for Evil mode rebindings
(package-install 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "kj" 'evil-normal-state)

;; Evil-Leader setup
(package-install 'evil-leader)
										; Evil-mode activated due to evil-leader
(global-evil-leader-mode)
(evil-mode 1)

(evil-leader/set-leader ",")
(evil-leader/set-key
  "]" 'find-tag
  "b" 'buffer-menu)

;; Evil-Org-Mode
(package-install 'evil-org)

;; Evil-Mode Key Translations
(defun make-conditional-key-translation (key-from key-to translate-keys-p)
  "Make a Key Translation such that if the translate-keys-p function returns true,
  key-from translates to key-to, else key-from translates to itself.  translate-keys-p
  takes key-from as an argument. "
  (define-key key-translation-map key-from
	(lambda (prompt)
	  (if (funcall translate-keys-p key-from) key-to key-from))))
(defun my-translate-keys-p (key-from)
  "Returns whether conditional key translations should be active.  See make-conditional-key-translation function. "
  (and
   ;; Only allow a non identity translation if we're beginning a Key Sequence.
   (equal key-from (this-command-keys))
   (or (evil-motion-state-p) (evil-normal-state-p) (evil-visual-state-p))))
;;(define-key evil-normal-state-map "c" nil) ;; I'm not entirely sure what this does
;;(define-key evil-motion-state-map "cu" 'universal-argument) ;; Or this one, either
;; (make-conditional-key-translation (kbd "ch") (kbd "C-h") 'my-translate-keys-p)
;; (make-conditional-key-translation (kbd "g") (kbd "C-x") 'my-translate-keys-p)')
;; (make-conditional-key-translation (kbd "C-l") (kbd "C-S-w l") 'my-translate-keys-p)
;; (make-conditional-key-translation (kbd "C-h") (kbd "C-S-w h") 'my-translate-keys-p)
;; (make-conditional-key-translation (kbd "C-j") (kbd "C-S-w j") 'my-translate-keys-p)
;; (make-conditional-key-translation (kbd "C-k") (kbd "C-S-w k") 'my-translate-keys-p)
(define-key evil-motion-state-map (kbd "M-]") 'find-tag)

;; Evil surround
(package-install 'evil-surround)
(global-evil-surround-mode 1)

;; Evil match-it
(package-install 'evil-matchit)
(global-evil-matchit-mode 1)

;; Powerline
(package-install 'powerline)
(package-install 'powerline-evil)

(powerline-evil-center-color-theme)

;; Evil-nerd-commenter
(package-install 'evil-nerd-commenter)

;; Emacs key bindings
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-c l") 'evilnc-quick-comment-or-uncomment-to-the-line)
(global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
(global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)

;; Vim key bindings
(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  "\\" 'evilnc-comment-operator ; if you prefer backslash key
  )

;; Asynchronous processing?
(package-install 'async)
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;; Helm
(package-install 'helm)

(require 'helm)

(global-set-key (kbd "M-x") 'helm-M-x)

;; Change `helm-command-prefix' to "C-c h" because "C-x c" is too close to exiting.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; Helm-gtags
(package-install 'helm-gtags)

;; Activate Org-Mode
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Electric Pair time!
(electric-pair-mode 1)

;; Show Matching Parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Projectile
(package-install 'projectile)

;; Enhanced ruby mode
(package-install 'enh-ruby-mode)

;; Ruby-end mode
(package-install 'ruby-end)
(add-hook 'enh-ruby-mode-hook 'ruby-end-mode)

;; Robe
(package-install 'robe)
(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'enh-ruby-mode-hook 'robe-mode)

(setq electric-pair-pairs '(
							(?\" . ?\")
							(?\{ . ?\})
							(?\( . ?\))
							) )

;; Minitest
(package-install 'minitest)

;; Indent-Guide
(package-install 'indent-guide)
(indent-guide-global-mode)
(setq indent-guide-recursive t)

;; Paredit
(package-install 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; Evil-Paredit
(package-install 'evil-paredit)
(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)

;; CC mode
(require 'cc-mode)

;; C indentation
(setq-default tab-width 4 indent-tabs-mode t)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;; Objective-C Mode Configuration
(add-to-list 'magic-mode-alist
			 `(,(lambda ()
				  (and (string= (file-name-extension buffer-file-name) "h")
					   (re-search-forward "@\\<interface\\>" 
										  magic-mode-regexp-match-limit t)))
			   . objc-mode))

;; Geiser
(package-install 'geiser)

;; Flycheck
(package-install 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Yasnippet
(package-install 'yasnippet)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; C Eldoc Mode
(package-install 'c-eldoc)
;; add in your commonly used packages/include directories here, for
;; example, SDL or OpenGL. this shouldn't slow down cpp, even if
;; you've got a lot of them
(setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ")
(load "c-eldoc")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; Lua!
(package-install 'lua-mode)

;; Markdown
(package-install 'markdown-mode)

;; Rust
(package-install 'rust-mode)

;; flycheck-rust
(package-install 'flycheck-rust)

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; Racer Integration
(setq racer-rust-src-path "~/Desktop/computer/rust-src/rustc-1.0.0/src/")
(setq racer-cmd "/home/chiggie/Desktop/computer/rust-src/racer/target/release/racer")
(add-to-list 'load-path "/home/chiggie/Desktop/computer/rust-src/racer/editors/emacs")
(eval-after-load "rust-mode" '(require 'racer))

;; Rainbow delimiters
(package-install 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Coffee-script
(package-install 'coffee-mode)

;; Haskell
(package-install 'haskell-mode)

(add-hook 'haskell-mode-hook 'haskell-indent-mode)

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))
(custom-set-variables '(haskell-tags-on-save t))

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))
(eval-after-load 'haskell-mode '(progn
								  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
								  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
								  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
								  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
								  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
								  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
								  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
(eval-after-load 'haskell-cabal '(progn
								   (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
								   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
								   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
								   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

;; Structured Haskell Mode
;;
;; Unfortunately, this isn't available on MELPA.
;; This is implemented with git submodules on my dotfiles repository.
;; Check https://github.com/chrisdone/structured-haskell-mode
;; and https://git-scm.com/book/en/v1/Git-Tools-Submodules
;; for more info.
;; Disabled due to lack of interest in Haskell.
;; (add-to-list 'load-path "~/dotfiles/.emacs.d/submodules/structured-haskell-mode/elisp")
;; (require 'shm)

(eval-after-load 'structured-haskell-mode '(progn
											 (define-key haskell-mode-map (kbd "SPC") 'shm/newline-indent)))

;; Semantic Refactor! Woo-hoo!
(package-install 'srefactor)
(require 'srefactor)
(require 'srefactor-lisp)

;; OPTIONAL: ADD IT ONLY IF YOU USE C/C++. 
(semantic-mode 1) ;; -> this is optional for Lisp

(define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(global-set-key (kbd "M-RET o") 'srefactor-lisp-one-line)
(global-set-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
(global-set-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
(global-set-key (kbd "M-RET b") 'srefactor-lisp-format-buffer)

;; Irony-Mode - C++ editing for Emacs
(package-install 'irony)
(package-install 'irony-eldoc)
(package-install 'company-irony)
(add-hook 'c++-mode-hook 'irony-mode)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
	'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
	'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook 'irony-eldoc)
(add-hook 'irony-mode-hook 'eldoc-mode)

(eval-after-load 'company
  '(add-to-list 'company-backend 'company-irony))

;; Custom C++ style for cc-mode
(defconst my-cc-style
  '("cc-mode"
	(c-offsets-alist . ((innamespace . [0])))))

(c-add-style "chiggie-style" my-cc-style)
(setq-default c-basic-offset 4 c-default-style "chiggie-style")

;; Company-C-Headers
(package-install 'company-c-headers)
(add-to-list 'company-backend 'company-c-headers)

;; Emacs eclim, for java
;; Remember to install eclim, not just emacs-eclim.
;; https://github.com/senny/emacs-eclim
;; (package-install 'emacs-eclim)
;; (require 'eclim)
;; (global-eclim-mode)
