;; -*- lexical-binding: t -*-


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load "~/.emacs.d/config/functions.el")

(load "~/.emacs.d/config/plugins.el")

(load "~/.emacs.d/config/themes.el")

(load "~/.emacs.d/config/rebind.el")

(load "~/.emacs.d/config/basic.el")

;; Auto-set custom stuff

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote pushy))
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
	("c59857e3e950131e0c17c65711f1812d20a54b829115b7c522672ae6ba0864cc" "6c62b1cd715d26eb5aa53843ed9a54fc2b0d7c5e0f5118d4efafa13d7715c56e" "118717ce0a2645a0cf240b044999f964577ee10137b1f992b09a317d5073c02d" "49ad7c8d458074db7392f8b8a49235496e9228eb2fa6d3ca3a7aa9d23454efc6" "19352d62ea0395879be564fc36bc0b4780d9768a964d26dfae8aad218062858d" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "3dafeadb813a33031848dfebfa0928e37e7a3c18efefa10f3e9f48d1993598d3" "f1af57ed9c239a5db90a312de03741e703f712355417662c18e3f66787f94cbe" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" "90edd91338ebfdfcd52ecd4025f1c7f731aced4c9c49ed28cfbebb3a3654840b" "6a9606327ecca6e772fba6ef46137d129e6d1888dcfc65d0b9b27a7a00a4af20" "a041a61c0387c57bb65150f002862ebcfe41135a3e3425268de24200b82d6ec9" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "0eebf69ceadbbcdd747713f2f3f839fe0d4a45bd0d4d9f46145e40878fc9b098" default)))
 '(evil-overriding-maps
   (quote
	((color-theme-mode-map)
	 (comint-mode-map)
	 (compilation-mode-map)
	 (grep-mode-map)
	 (dictionary-mode-map)
	 (ert-results-mode-map . motion)
	 (Info-mode-map . motion)
	 (speedbar-key-map)
	 (speedbar-file-key-map)
	 (speedbar-buffers-key-map))))
 '(fci-rule-color "#3E3D31")
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-tags-on-save t)
 '(helm-mode t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-symbol-colors
   (--map
	(solarized-color-blend it "#002b36" 0.25)
	(quote
	 ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
	(("#3E3D31" . 0)
	 ("#67930F" . 20)
	 ("#349B8D" . 30)
	 ("#21889B" . 50)
	 ("#968B26" . 60)
	 ("#A45E0A" . 70)
	 ("#A41F99" . 85)
	 ("#3E3D31" . 100))))
 '(hl-bg-colors
   (quote
	("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
	("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(org-agenda-files (quote ("~/Documents/todo.org")))
 '(org-cycle-level-faces t)
 '(org-fontify-emphasized-text t)
 '(org-fontify-whole-heading-line t)
 '(org-lowest-priority 68)
 '(org-time-stamp-custom-formats (quote ("<%Y-%m-%d>" . "<%Y-%m-%d %a %I:%M%p>")))
 '(package-archives
   (quote
	(("gnu" . "http://elpa.gnu.org/packages/")
	 ("melpa_stable" . "http://melpa-stable.org/packages/")
	 ("melpa2" . "http://melpa.milkbox.net/packages/"))))
 '(package-selected-packages
   (quote
	(org-evil 0blayout jazz-theme ample-zen-theme darcula-theme solarized-theme monokai-theme irony flycheck paredit helm async powerline evil-leader evil company yasnippet srefactor rust-mode ruby-end robe rainbow-delimiters projectile powerline-evil minitest markdown-mode lua-mode key-chord irony-eldoc indent-guide helm-gtags haskell-mode geiser flycheck-rust evil-surround evil-paredit evil-org evil-nerd-commenter evil-matchit enh-ruby-mode company-irony company-c-headers coffee-mode c-eldoc)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
	((20 . "#F92672")
	 (40 . "#CF4F1F")
	 (60 . "#C26C0F")
	 (80 . "#E6DB74")
	 (100 . "#AB8C00")
	 (120 . "#A18F00")
	 (140 . "#989200")
	 (160 . "#8E9500")
	 (180 . "#A6E22E")
	 (200 . "#729A1E")
	 (220 . "#609C3C")
	 (240 . "#4E9D5B")
	 (260 . "#3C9F79")
	 (280 . "#A1EFE4")
	 (300 . "#299BA6")
	 (320 . "#2896B5")
	 (340 . "#2790C3")
	 (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3E3D31" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-structure ((t (:height 1.0 :family "Terminus"))))
 '(org-level-1 ((t (:foreground "#FD971F" :height 1.2 :family "Terminus"))))
 '(org-level-2 ((t (:foreground "#A6E22E" :height 1.0 :family "Terminus"))))
 '(org-level-3 ((t (:foreground "#66D9EF" :height 1.0 :family "Terminus"))))
 '(org-level-4 ((t (:foreground "#E6DB74" :height 1.0 :family "Terminus"))))
 '(org-level-5 ((t (:foreground "#A1EFE4" :height 1.0 :family "Terminus"))))
 '(org-level-6 ((t (:foreground "#A6E22E" :height 1.0 :family "Terminus"))))
 '(org-level-7 ((t (:foreground "#F92672" :height 1.0 :family "Terminus"))))
 '(org-level-8 ((t (:foreground "#66D9EF" :height 1.0 :family "Terminus")))))
