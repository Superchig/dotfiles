;;;; Color themes, so they're loaded after plugins

;; Theme installation
(package-install 'monokai-theme)
(package-install 'solarized-theme)
(package-install 'darcula-theme)
(package-install 'ample-zen-theme)
(package-install 'jazz-theme)

;; Current Theme
(load-theme 'monokai t)
