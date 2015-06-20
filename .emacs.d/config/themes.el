;;;; Color themes, so they're loaded after plugins

;; Theme installation
(package-install 'monokai-theme)
(package-install 'solarized-theme)
(package-install 'darcula-theme)

;; Current Theme
(load-theme 'monokai t)
