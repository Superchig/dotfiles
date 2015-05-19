;;;; Color themes, so they're loaded after plugins

;; Theme installation
(package-install 'monokai-theme)
(package-install 'solarized-theme)

;; Current Theme
(load-theme 'solarized-dark t)
