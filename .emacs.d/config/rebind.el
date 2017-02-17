;;;; Bind keys in various modes
(defun org-evil-mode-config ()
  "For use in `org-mode-hook'."
  (local-set-key (kbd "M-l") 'org-metaright)
  (local-set-key (kbd "M-h") 'org-metaleft)
  (local-set-key (kbd "M-k") 'org-metaup)
  (local-set-key (kbd "M-j") 'org-metadown))

(add-hook 'org-mode-hook 'org-evil-mode-config)

;; Change evil-mode M-hjkl to match M-up, down, left, right
(define-key key-translation-map (kbd "M-l") (kbd "M-<right>"))
(define-key key-translation-map (kbd "M-h") (kbd "M-<left>"))
(define-key key-translation-map (kbd "M-j") (kbd "M-<down>"))
(define-key key-translation-map (kbd "M-k") (kbd "M-<up>"))
