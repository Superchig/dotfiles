;;;; Bind keys in various modes
(defun org-evil-mode-config ()
  "For use in `org-mode-hook'."
  (local-set-key (kbd "M-l") 'org-shiftright)
  (local-set-key (kbd "M-h") 'org-shiftleft)
  (local-set-key (kbd "M-k") 'org-move-item-up)
  (local-set-key (kbd "M-j") 'org-move-item-down))

(add-hook 'org-mode-hook 'org-evil-mode-config)
