;;; Summary

;;; All of my codes for disabling and enabling parts of rubocop.

;; Rubocop disable / enable functions
(defun my/rubocop/disable-style-documentation ()
  "Inserts disable Style/Documentation comment at line above current."
  (interactive)
  (my/open-above-insert "# rubocop:disable Style/Documentation"))

(defun my/rubocop/enable-style-documentation ()
  "Inserts enable Style/Documentation comment at line below current."
  (interactive)
  (my/open-below-insert "# rubocop:enable Style/Documentation"))

(defun my/rubocop/line-disable-style-documentation ()
  "Disables Style/Documentation for the line Point is on."
  (interactive)
  (my/rubocop/disable-style-documentation)
  (my/rubocop/enable-style-documentation))

(defun my/rubocop/disable-style-module-children ()
  "Inserts disable Style/ClassAndModuleChildren comment at line above current."
  (interactive)
  (my/open-above-insert "# rubocop:disable Style/ClassAndModuleChildren"))

(defun my/rubocop/enable-style-module-children ()
  "Inserts enable Style/Documentation comment at line below current."
  (interactive)
  (my/open-below-insert "# rubocop:enable Style/ClassAndModuleChildren"))

(defun my/rubocop/line-disable-style-module-children ()
  (interactive)
  (my/rubocop/disable-style-module-children)
  (my/rubocop/enable-style-module-children))
