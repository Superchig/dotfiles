;;; Summary

;;; My emacs lisp extra functions. Mainly just satisfying flycheck.

(defun elisp/satisfy-flycheck (package-name)
  "Satisfies flychecks' Emacs Lisp package name, summary, and commentary section and such.
PACKAGE-NAME - The name of the package."
  (interactive "sPackage name: ")
  (save-excursion
	(goto-char (point-min))
	(insert (format ";;; %s --- Summary" package-name))
	(newline 2)
	(insert ";;; Commentary:")
	(newline 2)
	(insert ";;; Code:")
	(newline 2)
	(goto-char (point-max))
	(newline)
	(insert (format "(provide '%s)"
					(substring (buffer-name)
							   0
							   (- (length (buffer-name)) 4))))
	(newline)
	(insert (format ";;; %s ends here"
					(buffer-name)))))

()
