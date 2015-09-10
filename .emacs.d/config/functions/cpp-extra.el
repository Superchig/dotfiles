;;; cpp-extra.el --- Summary

;;; Commentary:
;;; These comments are just satisfying flycheck.

;;; Code:

(defun cpp/flycheck-clang-c++11 ()
  "Set the directory local variables to stop flycheck c++11 errors."
  (interactive)
  (add-dir-local-variable 'c++-mode
						  'flycheck-clang-language-standard
						  "c++11")
  (save-buffer))

(provide 'cpp-extra)
;;; cpp-extra.el ends here
