;;;; My 'custom' functions

;; Etag
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command 
   (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

;; Kill all other buffers function
(defun only-current-buffer () 
  (interactive)                                                                   
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

(defun my/clean-buffer-formatting ()
  "Indent and clean up the buffer"
  (interactive)
  (indent-region (point-min) (point-max))
  (whitespace-cleanup))

(defun notes ()
  "Switch to my work dir."
  (interactive)
  (find-file "~/Desktop/notes"))

(defun my/open-above-insert (text)
  "Opens new line above Point, and insert text in this line."
  (save-excursion 
	(beginning-of-line)
	(open-line 1)
	(indent-according-to-mode)
	(insert text)))

(defun my/open-below-insert (text)
  "Opens new line below Point, and insert text in this line."
  (save-excursion
	(end-of-line)
	(open-line 1)
	(forward-line 1)
	(indent-according-to-mode)
	(insert text)))

(load-file "~/dotfiles/.emacs.d/config/functions/rubocop-extra.el")
(load-file "~/dotfiles/.emacs.d/config/functions/elisp-extra.el")
(load-file "~/dotfiles/.emacs.d/config/functions/cpp-extra.el")

()
