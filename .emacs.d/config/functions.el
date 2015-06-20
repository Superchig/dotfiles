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

(defun notes ()
  "Switch to my work dir."
   (interactive)
   (find-file "~/Desktop/notes")
   )
