;; Test the exact structure of the function
(defun my/file-icon (filename)
  "Get file icon for FILENAME with nerd-icons."
  (when (require 'nerd-icons nil t)
    (let ((ext (and filename (downcase (file-name-extension filename)))))
      (cond
       ((string= ext "js") (nerd-icons-devicon "nf-dev-javascript" :face 'nerd-icons-yellow))
       (t (nerd-icons-faicon "nf-fa-file" :face 'nerd-icons-blue))))))