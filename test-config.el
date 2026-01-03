(defun my/test-function ()
  (when (require 'nerd-icons nil t)
    (let ((test-var "test"))
      (cond
       ((string= test-var "test") (nerd-icons-faicon "nf-fa-file" :face 'nerd-icons-blue))
       (t (nerd-icons-faicon "nf-fa-file_o" :face 'nerd-icons-blue))))))

;; Test the syntax
(my/test-function)