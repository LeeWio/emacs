;;; treesit-config.el --- Tree-sitter configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Tree-sitter configuration for modern syntax parsing

;;; Code:

;; Configure tree-sitter-auto for automatic grammar management
;; Temporarily disabled due to version compatibility issues
;; (use-package treesit-auto
;;   :ensure t
;;   :custom
;;   ;; Automatically install grammars when needed
;;   (treesit-auto-install 'prompt)
;;   ;; List of languages to automatically manage
;;   (treesit-auto-langs 
;;    '(tsx typescript javascript jsx css html))
;;   :config
;;   ;; Enable tree-sitter-auto globally
;;   (global-treesit-auto-mode))

;; Configure major modes to use tree-sitter when available
;; Temporarily disabled due to version compatibility issues
;; (use-package treesit
;;   :ensure nil
;;   :if (treesit-available-p)
;;   :config
;;   ;; Map file extensions to tree-sitter modes
;;   (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(jsx-mode . tsx-ts-mode))
;;   
;;   ;; Setup tree-sitter for TSX files specifically
;;   (defun my/setup-tsx-tree-sitter ()
;;     "Setup tree-sitter for TSX files."
;;     (when (and (buffer-file-name)
;;                (string-match "\\.tsx\\'" (buffer-file-name)))
;;       ;; Ensure we're using the tree-sitter mode
;;       (when (fboundp 'tsx-ts-mode)
;;         (tsx-ts-mode))
;;       ;; Setup eglot for tree-sitter mode
;;       (when (fboundp 'eglot-ensure)
;;         (eglot-ensure))))
;;
;;   ;; Apply setup to appropriate hooks
;;   (add-hook 'find-file-hook 'my/setup-tsx-tree-sitter)
;;   
;;   ;; Also setup for buffers that might switch modes
;;   (add-hook 'typescript-mode-hook 
;;             (lambda () 
;;               (when (and (buffer-file-name)
;;                          (string-match "\\.tsx\\'" (buffer-file-name)))
;;                 (my/setup-tsx-tree-sitter)))))

(provide 'treesit-config)
;;; treesit-config.el ends here