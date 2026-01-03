;;; tsx-config.el --- TSX specific configuration -*- lexical-binding: t -*-

;;; Commentary:
;; TSX-specific configuration for React/JSX in TypeScript files with tree-sitter support
;; Works with both tree-sitter and web-mode for optimal JSX editing experience

;;; Code:

;; Special configuration for TSX files to ensure JSX support
(defun my/tsx-file-setup ()
  "Setup for TSX files with tree-sitter support."
  ;; This runs when a .tsx file is opened
  (when (and (buffer-file-name) (string-match "\\.tsx\\'" (buffer-file-name)))
    ;; Ensure proper indentation and eglot settings
    (setq-local eglot-send-changes-idle-time 0.5)
    
    ;; Handle both traditional and tree-sitter modes
    (cond
     ;; Tree-sitter mode (tsx-ts-mode)
     ((eq major-mode 'tsx-ts-mode)
      (setq-local typescript-indent-level 2)
      ;; Ensure eglot is enabled for tree-sitter mode
      (when (fboundp 'eglot-ensure)
        (eglot-ensure)))
     
     ;; Traditional mode (typescript-mode)
     ((eq major-mode 'typescript-mode)
      (setq-local typescript-indent-level 2))
     
     ;; Fallback - switch to appropriate mode
     (t
      (when (fboundp 'tsx-ts-mode)
        (tsx-ts-mode))))))

;; Enhanced TSX file handling with web-mode compatibility
(defun my/enhanced-tsx-setup ()
  "Enhanced setup for TSX files including tree-sitter support."
  (when (and (buffer-file-name) (string-match "\\.tsx\\'" (buffer-file-name)))
    ;; Enable tree-sitter mode if available
    (when (and (fboundp 'tsx-ts-mode) (treesit-available-p))
      (tsx-ts-mode))
    
    ;; Setup eglot regardless of mode
    (when (fboundp 'eglot-ensure)
      (eglot-ensure))
    
    ;; Configure for React development
    (setq-local eglot-workspace-configuration
                '(:typescript
                  (:inlayHints
                   (:parameterNames
                    (:enabled "literals")
                    :parameterTypes
                    (:enabled t)
                    :variableTypes
                    (:enabled t)
                    :propertyDeclarationTypes
                    (:enabled t)
                    :functionLikeReturnTypes
                    (:enabled t)
                    :enumMemberValues
                    (:enabled t))
                  :javascript
                  (:inlayHints
                   (:parameterNames
                    (:enabled "literals")
                    :parameterTypes
                    (:enabled t)
                    :variableTypes
                    (:enabled t))))))))

;; Enhanced JSX-specific setup for better tag handling
(defun my-enhanced-jsx-setup ()
  "Setup for better JSX editing experience."
  (when (and (buffer-file-name) 
             (or (string-match "\\.tsx\\'" (buffer-file-name))
                 (string-match "\\.jsx\\'" (buffer-file-name))))
    ;; Configure indentation for JSX
    (setq-local indent-tabs-mode nil)
    (setq-local tab-width 2)))

;; Run setup when opening TSX files
(add-hook 'typescript-mode-hook 'my/tsx-file-setup)
(add-hook 'tsx-ts-mode-hook 'my/enhanced-tsx-setup)
(add-hook 'tsx-ts-mode-hook 'my-enhanced-jsx-setup)
(add-hook 'typescript-mode-hook 'my-enhanced-jsx-setup)

(provide 'tsx-config)
;;; tsx-config.el ends here