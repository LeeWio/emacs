;;; eglot-ts-config.el --- Eglot configuration for TypeScript -*- lexical-binding: t -*-

;;; Commentary:
;; Eglot configuration specifically for TypeScript / JavaScript

;;; Code:

(use-package eglot
  :ensure t
  :hook
  ((typescript-mode js-mode js-ts-mode tsx-ts-mode js-jsx-mode typescript-tsx-mode typescript-ts-mode) . eglot-ensure)

  :config
  ;; -----------------------------
  ;; Detect language servers
  ;; -----------------------------
  (let ((ts-server-found (executable-find "typescript-language-server"))
        (vtsls-found (executable-find "vtsls"))  ; Volta TypeScript Language Server for better Next.js/React support
        (biome-found (executable-find "biome"))  ; Biome is excellent for React projects
        )
    (cond
     ;; Use Biome if available (excellent for React projects)
     (biome-found
      (message "[Eglot] Using Biome (excellent for React/JSX/TSX projects)")
      (add-to-list 'eglot-server-programs
                   '((js-mode js-jsx-mode js-ts-mode typescript-mode tsx-ts-mode typescript-tsx-mode typescript-ts-mode)
                     .
                     ("biome" "lsp-proxy" "--stdio")))
      ;; Add tree-sitter support if available
      (when (treesit-available-p)
        (add-hook 'tsx-ts-mode-hook #'eglot-ensure)
        (add-hook 'js-ts-mode-hook #'eglot-ensure)
        (add-hook 'typescript-tsx-mode-hook #'eglot-ensure)
        (add-hook 'typescript-ts-mode-hook #'eglot-ensure)
        (add-to-list 'eglot-server-programs
                     '((tsx-ts-mode js-ts-mode typescript-tsx-mode typescript-ts-mode)
                       .
                       ("biome" "lsp-proxy" "--stdio")))))
     
     ;; Use vtsls if available (better for Next.js/React projects)
     (vtsls-found
      (message "[Eglot] Using vtsls (Volta TypeScript Language Server)")
      (add-to-list 'eglot-server-programs
                   '((typescript-mode js-mode js-jsx-mode tsx-ts-mode typescript-ts-mode)
                     .
                     ("vtsls" "--stdio")))
      ;; Add tree-sitter support if available
      (when (treesit-available-p)
        (add-hook 'tsx-ts-mode-hook #'eglot-ensure)
        (add-hook 'js-ts-mode-hook #'eglot-ensure)
        (add-hook 'typescript-tsx-mode-hook #'eglot-ensure)
        (add-hook 'typescript-ts-mode-hook #'eglot-ensure)
        (add-to-list 'eglot-server-programs
                     '((tsx-ts-mode js-ts-mode typescript-tsx-mode typescript-ts-mode)
                       .
                       ("vtsls" "--stdio")))))
     
     ;; Fallback to typescript-language-server (tsserver approach as recommended)
     (ts-server-found
      (message "[Eglot] Using typescript-language-server")
      (add-to-list 'eglot-server-programs
                   '((typescript-mode js-mode js-jsx-mode tsx-ts-mode typescript-ts-mode)
                     .
                     ("typescript-language-server" "--stdio")))
      ;; Add tree-sitter support if available
      (when (treesit-available-p)
        (add-hook 'tsx-ts-mode-hook #'eglot-ensure)
        (add-hook 'js-ts-mode-hook #'eglot-ensure)
        (add-hook 'typescript-tsx-mode-hook #'eglot-ensure)
        (add-hook 'typescript-ts-mode-hook #'eglot-ensure)
        (add-to-list 'eglot-server-programs
                     '((tsx-ts-mode js-ts-mode typescript-tsx-mode typescript-ts-mode)
                       .
                       ("typescript-language-server" "--stdio")))))
     
     ;; No server found
     (t
      (message "[Eglot] No JavaScript/TypeScript language server found! Install one of: npm i -g @biomejs/biome OR npm i -g @volar/vscode-typescript-languageserver OR npm i -g typescript-language-server typescript"))))

  ;; -----------------------------
  ;; QoL tweaks
  ;; -----------------------------
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Inlay hints (Emacs29+)
              (when (fboundp 'eglot-inlay-hints-mode)
                (eglot-inlay-hints-mode 1))

              ;; 避免阻塞
              (setq-local eglot-sync-connect nil)

              ;; 自动重启
              (setq-local eglot-autoshutdown t)

              ;; xref 支持
              (setq-local eglot-extend-to-xref t)

              ;; Format on save for frontend development
              (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
              
              ;; React-specific configuration
              (when (and (derived-mode-p 'js-jsx-mode 'tsx-ts-mode 'typescript-tsx-mode)
                         (fboundp 'eglot-inlay-hints-mode))
                ;; Enable inlay hints for better React development
                (eglot-inlay-hints-mode 1))
              
              ;; Additional JavaScript/TypeScript and frontend-specific settings
              (when (or (derived-mode-p 'js-mode 'typescript-mode 'js-jsx-mode 'tsx-ts-mode 'typescript-tsx-mode)
                        (and (buffer-file-name)
                             (string-match-p "\\(\\.\\(js\\|ts\\|tsx\\|jsx\\|mjs\\|cjs\\)$\\)" (buffer-file-name))))
                (setq-local eglot-workspace-configuration
                            (append eglot-workspace-configuration
                                    '(:typescript
                                      (:preferences
                                       (:includePackageJsonAutoImports "auto")
                                       (:includeAutomaticOptionalChainCompletions t)
                                       (:includeCompletionsForModuleExports t)
                                       (:includeCompletionsWithSnippetText t))
                                      (:formatOptions
                                       (:tabSize 2
                                        :indentSize 2
                                        :convertTabsToSpaces t))
                                      :javascript
                                      (:preferences
                                       (:includePackageJsonAutoImports "auto")
                                       (:includeAutomaticOptionalChainCompletions t)
                                       (:includeCompletionsForModuleExports t)
                                       (:includeCompletionsWithSnippetText t))
                                      (:formatOptions
                                       (:tabSize 2
                                        :indentSize 2
                                        :convertTabsToSpaces t)))))))))

;; -----------------------------
;; Key bindings
;; -----------------------------
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c d") 'eldoc)
  (define-key eglot-mode-map (kbd "C-c g d") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "C-c g r") 'xref-find-references)
  (define-key eglot-mode-map (kbd "C-c a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c f") 'eglot-format-buffer))

(provide 'eglot-ts-config)
;;; eglot-ts-config.el ends here