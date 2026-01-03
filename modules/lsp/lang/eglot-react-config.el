;;; eglot-react-config.el --- Eglot configuration for React/JSX -*- lexical-binding: t -*-

;;; Commentary:
;; Eglot configuration specifically for React/JSX development

;;; Code:

;; React-specific configuration for eglot
(defun my-react-eglot-config ()
  "Configure eglot for React/JSX development."
  (when (or (derived-mode-p 'js-jsx-mode 'tsx-ts-mode 'typescript-tsx-mode 'typescript-ts-mode 'js-ts-mode)
            (and buffer-file-name 
                 (string-match "\\.\\(jsx\\|tsx\\|ts\\)$" buffer-file-name)))
    ;; Set up React-specific TypeScript/JavaScript settings
    (setq-local eglot-workspace-configuration
                (append eglot-workspace-configuration
                        '(:typescript
                          (:preferences
                           (:includePackageJsonAutoImports "auto")
                           (:includeAutomaticOptionalChainCompletions t)
                           (:includeCompletionsForModuleExports t)
                           (:includeCompletionsWithSnippetText t)
                           (:jsxAttributeCompletionStyle "auto")
                           (:includeInlayParameterNameHints "all")
                           (:includeInlayParameterNameHintsWhenArgumentMatchesName nil)
                           (:includeInlayFunctionParameterNameHintsWhenTypeMatchesName nil)
                           (:includeInlayFunctionParameterTypeHints t)
                           (:includeInlayVariableTypeHints t)
                           (:includeInlayPropertyDeclarationTypeHints t)
                           (:includeInlayFunctionLikeReturnTypeHints t)
                           (:includeInlayEnumMemberValueHints t)
                           (:includeInlayVariableTypeHintsWhenTypeMatchesName nil))
                          :javascript
                          (:preferences
                           (:includePackageJsonAutoImports "auto")
                           (:includeAutomaticOptionalChainCompletions t)
                           (:includeCompletionsForModuleExports t)
                           (:includeCompletionsWithSnippetText t)
                           (:jsxAttributeCompletionStyle "auto")
                           (:includeInlayParameterNameHints "all")
                           (:includeInlayParameterNameHintsWhenArgumentMatchesName nil)
                           (:includeInlayFunctionParameterNameHintsWhenTypeMatchesName nil)
                           (:includeInlayFunctionParameterTypeHints t)
                           (:includeInlayVariableTypeHints t)
                           (:includeInlayPropertyDeclarationTypeHints t)
                           (:includeInlayFunctionLikeReturnTypeHints t)
                           (:includeInlayEnumMemberValueHints t))
                          :compilerOptions
                          (:jsx "react-jsx"
                           :jsxFactory "React.createElement"
                           :jsxFragmentFactory "React.Fragment"
                           :allowSyntheticDefaultImports t
                           :esModuleInterop t
                           :strictNullChecks t
                           :skipLibCheck t
                           :moduleResolution "node"
                           :resolveJsonModule t
                           :isolatedModules t
                           :noEmit t
                           :lib ["dom" "dom.iterable" "esnext"]
                           :allowJs t
                           :jsxImportSource "react"))))))

;; Add to eglot initialization
(add-hook 'eglot-managed-mode-hook 'my-react-eglot-config)

(provide 'eglot-react-config)
;;; eglot-react-config.el ends here