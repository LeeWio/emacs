;;; jsx-config.el --- JSX configuration -*- lexical-binding: t -*-

;;; Commentary:
;; JSX-specific configuration for React development

;;; Code:

;; Enable tree-sitter auto mode for TypeScript/TSX files (recommended approach)
(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode)

  ;; Remap major modes to use tree-sitter versions
  (setq major-mode-remap-alist
        '((typescript-mode . typescript-ts-mode)
          (js-mode         . js-ts-mode)
          (tsx-mode        . tsx-ts-mode)))

  ;; Explicit file associations
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode)))

(provide 'jsx-config)
;;; jsx-config.el ends here