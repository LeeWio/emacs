;;; jsx-web-mode-config.el --- Enhanced JSX/TSX editing with tree-sitter and smart tools -*- lexical-binding: t -*-

;;; Commentary:
;; Enhanced JSX/TSX editing experience using tree-sitter and other modes
;; This provides better tag handling while maintaining eglot compatibility

;;; Code:

;; Configure emmet-mode for better JSX expansion (works with tsx-ts-mode)
(use-package emmet-mode
  :ensure t
  :hook ((tsx-ts-mode js-ts-mode typescript-ts-mode js-jsx-mode) . emmet-mode)
  :config
  ;; Configure emmet for JSX/TSX
  (defun my-emmet-setup ()
    "Setup emmet for JSX/TSX files."
    (when (or (string-match "\\.tsx\\'" (buffer-file-name))
              (string-match "\\.jsx\\'" (buffer-file-name)))
      (setq emmet-expand-jsx-className? t
            emmet-jsx-tag-syntax t
            emmet-expand-on-tab t
            emmet-self-closing-tag-style " />"
            emmet-move-cursor-after-expanding t))) ; Ensure cursor moves to content position
  
  (add-hook 'tsx-ts-mode-hook 'my-emmet-setup)
  (add-hook 'js-ts-mode-hook 'my-emmet-setup)
  (add-hook 'typescript-ts-mode-hook 'my-emmet-setup)
  (add-hook 'js-jsx-mode-hook 'my-emmet-setup)
  
  ;; Key bindings for emmet in JSX/TSX
  (define-key emmet-mode-keymap (kbd "C-j") 'emmet-expand-line))

;; Configure proper JSX tag formatting with newlines
(defun my-setup-jsx-formatting ()
  "Setup JSX tag formatting with proper newlines."
  (when (or (string-match "\\.tsx\\'" (buffer-file-name))
            (string-match "\\.jsx\\'" (buffer-file-name)))
    ;; Customize emmet to add newlines for JSX tags
    (setq-local emmet-use-style-tag-and-class-shorthand t)))

;; Apply formatting setup to JSX modes
(add-hook 'tsx-ts-mode-hook 'my-setup-jsx-formatting)
(add-hook 'js-jsx-mode-hook 'my-setup-jsx-formatting)

;; Configure Enter key behavior for proper JSX tag formatting
(defun my-setup-jsx-enter-behavior ()
  "Setup Enter key behavior for proper JSX tag formatting."
  (when (or (string-match "\\.tsx\\'" (buffer-file-name))
            (string-match "\\.jsx\\'" (buffer-file-name)))
    (local-set-key (kbd "RET") 
                   (lambda ()
                     (interactive)
                     ;; Check if we're between matching JSX tags on the same line
                     (if (looking-back ">\\s-*</" (line-beginning-position))
                         (let ((line-content (thing-at-point 'line t)))
                           (if (string-match "\\(<[^>]+>\\)\\(</[^>]+>\\)" line-content)
                               ;; We have <tag></tag> on the same line, split it
                               (progn
                                 (newline-and-indent)
                                 (insert "  ") ; Add some indentation
                                 (newline-and-indent))
                             ;; Otherwise, just do normal newline
                             (newline-and-indent)))
                       ;; Otherwise, just do normal newline
                       (newline-and-indent))))))

;; Apply Enter key behavior to JSX modes
(add-hook 'tsx-ts-mode-hook 'my-setup-jsx-enter-behavior)
(add-hook 'js-jsx-mode-hook 'my-setup-jsx-enter-behavior)

;; The default Tab key behavior should work fine with corfu

;; Configure smartparens for JSX tag handling in tree-sitter modes
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  
  ;; Disable strict mode specifically in JSX files to avoid bracket balancing conflicts
  (defun my-disable-smartparens-strict-mode-for-jsx ()
    "Disable smartparens strict mode in JSX files."
    (when (or (string-match "\\.tsx\\'" (buffer-file-name))
              (string-match "\\.jsx\\'" (buffer-file-name)))
      (when smartparens-strict-mode
        (smartparens-strict-mode -1))))
  
  ;; Apply to relevant modes
  (add-hook 'tsx-ts-mode-hook 'my-disable-smartparens-strict-mode-for-jsx)
  (add-hook 'js-jsx-mode-hook 'my-disable-smartparens-strict-mode-for-jsx)
  
  ;; Define pairs for JSX (works regardless of strict mode)
  (sp-with-modes '(tsx-ts-mode js-ts-mode typescript-ts-mode js-mode js-jsx-mode)
    (sp-local-pair "<" ">")
    (sp-local-tag "<" "<%/>" "</%>" :actions '(insert navigate))
    (sp-local-pair "/* " " */")) ; For comments

  ;; Additional JSX-specific smartparens rules
  (sp-with-modes '(tsx-ts-mode js-ts-mode typescript-ts-mode js-mode js-jsx-mode)
    (sp-local-pair "<" ">" :actions '(insert navigate)
                    :post-handlers '(("||\n[i]" "RET")
                                     ("| " "SPC")))))

;; Additional helper functions for JSX editing
(defun jsx-insert-self-closing-tag (tag)
  "Insert a self-closing JSX tag."
  (interactive "sTag: ")
  (insert (format "<%s />" tag))
  (backward-char 3))

;; Key bindings for JSX convenience
(with-eval-after-load 'typescript-ts-mode
  (define-key typescript-ts-mode-map (kbd "C-c j t") 'jsx-insert-self-closing-tag))

(with-eval-after-load 'tsx-ts-mode
  (define-key tsx-ts-mode-map (kbd "C-c j t") 'jsx-insert-self-closing-tag))

(provide 'jsx-web-mode-config)
;;; jsx-web-mode-config.el ends here