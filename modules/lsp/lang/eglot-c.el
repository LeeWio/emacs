;;; eglot-c.el --- Optimized Eglot config for C/C++ -*- lexical-binding: t -*-

;;; Commentary:
;; Reliable + performant eglot setup for C & C++ using clangd

;;; Code:

(use-package eglot
  :ensure t
  :hook
  ((c-mode c++-mode) . eglot-ensure)

  :config
  ;; -----------------------------
  ;; Detect clangd
  ;; -----------------------------
  (if (executable-find "clangd")
      (progn
        (message "[Eglot] Using clangd")

        ;; 避免重复添加
        (add-to-list
         'eglot-server-programs
         '((c-mode c++-mode)
           .
           ("clangd"
            "--background-index"
            "--clang-tidy"
            "--completion-style=detailed"
            "--header-insertion=never"
            "--fallback-style=llvm"
            "--suggest-missing-includes"
            "--log=error"))))
    (message "[Eglot] clangd NOT FOUND! Please install clangd."))

  ;; -----------------------------
  ;; QoL tweaks
  ;; -----------------------------
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; 语义高亮（Emacs29+）
              (when (fboundp 'eglot-inlay-hints-mode)
                (eglot-inlay-hints-mode 1))

              ;; 避免阻塞
              (setq-local eglot-sync-connect nil)

              ;; 自动重启
              (setq-local eglot-autoshutdown t)

              ;; xref 支持
              (setq-local eglot-extend-to-xref t)

              ;; 如需保存自动格式化，放开：
              ;; (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
              )))

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

(provide 'eglot-c)
;;; eglot-c.el ends here
