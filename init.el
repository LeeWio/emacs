;;; init.el --- Optimized C/C++ config with Catppuccin, Corfu, Eglot, Treemacs, Window Management & Icons -*- lexical-binding: t -*-

;;; ------------------------------------------------------------
;;; 启动优化
;;; ------------------------------------------------------------
(setq gc-cons-threshold (* 64 1024 1024))
(defvar my/original-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  file-name-handler-alist my/original-file-name-handler-alist)
            (message "Emacs ready in %s with %d GCs."
                     (emacs-init-time) gcs-done)))

;;; ------------------------------------------------------------
;;; straight.el bootstrap
;;; ------------------------------------------------------------
(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)

;;; ------------------------------------------------------------
;;; use-package
;;; ------------------------------------------------------------
(straight-use-package 'use-package)
(setq use-package-always-defer t)

;;; ------------------------------------------------------------
;;; Catppuccin 主题
;;; ------------------------------------------------------------
(straight-use-package 'catppuccin-theme)
(setq catppuccin-flavor 'mocha)
(load-theme 'catppuccin :no-confirm)

;; Set transparent backgrounds for faces
(set-face-attribute 'default nil :background nil)
(set-face-attribute 'fringe nil :background nil)
(set-face-attribute 'line-number nil :background nil)
(set-face-attribute 'line-number-current-line nil :background nil)
(set-face-attribute 'mode-line nil :background nil)
(set-face-attribute 'mode-line-inactive nil :background nil)

;;; ------------------------------------------------------------
;;; 基础编辑体验
;;; ------------------------------------------------------------
(setq-default
 indent-tabs-mode nil
 tab-width 4
 fill-column 120)
(setq ring-bell-function 'ignore
      make-backup-files nil
      auto-save-default nil)

;;; ------------------------------------------------------------
;;; 高亮行 + 行号
;;; ------------------------------------------------------------
(global-hl-line-mode 1)

(defun my/prog-mode-hook ()
  "Programming mode hooks: line numbers, relative numbers, electric pair, flymake, imenu."
  (display-line-numbers-mode 1)
  (setq display-line-numbers 'relative)
  (electric-pair-mode 1)
  (flymake-mode 1)
  (imenu-add-menubar-index))

(add-hook 'prog-mode-hook #'my/prog-mode-hook)

;;; ------------------------------------------------------------
;;; 清爽界面
;;; ------------------------------------------------------------
(menu-bar-mode -1)
(tool-bar-mode -1)

;;; ------------------------------------------------------------
;;; Flymake 快捷键 (手动跳转)
;;; ------------------------------------------------------------
(define-key prog-mode-map (kbd "M-n") #'flymake-goto-next-error)
(define-key prog-mode-map (kbd "M-p") #'flymake-goto-prev-error)
(setq flymake-show-diagnostics-at-end-of-line nil
      flymake-no-changes-timeout nil
      flymake-start-on-flymake-mode nil)

;;; ------------------------------------------------------------
;;; Completion 基础
;;; ------------------------------------------------------------
(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

(use-package orderless
  :init
  (setq completion-styles '(basic orderless)
        completion-category-defaults nil
        completion-category-overrides '((eglot (styles orderless)))))

;;; ------------------------------------------------------------
;;; Corfu 自动补全 + TAB 联动 Yasnippet
;;; ------------------------------------------------------------
(use-package corfu-terminal
  :ensure t
  :init (corfu-terminal-mode 1))

(use-package corfu
  :defer nil
  :init
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-cycle t
        corfu-preselect 'prompt)
  :config
  (global-corfu-mode 1)

  ;; TAB / Shift-TAB 补全和 snippet 联动
  (defun my/corfu-yas-or-next ()
    "如果在 snippet 中跳到下一个 field，否则 corfu-next."
    (interactive)
    (if (and (bound-and-true-p yas-minor-mode)
             (or (yas--templates-for-key-at-point)
                 (yas--snippets-at-point)))
        (yas-next-field)
      (corfu-next)))

  (defun my/corfu-yas-or-previous ()
    "如果在 snippet 中跳到上一个 field，否则 corfu-previous."
    (interactive)
    (if (and (bound-and-true-p yas-minor-mode)
             (yas--snippets-at-point))
        (yas-prev-field)
      (corfu-previous)))

  (define-key corfu-map (kbd "TAB") #'my/corfu-yas-or-next)
  (define-key corfu-map (kbd "<tab>") #'my/corfu-yas-or-next)
  (define-key corfu-map (kbd "S-TAB") #'my/corfu-yas-or-previous)
  (define-key corfu-map (kbd "<backtab>") #'my/corfu-yas-or-previous)
  (define-key corfu-map (kbd "RET") #'corfu-complete)
  (define-key corfu-map (kbd "ESC") #'corfu-quit))

;;; Yasnippet
(use-package yasnippet
  :straight t
  :init
  (yas-global-mode 1))

;;; ------------------------------------------------------------
;;; C/C++ + Eglot
;;; ------------------------------------------------------------
(defun my/find-project-root ()
  "Find project root using VC or default."
  (or (vc-root-dir)
      (locate-dominating-file default-directory ".")
      default-directory))

(use-package cc-mode
  :defer nil
  :mode (("\\.c\\'"   . c-mode)
         ("\\.h\\'"   . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode))
  :init
  (setq c-basic-offset 4
        c-default-style "linux")
  :config
  (define-key c-mode-base-map (kbd "C-c o") #'ff-find-other-file)
  (define-key c-mode-base-map (kbd "C-c C-o")
    (lambda () (interactive)
      (let ((ff-search-directories
             (cons (my/find-project-root)
                   '("." "../include" "../inc" "../src" "../"))))
        (ff-find-other-file)))))

(use-package eglot
  :defer nil
  :hook ((c-mode c++-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode) . ("clangd"
                                      "--background-index"
                                      "--clang-tidy"
                                      "--completion-style=detailed"
                                      "--header-insertion=never"
                                      "--pch-storage=memory")))

  ;; 禁用自动 Eldoc 弹窗
  (setq eldoc-mode nil
        eldoc-documentation-strategy nil
        eldoc-echo-area-use-multiline-p nil)

  ;; Flymake 只在手动触发
  (setq flymake-start-on-flymake-mode nil
        flymake-no-changes-timeout nil
        flymake-show-diagnostics-at-end-of-line nil)

  ;; 禁用 Eglot 自动弹窗 signature/help
  (setq eglot-ignored-server-capabilities '(:hoverProvider :signatureHelpProvider))

  ;; 常用快捷键
  (define-key eglot-mode-map (kbd "C-c f") #'eglot-format)
  (define-key eglot-mode-map (kbd "C-c h") #'eglot-hover)
  (define-key eglot-mode-map (kbd "C-c d") #'flymake-show-buffer-diagnostics)
  (define-key eglot-mode-map (kbd "M-.") #'xref-find-definitions)
  (define-key eglot-mode-map (kbd "M-,") #'xref-pop-marker-stack)
  (define-key eglot-mode-map (kbd "C-c r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c i") #'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c e") #'eglot-find-declaration))

;;; ------------------------------------------------------------
;;; Emacs Lisp — built-in Eglot LSP (NO external server)
;;; ------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook #'eglot-ensure)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local eldoc-mode nil
                        eldoc-documentation-strategy nil
                        eldoc-echo-area-use-multiline-p nil
                        flymake-show-diagnostics-at-end-of-line nil)))

;;; ------------------------------------------------------------
;;; Java — Eglot + jdtls
;;; ------------------------------------------------------------
(use-package eglot
  :hook (java-mode . eglot-ensure)
  :config
  (add-to-list
   'eglot-server-programs
   `(java-mode .
     ("jdtls"
      "-data" ,(expand-file-name "~/.cache/jdtls-workspace/")))))

;;; ------------------------------------------------------------
;;; TypeScript / TSX / JavaScript
;;; ------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.ts\\'"  . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'"  . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode))

(setq treesit-extra-load-path '("~/.emacs.d/tree-sitter-libs/"))

(when (treesit-available-p)
  (dolist (lang '((typescript "libtree-sitter-typescript.dylib")
                  (tsx        "libtree-sitter-tsx.dylib")))
    (let ((lang-name (car lang))
          (lib-path (expand-file-name (cadr lang) "~/.emacs.d/tree-sitter-libs/")))
      (unless (treesit-language-available-p lang-name)
        (ignore-errors
          (treesit-load-language-grammar lang-name lib-path))))))

(when (treesit-available-p)
  (add-to-list 'major-mode-remap-alist '(typescript-ts-mode . typescript-ts-mode))
  (add-to-list 'major-mode-remap-alist '(tsx-ts-mode . tsx-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-ts-mode . js-ts-mode)))

(use-package eglot
  :defer nil
  :hook ((typescript-ts-mode tsx-ts-mode js-ts-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode js-ts-mode)
                 . ("typescript-language-server" "--stdio")))

  ;; 自动保存时格式化
  (dolist (hook '(typescript-ts-mode-hook tsx-ts-mode-hook js-ts-mode-hook))
    (add-hook hook (lambda () (setq-local eglot-format-buffer-on-save t))))

  ;; 安静模式，不打扰
  (setq eglot-ignored-server-capabilities '(:hoverProvider :signatureHelpProvider)))

;;; ------------------------------------------------------------
;;; 项目浏览 / 模糊搜索 (Treemacs + Vertico + Consult)
;;; ------------------------------------------------------------
(straight-use-package 'vertico)
(vertico-mode 1)
(straight-use-package 'savehist)
(savehist-mode 1)
(straight-use-package 'consult)
(straight-use-package 'consult-projectile)

(straight-use-package 'treemacs)
(straight-use-package 'treemacs-projectile)
(straight-use-package 'treemacs-icons-dired)
(straight-use-package 'projectile)
(projectile-mode +1)
(setq projectile-completion-system 'default)

(setq treemacs-follow-after-init t
      treemacs-is-never-other-window t
      treemacs-width 30)
(global-set-key (kbd "C-c t") 'treemacs)
(global-set-key (kbd "C-c T") 'treemacs-toggle)
(treemacs-projectile)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x C-f") 'consult-find)
(global-set-key (kbd "M-g g") 'consult-goto-line)
(global-set-key (kbd "M-g f") 'consult-flymake)
(global-set-key (kbd "M-g o") 'consult-outline)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-c p f") 'consult-projectile)

;;; ------------------------------------------------------------
;;; imenu-list
;;; ------------------------------------------------------------
(use-package imenu-list
  :straight t
  :bind ("C-'" . imenu-list-smart-toggle)
  :init
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize t
        imenu-list-size 30
        imenu-list-refresh-delay 0.5))

;;; ------------------------------------------------------------
;;; Tree-sitter
;;; ------------------------------------------------------------
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  (setq treesit-font-lock-level 4))

;;; ------------------------------------------------------------
;;; Which-Key
;;; ------------------------------------------------------------
(use-package which-key
  :straight t
  :defer 0
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.05
        which-key-show-early-on-C-h t
        which-key-max-description-length 40
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.25))

;;; ------------------------------------------------------------
;;; 窗口管理插件
;;; ------------------------------------------------------------
(use-package ace-window
  :straight t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-scope 'global))

(use-package buffer-move
  :straight t
  :bind (("C-c <up>"    . buf-move-up)
         ("C-c <down>"  . buf-move-down)
         ("C-c <left>"  . buf-move-left)
         ("C-c <right>" . buf-move-right)))

(use-package eyebrowse
  :straight t
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t))

;;; ------------------------------------------------------------
;;; Nerd Icons
;;; ------------------------------------------------------------
(use-package nerd-icons
  :straight t)

(use-package treemacs
  :after nerd-icons
  :straight t
  :config
  (setq treemacs-icon-open (nerd-icons-icon-for-dir "folder-open"))
  (setq treemacs-icon-closed (nerd-icons-icon-for-dir "folder")))

(use-package nerd-icons-dired
  :straight t
  :hook (dired-mode . nerd-icons-dired-mode))

;;; ------------------------------------------------------------
;;; Corfu + Kind-Icon
;;; ------------------------------------------------------------
(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; ------------------------------------------------------------
;;; Git integration
;;; ------------------------------------------------------------
(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)))

(use-package git-gutter
  :straight t
  :diminish
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 2)
  (custom-set-faces
   '(git-gutter:added    ((t (:foreground "#A6E3A1" :background nil :weight bold))))
   '(git-gutter:modified ((t (:foreground "#F9E2AF" :background nil :weight bold))))
   '(git-gutter:deleted  ((t (:foreground "#F38BA8" :background nil :weight bold)))))
  (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
  (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
  (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
  (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk))

(use-package blamer
  :ensure t
  :bind (("s-i" . blamer-show-commit-info)
         ("C-c i" . blamer-show-posframe-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t)))
  :config
  (global-blamer-mode 1))

;;; ------------------------------------------------------------
;;; Clipboard integration
;;; ------------------------------------------------------------
(defun my/setup-clipboard ()
  "Setup system clipboard integration for Wayland, X11, macOS, and GUI Emacs."
  (cond
   ((and (not (display-graphic-p))
         (string-equal (getenv "XDG_SESSION_TYPE") "wayland")
         (executable-find "wl-copy")
         (executable-find "wl-paste"))
    (setq interprogram-cut-function
          (lambda (text &optional push)
            (when text
              (let ((proc (start-process "wl-copy" nil "wl-copy" "-f")))
                (process-send-string proc text)
                (process-send-eof proc))))
          interprogram-paste-function
          (lambda ()
            (let ((output (string-trim (shell-command-to-string "wl-paste -n"))))
              (unless (string-empty-p output)
                output)))))

   ((and (not (display-graphic-p))
         (executable-find "xclip"))
    (setq interprogram-cut-function
          (lambda (text &optional push)
            (when text
              (let ((proc (start-process "xclip" nil "xclip" "-selection" "clipboard")))
                (process-send-string proc text)
                (process-send-eof proc))))
          interprogram-paste-function
          (lambda ()
            (let ((output (string-trim (shell-command-to-string "xclip -selection clipboard -o"))))
              (unless (string-empty-p output)
                output)))))

   ((eq system-type 'darwin)
    (setq interprogram-cut-function
          (lambda (text &optional push)
            (when text
              (let ((proc (start-process "pbcopy" nil "pbcopy")))
                (process-send-string proc text)
                (process-send-eof proc))))
          interprogram-paste-function
          (lambda ()
            (let ((output (string-trim (shell-command-to-string "pbpaste"))))
              (unless (string-empty-p output)
                output)))))))

(my/setup-clipboard)

(use-package rg
  :straight t)
(setq consult-ripgrep-args "rg --no-heading --line-number --color=always %s")

(use-package avy
  :straight t
  :bind ("C-:" . avy-goto-char-timer))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 5))

(provide 'init)
;;; init.el ends here

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "unspecified" :foreground "#cdd6f4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(corfu-bar ((t (:background "unspecified"))))
 '(corfu-current ((t (:extend t :background "gray1"))))
 '(corfu-default ((t (:background "unspecified"))))
 '(git-gutter:added ((t (:foreground "#A6E3A1" :background nil :weight bold))))
 '(git-gutter:deleted ((t (:foreground "#F38BA8" :background nil :weight bold))))
 '(git-gutter:modified ((t (:foreground "#F9E2AF" :background nil :weight bold))))
 '(hl-line ((t (:extend t :background "gray1"))))
 '(line-number ((t (:inherit default :background "unspecified" :foreground "#45475a"))))
 '(mode-line ((t (:background "unspecified"))))
 '(mode-line-highlight ((t (:inherit highlight))))
 '(scroll-bar ((t nil)))
 '(tab-bar ((t (:background "unspecified" :foreground "#a6adc8"))))
 '(tab-bar-tab ((t (:background "unspecified" :foreground "#cdd6f4"))))
 '(tab-bar-tab-ungrouped ((t (:inherit (shadow tab-bar-tab-inactive)))))
 '(tab-line ((t (:inherit tab-bar))))
 '(treemacs-root-face ((t (:background "unspecified" :foreground "#89b4fa"))))
 '(treemacs-window-background-face ((t (:background "unspecified")))))
