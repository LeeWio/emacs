;;; frontend-tools.el --- Frontend development tools configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for frontend development tools like prettier, eslint, etc.

;;; Code:

;; Flycheck for linting
(use-package flycheck
  :ensure t
  :hook ((js-mode js-jsx-mode tsx-ts-mode typescript-mode typescript-tsx-mode css-mode scss-mode less-mode html-mode json-mode) . flycheck-mode)
  :config
  ;; Configure multiple checkers for frontend development
  (setq flycheck-checkers '(javascript-eslint
                            javascript-prettier
                            typescript-tslint
                            typescript-eslint
                            json-jsonlint
                            css-csslint
                            css-stylelint
                            scss-lint
                            html-tidy
                            html-angular))
  
  ;; Configure executables for various linters
  (setq flycheck-javascript-eslint-executable "eslint"
        flycheck-typescript-tslint-executable "tslint"
        flycheck-typescript-eslint-executable "eslint"
        flycheck-css-stylelint-executable "stylelint"
        flycheck-scss-lint-executable "scss-lint"
        flycheck-html-tidy-executable "tidy")
  
  ;; Enable checkers for specific modes
  (flycheck-add-mode 'javascript-eslint 'js-jsx-mode)
  (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
  (flycheck-add-mode 'typescript-eslint 'tsx-ts-mode)
  (flycheck-add-mode 'typescript-eslint 'typescript-tsx-mode)
  (flycheck-add-mode 'css-stylelint 'scss-mode)
  (flycheck-add-mode 'css-stylelint 'less-mode)
  (flycheck-add-mode 'css-stylelint 'sass-mode)
  
  ;; Set configuration files
  (setq flycheck-eslintrc ".eslintrc.js"
        flycheck-tslint-rc ".tslint.json"
        flycheck-stylelintrc ".stylelintrc.json"
        flycheck-scss-lintrc ".scss-lint.yml"
        flycheck-eslint-rules-directories '("node_modules")
        flycheck-javascript-prettier-config-file ".prettierrc"
        flycheck-javascript-prettier-executable "prettier")
  
  ;; Enable more helpful features
  (setq flycheck-display-errors-delay 0.3
        flycheck-idle-change-delay 0.3
        flycheck-indication-mode 'left-fringe
        flycheck-highlighting-mode 'symbols
        flycheck-completion-system 'childframe))

;; Prettier integration for formatting
(use-package prettier-js
  :ensure t
  :hook ((js-mode js-jsx-mode tsx-ts-mode typescript-mode typescript-tsx-mode css-mode scss-mode json-mode web-mode) . prettier-js-mode)
  :config
  (setq prettier-js-show-errors nil))

;; Configure project management for frontend projects
(use-package project
  :config
  ;; Recognize common frontend project root markers
  (add-to-list 'project-find-functions 'project-try-vcs))

(provide 'frontend-tools)
;;; frontend-tools.el ends here