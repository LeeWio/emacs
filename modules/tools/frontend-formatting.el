;;; frontend-formatting.el --- Frontend formatting tools -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for frontend development formatting tools like prettier, eslint

;;; Code:

;; Prettier integration for formatting
(use-package prettier-js
  :ensure t
  :hook ((js-mode js-jsx-mode tsx-ts-mode typescript-mode typescript-tsx-mode css-mode scss-mode json-mode web-mode) . prettier-js-mode)
  :config
  (setq prettier-js-show-errors nil))

;; Configure automatic formatting with eglot and prettier
(defun my-frontend-format-buffer ()
  "Format buffer using eglot first, then prettier if available."
  (interactive)
  (condition-case nil
      (eglot-format-buffer)
    (error
     (when (fboundp 'prettier-js)
       (prettier-js)))))

;; Key binding for formatting
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c f") 'my-frontend-format-buffer))

(provide 'frontend-formatting)
;;; frontend-formatting.el ends here