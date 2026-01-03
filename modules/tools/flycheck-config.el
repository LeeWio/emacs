;;; flycheck-config.el --- Enhanced Flycheck configuration for frontend development -*- lexical-binding: t -*-

;;; Commentary:
;; Enhanced Flycheck configuration with better error display and integration

;;; Code:

;; Configure flycheck UI enhancements
(use-package flycheck
  :ensure t
  :config
  ;; Configure error display
  (setq flycheck-indication-mode 'left-fringe
        flycheck-highlighting-mode 'symbols
        flycheck-display-errors-delay 0.3
        flycheck-idle-change-delay 0.3
        flycheck-completion-system 'childframe)
  
  ;; Use more descriptive error levels
  (flycheck-define-error-level 'error
    :severity 100
    :compilation-level 2
    :highlighting-fn 'flycheck-error-overlay-face
    :fringe-bitmap 'exclamation-mark
    :fringe-face 'flycheck-fringe-error
    :error-list-face 'flycheck-error-list-error)
  
  (flycheck-define-error-level 'warning
    :severity 10
    :compilation-level 1
    :highlighting-fn 'flycheck-warning-overlay-face
    :fringe-bitmap 'question-mark
    :fringe-face 'flycheck-fringe-warning
    :error-list-face 'flycheck-error-list-warning)
  
  ;; Enable automatic mode
  (global-flycheck-mode)

  ;; Configure error display function
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

;; Integrate with corfu for better completion of error messages
;; Only add flycheck completion if the function exists and is safe to use
(with-eval-after-load 'corfu
  (add-hook 'flycheck-mode-hook
            (lambda ()
              (when (fboundp 'flycheck-completion-at-point)
                (setq-local completion-at-point-functions
                            (cons #'flycheck-completion-at-point
                                  completion-at-point-functions))))))

;; Add history for tsx-ts-mode to prevent error
(with-eval-after-load 'consult
  (add-to-list 'consult-mode-histories '(tsx-ts-mode . minibuffer-history))
  (add-to-list 'consult-mode-histories '(js-ts-mode . minibuffer-history))
  (add-to-list 'consult-mode-histories '(typescript-ts-mode . minibuffer-history)))

;; Configure flycheck-posframe for better error popups (if available)
(use-package flycheck-posframe
  :ensure t
  :when (display-graphic-p)  ; Only in GUI mode, not needed for terminal
  :config
  (flycheck-posframe-mode))

;; Configure error navigation
(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map (kbd "C-c n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "C-c p") 'flycheck-previous-error)
  (define-key flycheck-mode-map (kbd "C-c l") 'flycheck-list-errors)
  (define-key flycheck-mode-map (kbd "C-c c") 'flycheck-compile))

(provide 'flycheck-config)
;;; flycheck-config.el ends here