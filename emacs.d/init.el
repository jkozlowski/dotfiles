;; Package locations

(let ((default-directory (file-name-directory (or load-file-name (buffer-file-name)))))
  (add-to-list 'load-path default-directory)
  (add-to-list 'load-path (concat default-directory "/packages/haskell-mode"))
  (add-to-list 'load-path (concat default-directory "/packages/auto-complete"))
  (add-to-list 'load-path (concat default-directory "/packages/s"))
  (add-to-list 'load-path (concat default-directory "/packages/f"))
  (add-to-list 'load-path (concat default-directory "/packages/dash"))
  (add-to-list 'load-path (concat default-directory "/packages/cl-lib"))
  (add-to-list 'load-path (concat default-directory "/packages/flycheck"))
  (add-to-list 'load-path (concat default-directory "/packages/color-theme"))
  (add-to-list 'load-path (concat default-directory "/packages/zenburn")))

;; Loading dependencies

(load "haskell-mode-autoloads.el")
(require 'cl-lib)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'flycheck)
(require 'fpco-mode)
;; Optional
(require 'color-theme)
(require 'zenburn)

;; Activates haskell mode for approprioate extensions

(add-to-list 'auto-mode-alist (cons "\\.hs\\'" 'haskell-mode))
(add-to-list 'auto-mode-alist (cons "\\.cabal\\'" 'haskell-cabal-mode))
(add-to-list 'auto-mode-alist '("\\.hcr\\'" . haskell-core-mode))

;; Haskell-mode configuration

(add-hook 'haskell-mode-hook 'auto-complete-mode)
(add-hook 'haskell-mode-hook 'flycheck-mode)
;; (add-hook 'haskell-mode-hook 'fpco-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(ac-config-default)
(eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

(eval-after-load "haskell-cabal"
    '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

;; General Emacs customizations (optional)

(windmove-default-keybindings 'meta)
(global-set-key (kbd "C-z") 'ido-switch-buffer)
(ido-mode 1)
(tool-bar-mode -1)
;; (scroll-bar-mode -1)
(setq ac-ignore-case t)
(zenburn)

;; FP Complete configuration

;;(fpco/start)
;;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(safe-local-variable-values (quote ((eval set (make-local-variable (quote fpco-root)) (expand-file-name (locate-dominating-file buffer-file-name ".dir-locals.el"))) (fpco-pid . 19258)))))
;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; )
