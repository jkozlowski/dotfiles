;; Package locations
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

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
  (add-to-list 'load-path (concat default-directory "/packages/zenburn"))
  (add-to-list 'load-path (concat default-directory "/packages/ghc"))
  (add-to-list 'load-path (concat default-directory "/packages/projectile"))
  (add-to-list 'load-path (concat default-directory "/package/tabbar-mode")))

;; Ido
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; Put backup copied into temp
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

;; mouse support from http://stackoverflow.com/questions/5710334/how-can-i-get-mouse-selection-to-work-in-emacs-and-iterm2-on-mac
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e)) 
(global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
(global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))

;; The CUA mode will do 4 things:
;; 1) {Cut, Copy, Paste, Undo} have {X, C, V, Z} keys.
;; 2) Text selection will be highlighted. (this is default starting with emacs 23) 〔☛ New Features in Emacs 23〕
;; 3) When there's a text selection, typing will over-ride it.
;; 4) Text selection can be done by holding down the ⇧ Shift key and press a arrow key. (default behavior starting with emacs 23)
(cua-mode 1)

;; How to have emacs highlight text selections?
(transient-mark-mode 1) ; highlight text selection
(delete-selection-mode 1) ; delete seleted text when typing

;; How to show the cursor's column position?
(column-number-mode 1)

(tabbar-mode 1)

(global-hl-line-mode 1) ; turn on highlighting current line

(setq pop-up-frames t) ; t for true, nil for false

;; Nice selecting
 (if (fboundp 'pc-selection-mode)
        (pc-selection-mode)
      (require 'pc-select))

(require 'projectile)
(projectile-global-mode)
(eval-after-load "projectile"
  '(progn
     (define-key projectile-mode-map (kbd "C-t") 'projectile-find-file)))

;; I hate tabs!
(setq-default indent-tabs-mode nil)

;; Loading dependencies
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(load "haskell-mode-autoloads.el")

(require 'cl-lib)
(require 'auto-complete)
(require 'auto-complete-config)
;;(require 'flycheck)
(require 'fpco-mode)
(require 'sr-speedbar)
;; Optional
(require 'color-theme)
(require 'zenburn)

;; Activates haskell mode for approprioate extensions
(setq haskell-stylish-on-save t)
(add-to-list 'auto-mode-alist (cons "\\.hs\\'" 'haskell-mode))
(add-to-list 'auto-mode-alist (cons "\\.cabal\\'" 'haskell-cabal-mode))
(add-to-list 'auto-mode-alist '("\\.hcr\\'" . haskell-core-mode))

;; Haskell-mode configuration
;;(eval-after-load "haskell-mode"
;;  '(progn
;;    (define-key haskell-mode-map (kbd "C-x C-d") nil)
;;    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
;;    (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
;;    (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
;;    (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
;;    (define-key haskell-mode-map (kbd "C-c M-.") nil)
;;    (define-key haskell-mode-map (kbd "C-c C-d") nil)))
;;(add-hook 'haskell-mode-hook 'auto-complete-mode)
;;(add-hook 'haskell-mode-hook 'flycheck-mode)
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

