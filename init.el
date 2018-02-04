(require 'package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(initial-buffer-choice t)
 '(menu-bar-mode nil)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 102 :width normal :foundry "GOOG" :family "Office Code Pro"))))
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("melpa-stable" . "http://stable.melpa.org/packages/")))))


 (custom-set-variables
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t))
 (eval-after-load 'haskell-mode '(progn
   (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
   (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
   (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
   (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
   (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
   (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
 (eval-after-load 'haskell-cabal '(progn
   (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))


;;;;;;;;;;;;;;;;;
; buffer-stack ;;
;;;;;;;;;;;;;;;;;

(setq buffer-stack-show-position 'buffer-stack-show-position-buffers)

(autoload 'buffer-stack-down "buffer-stack"  nil t)
(autoload 'buffer-stack-up "buffer-stack"  nil t)
(autoload 'buffer-stack-bury-and-kill "buffer-stack"  nil t)
(autoload 'buffer-stack-bury "buffer-stack"  nil t)
(eval-after-load "buffer-stack" '(require 'buffer-stack-suppl))

(global-set-key [(f10)] 'buffer-stack-bury)
(global-set-key [(control f10)] 'buffer-stack-bury-and-kill)
(global-set-key [(f9)] 'buffer-stack-down)
(global-set-key [(f11)] 'buffer-stack-up)
(global-set-key [(shift f10)] 'buffer-stack-bury-thru-all)
(global-set-key [(shift f9)] 'buffer-stack-down-thru-all)
(global-set-key [(shift f11)] 'buffer-stack-up-thru-all)N
