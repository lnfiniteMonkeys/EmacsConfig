;;; An example config file to get you started with Emacs.
;;; It uses use-package for automatic package installation and management,
;;; it maintains the familiar-to-many Ctrl-c for copy, Ctrl-x for cut, Ctrl-v for paste.

;;;;;;;;;;;;;;;;; Setup package stuff
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("org"   . "http://orgmode.org/elpa/")))
(package-initialize)
(package-refresh-contents)

;; Install use-package if it's not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;; Haskell mode provides syntax highlighting and other goodies
(use-package haskell-mode)

;; Load up timelines-mode, the star of the evening
(load "~/.emacs.d/timelines-mode.el")

;;
;; ~~~ Change this if you cloned the TimeLines source repo in any other path ~~~
;;
(setq timelines-path "~/timelines")

;; Snippets = less typing = good
(use-package yasnippet
  :config 
  (yas-global-mode 1))

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)

;;;;;;;;;;;;;; Setup some familiar functionality defaults (e.g. the usual cut/copy/paste bindings)
(cua-mode t)
(transient-mark-mode 1)
(setq cua-keep-region-after-copy t)

;; Type 'y' or 'n' instead of 'yes' or 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;; Replaces a selection with any letter pressed
(delete-selection-mode 1)

(setq scroll-conservatively 100)

;;;;;;;;;;;;;; Setup themes and other aesthetic stuff

;; Set the mood, there's plenty of other themes to choose from here:
;; https://emacsthemes.com/popular/index.html
(use-package monokai-theme)

;; Set the window's name
(setq frame-title-format "TimeLines")

;; Remove unecessary distractions from the frame
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(line-number-mode 1)
(show-paren-mode 1)

;; Cursor settings
(blink-cursor-mode 0)
(set-default 'cursor-type 'box)
(set-cursor-color "#ff1493")

;; Highlight the line the cursor is on
(global-hl-line-mode t)

;; Operate on visual, rather than logical, lines
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Reload the init file with a key binding
(global-set-key (kbd "C-c p")
                (lambda() (interactive)
                  (org-babel-load-file "~/.emacs.d/init.el")))


;; Clean up startup behaviour
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq ring-bell-function 'ignore)

;; Go to any line with Alt-g
(global-set-key "\M-g" 'goto-line)

;; Instal ido for autocompletion
(use-package ido
  :config
  (ido-mode 1)
  (setq ido-enable-flex-matching nil)
  (setq ido-create-new-buffer 'always)
  (setq ido-everywhere t)
  (ido-mode 1))

(use-package ido-vertical-mode
  :init
  (ido-vertical-mode 1))

(setq initial-major-mode 'lisp-interaction-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ido-vertical-mode monokai-theme haskell-mode use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
