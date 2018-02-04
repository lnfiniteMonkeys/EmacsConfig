;; Window settings
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq frame-title-format "TimeLines")
;; Cursor settings
(blink-cursor-mode 0)
(set-default 'cursor-type 'box)
(set-cursor-color "#ff1493") 


(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; Package archive setup
(require 'package)
(setq package-archives '(("melpa" . "https://stable.melpa.org/packages/")  ;;("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")))

(package-initialize)



;; Install use-package if it's not already installed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; use-package setup, auto install packages that are not present
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))




(use-package haskell-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(use-package paredit)
(use-package magit)
(use-package monokai-theme)
(use-package ghc)
(use-package org)
(use-package smex)


;; QWERTY -> DVORAK while keeping all keybindngs the same
;; First row
(define-key key-translation-map (kbd "q") (kbd "'"))
(define-key key-translation-map (kbd "w") (kbd ","))
(define-key key-translation-map (kbd "e") (kbd "."))
(define-key key-translation-map (kbd "r") (kbd "p"))
(define-key key-translation-map (kbd "t") (kbd "y"))
(define-key key-translation-map (kbd "y") (kbd "f"))
(define-key key-translation-map (kbd "u") (kbd "g"))
(define-key key-translation-map (kbd "i") (kbd "c"))
(define-key key-translation-map (kbd "o") (kbd "r"))
(define-key key-translation-map (kbd "p") (kbd "l"))
(define-key key-translation-map (kbd "[") (kbd "/"))
(define-key key-translation-map (kbd "]") (kbd "="))

(define-key key-translation-map (kbd "Q") (kbd "\""))
(define-key key-translation-map (kbd "W") (kbd "<"))
(define-key key-translation-map (kbd "E") (kbd ">"))
(define-key key-translation-map (kbd "R") (kbd "P"))
(define-key key-translation-map (kbd "T") (kbd "Y"))
(define-key key-translation-map (kbd "Y") (kbd "F"))
(define-key key-translation-map (kbd "U") (kbd "U"))
(define-key key-translation-map (kbd "I") (kbd "C"))
(define-key key-translation-map (kbd "O") (kbd "R"))
(define-key key-translation-map (kbd "P") (kbd "N"))
(define-key key-translation-map (kbd "{") (kbd "?"))
(define-key key-translation-map (kbd "}") (kbd "+"))

;; Second row
(define-key key-translation-map (kbd "a") (kbd "a"))
(define-key key-translation-map (kbd "s") (kbd "o"))
(define-key key-translation-map (kbd "d") (kbd "e"))
(define-key key-translation-map (kbd "f") (kbd "u"))
(define-key key-translation-map (kbd "g") (kbd "i"))
(define-key key-translation-map (kbd "h") (kbd "d"))
(define-key key-translation-map (kbd "j") (kbd "h"))
(define-key key-translation-map (kbd "k") (kbd "t"))
(define-key key-translation-map (kbd "l") (kbd "n"))
(define-key key-translation-map (kbd ";") (kbd "s"))
(define-key key-translation-map (kbd "'") (kbd "-"))

(define-key key-translation-map (kbd "A") (kbd "A"))
(define-key key-translation-map (kbd "S") (kbd "O"))
(define-key key-translation-map (kbd "D") (kbd "E"))
(define-key key-translation-map (kbd "F") (kbd "U"))
(define-key key-translation-map (kbd "G") (kbd "I"))
(define-key key-translation-map (kbd "H") (kbd "D"))
(define-key key-translation-map (kbd "J") (kbd "H"))
(define-key key-translation-map (kbd "K") (kbd "T"))
(define-key key-translation-map (kbd "L") (kbd "N"))
(define-key key-translation-map (kbd ":") (kbd "S"))
(define-key key-translation-map (kbd "\"") (kbd "_"))

;; Third row
(define-key key-translation-map (kbd "z") (kbd ";"))
(define-key key-translation-map (kbd "x") (kbd "q"))
(define-key key-translation-map (kbd "c") (kbd "j"))
(define-key key-translation-map (kbd "v") (kbd "k"))
(define-key key-translation-map (kbd "b") (kbd "x"))
(define-key key-translation-map (kbd "n") (kbd "b"))
(define-key key-translation-map (kbd "m") (kbd "m"))
(define-key key-translation-map (kbd ",") (kbd "w"))
(define-key key-translation-map (kbd ".") (kbd "v"))
(define-key key-translation-map (kbd "/") (kbd "z"))

(define-key key-translation-map (kbd "Z") (kbd ":"))
(define-key key-translation-map (kbd "X") (kbd "Q"))
(define-key key-translation-map (kbd "C") (kbd "J"))
(define-key key-translation-map (kbd "V") (kbd "K"))
(define-key key-translation-map (kbd "B") (kbd "X"))
(define-key key-translation-map (kbd "N") (kbd "B"))
(define-key key-translation-map (kbd "M") (kbd "M"))
(define-key key-translation-map (kbd "<") (kbd "W"))
(define-key key-translation-map (kbd ">") (kbd "V"))
(define-key key-translation-map (kbd "?") (kbd "Z"))


;; Misc
(define-key key-translation-map (kbd "-") (kbd "["))
(define-key key-translation-map (kbd "=") (kbd "]"))

(define-key key-translation-map (kbd "_") (kbd "{"))
(define-key key-translation-map (kbd "+") (kbd "}"))
