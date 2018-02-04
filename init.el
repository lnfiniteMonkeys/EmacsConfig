(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))


(setq inhibit-startup-message t)
(setq initial-scratch-message "")

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

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

(use-package haskell-mode)
(use-package ghc-mod)
(use-package org)
(use-package smex)
