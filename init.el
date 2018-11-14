;;;;;; Initialize package manager and archives
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(package-initialize)
(package-refresh-contents)


;; Install use-package if it's not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;; load the TimeLines setup file
(org-babel-load-file (expand-file-name "~/.emacs.d/timelines-setup.org"))

;; load any additional config files if they exist
;; you can add your own here
(let ((qwerty-to-dvorak "~/.emacs.d/qwerty-to-dvorak.el")
      (additional-settings "~/.emacs.d/additional-settings.el")
      (local-settings "~/.emacs.d/local-settings.el"))
  (when (file-exists-p qwerty-to-dvorak)
   (load-file qwerty-to-dvorak))
  (when (file-exists-p additional-settings)
   (load-file additional-settings))
  (when (file-exists-p local-settings)
   (load-file local-settings))
  )


;; load the timelines mode
(require 'timelines "~/.emacs.d/timelines-mode.el")

(global-set-key [tab] 'indent-for-tab-command)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (expand-region helm org-ref intero elpy yasnippet which-key use-package smartparens scroll-restore org-pdfview monokai-theme magit ido-vertical-mode hydra ghc avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
