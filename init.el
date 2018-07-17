;;;;;; PACKAGES
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(package-initialize)
(package-refresh-contents)


;; Install use-package if it's not already installed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

(global-set-key [tab] 'indent-for-tab-command)
;;unindent?
	
(global-set-key (kbd "C-w") 'kill-whole-line)

(org-babel-load-file (expand-file-name "~/.emacs.d/setup.org"))

(require 'timelines "~/.emacs.d/timelines.el")

(require 'ox-latex)
(add-to-list (quote org-latex-classes) (quote ("dissertation" "\\documentclass{report}" ("\\chapter{%s}" . "\\chapter*{%s}") ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}") ("\\paragraph{%s}" . "\\paragraph*{%s}"))))


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (haskell . t)))


(setq exec-path (append exec-path '("C:/Users/Carl/AppData/Local/Programs/MiKTeX 2.9/miktex/bin/x64")))

(use-package elpy)
(elpy-enable)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (intero elpy yasnippet which-key use-package smartparens scroll-restore org-pdfview monokai-theme magit ido-vertical-mode hydra ghc avy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
