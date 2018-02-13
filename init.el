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
	
(local-set-key [tab] 'tab-to-tab-stop)
;;unindent?

(global-set-key (kbd "C-w") 'kill-whole-line)

(org-babel-load-file (expand-file-name "~/.emacs.d/setup.org"))

