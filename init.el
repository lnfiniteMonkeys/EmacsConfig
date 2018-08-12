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

;; load the timelines mode
(require 'timelines "timelines-mode.el")

;; load the TimeLines setup file
(org-babel-load-file (expand-file-name "~/.emacs.d/timelines-setup.org"))

;; load any additional files if they exist
(let ((qwerty-to-dvorak "qwerty-to-dvorak.el")
      (additional-settings "additional-settings.el")
      (local-settings "local-settings.el"))
  (when (file-exists-p qwerty-to-dvorak)
   (load-file qwerty-to-dvorak))
  (when (file-exists-p additional-settings)
   (load-file additional-settings))
  (when (file-exists-p local-settings)
   (load-file local-settings))
 )

(global-set-key [tab] 'indent-for-tab-command)

