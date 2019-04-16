;; This mode was adapted from TidalCycles' own Emacs major mode
;; which offers very similar functionality, namely providing
;; Haskell syntax highlighting and formatting and communication
;; with a GHCi interpreter. The original can be found here:
;; https://github.com/tidalcycles/Tidal/blob/master/tidal.el

(require 'scheme)
(require 'comint)
(require 'thingatpt)
(require 'find-lisp)
(require 'pulse)
(require 'haskell-mode)

(defvar timelines-buffer
  "*timelines*"
  "*The name of the TimeLines process buffer (default = *timelines*).")


(defvar timelines-path-to-src
  "~/timelines"
  "*The path to the source files to be loaded on startup (default = '~/timelines')")

(defvar timelines-interpreter
  "stack"
  "*The haskell interpeter to use (default = stack).")

(defvar timelines-interpreter-args
  "ghci"
  "*Arguments for the Haskell interpreter (default = ghci).")
;;;;;;;;;;;Interactives
(defun timelines-load-src ()
    (interactive)
    (timelines-send-string ":l Sound.TimeLines.Context"))

(defun timelines-reset-server ()
  (interactive)
  (timelines-send-string "reset"))

(defun timelines-set-window ()
  (interactive)
  (let ((s (read-from-minibuffer "Start: "))
	(e (read-from-minibuffer "End: ")))
  (timelines-send-string (concat "window " s " " e))))

(defun timelines-play ()
  (interactive)
  (timelines-send-string "sendMessage \"/TimeLines/play\" \"\""))

(defun timelines-loop-on ()
  (interactive)
  (timelines-send-string "sendMessage \"/TimeLines/setLoop\" \"1\""))

(defun timelines-loop-off ()
  (interactive)
  (timelines-send-string "sendMessage \"/TimeLines/setLoop\" \"0\""))

(defun timelines-start ()
  "Start TimeLines."
  (interactive)
  (if (comint-check-proc timelines-buffer)
      (error "A TimeLines process is already running")
    (let ((default-directory timelines-path-to-src))
      (make-comint "timelines" timelines-interpreter nil timelines-interpreter-args))
    (delete-other-windows)
    (timelines-show-output)
    (timelines-send-string ":script Boot.hs")))

(defun timelines-reset ()
    (interactive)
  (timelines-reset-server))

(defun timelines-show-output ()
  "Show haskell output."
  (interactive)
  (when (comint-check-proc timelines-buffer)
    (delete-other-windows)
    ;;(split-window-vertically -10)
    (with-current-buffer timelines-buffer
      (let ((window (display-buffer (current-buffer))))
	(goto-char (point-max))
	(save-selected-window
	  (set-window-point window (point-max)))))))

(defun timelines-chunk-string (n s)
  "Split a string S into chunks of N characters."
  (let* ((l (length s))
         (m (min l n))
         (c (substring s 0 m)))
    (if (<= l n)
        (list c)
      (cons c (timelines-chunk-string n (substring s n))))))

(defun timelines-send-string (s)
  (if (comint-check-proc timelines-buffer)
      (let ((cs (timelines-chunk-string 64 (concat s "\n"))))
        (mapcar (lambda (c) (comint-send-string timelines-buffer c)) cs))
    (error "no TimeLines process ")))

(defun timelines-run-line ()
  "Send the current line to the interpreter."
  (interactive)
  (let* ((s (buffer-substring-no-properties
	     (line-beginning-position)
	     (line-end-position))))
    (timelines-send-string s))
  (pulse-momentary-highlight-one-line (point))
  ;;(forward-line)
  )

(defun timelines-send-region ()
  "Eval the current region as a single line"
  (mark-paragraph)
  (let* ((s (buffer-substring-no-properties (region-beginning)
					    (region-end))))
    (timelines-send-string ":{")
    (timelines-send-string s)
    (timelines-send-string ":}")
    (mark-paragraph)
    (next-line)
    (beginning-of-line)
    (exchange-point-and-mark)
    (left-char)
    (pulse-momentary-highlight-region (mark) (point)))
  )

(defun timelines-eval-region ()
  "Send the current region to the interpreter as a single line."
  (interactive)
  (if (>= emacs-major-version 25)
      (save-mark-and-excursion
       (timelines-send-region))
    (save-excursion
     (timelines-send-region))))

(defun timelines-interrupt-haskell ()
  (interactive)
  (if (comint-check-proc timelines-buffer)
      (with-current-buffer timelines-buffer
	(interrupt-process (get-buffer-process (current-buffer))))
    (error "no process running?")))

(defun timelines-quit-haskell ()
  "Quit haskell."
  (interactive)
  (kill-buffer timelines-buffer)
  (delete-other-windows))

(defvar timelines-mode-map nil
  "Timelines keymap.")

;;(global-set-key (kbd "C-t") nil)

(defun timelines-mode-keybindings (map)
  "Haskell Timelines keybindings."
  (define-key map (kbd "C-c C-s") 'timelines-start)
  (define-key map (kbd "C-t C-t") 'timelines-show-output)
  (define-key map (kbd "C-c C-q") 'timelines-quit-haskell)
  (define-key map (kbd "C-c C-e") 'timelines-load-src)
  (define-key map (kbd "<C-return>") 'timelines-eval-region)
  (define-key map (kbd "C-c C-r") 'timelines-reset)
  (define-key map (kbd "C-c C-w") 'timelines-set-window)
  (define-key map (kbd "C-c C-j") 'timelines-play)
  (define-key map (kbd "C-c C-k") 'timelines-loop-on)
  (define-key map (kbd "C-c C-l") 'timelines-loop-off)
  )

(defun timelines-turn-on-keybindings ()
  "Haskell Timelines keybindings in the local map."
  (local-set-key (kbd "C-t C-s") 'timelines-start)
  (local-set-key (kbd "C-t C-t") 'timelines-show-output)
  (local-set-key (kbd "C-t C-q") 'timelines-quit-haskell)
  (local-set-key (kbd "<C-return>") 'timelines-eval-region)
 )

(unless timelines-mode-map
  (let ((map (make-sparse-keymap "Haskell-Timelines")))
    (timelines-mode-keybindings map)
    (setq timelines-mode-map map)))


(define-derived-mode
  timelines-mode
  haskell-mode
  "Haskell Timelines"
  "Major mode for interacting with an inferior haskell process."
  (set (make-local-variable 'paragraph-start) "\f\\|[ \t]*$")
  (set (make-local-variable 'paragraph-separate) "[ \t\f]*$")
  (turn-on-font-lock))

(add-to-list 'auto-mode-alist '("\\.tl" . timelines-mode))

(provide 'timelines-mode)
;;
