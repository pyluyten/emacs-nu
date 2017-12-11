(require 'nu-prompters)
(require 'evil)

(defgroup nu nil
  "Emulate modern key bindings and provides popups."
  :prefix "nu"
  :group 'editing-basics
  :group 'emulations)

(defalias 'nu-prompt-for-keymap 'nu-light-prompt-for-keymap)

(defalias 'nu-full-prompt-for-keymap 'nu-buffer-prompt-for-keymap)


(defface nu-face-shortcut
  '((((class color) (background light))
     :foreground "#0f4b77" :bold t)
    (((class color) (background dark))
     :foreground "#b1d5ef" :bold t))
  "nu-face-shortcut")

;;
;; PADDLE
;;

;; default is
;;    I
;;  J K L
;;
;;  both j / k / l also allow to delete

(defun nu-push-paddle-to-keymap ()
  (define-key evil-emacs-state-map nu-previous-line-binding 'previous-line)
  (define-key evil-emacs-state-map nu-backward-char-binding 'backward-char)
  (define-key evil-emacs-state-map nu-next-line-binding 'next-line)
  (define-key evil-emacs-state-map nu-forward-char-binding 'forward-char)
  (define-key evil-emacs-state-map nu-back-to-indentation-binding 'nu-back-to-indentation)

  (define-key evil-emacs-state-map nu-kill-visual-line-key 'kill-visual-line)
  (define-key evil-emacs-state-map nu-del-backward-char-key 'delete-backward-char)
  (define-key evil-emacs-state-map nu-del-forward-char-key 'delete-forward-char))


(defun nu-setup-classic-paddle ()
 (setq nu-previous-line-binding (kbd "M-i"))
 (setq nu-backward-char-binding (kbd "M-j"))
 (setq nu-next-line-binding (kbd "M-k"))
 (setq nu-forward-char-binding (kbd "M-l"))
 (setq nu-back-to-indentation-binding (kbd "M-h"))

 (setq nu-previous-line-key "i")
 (setq nu-backward-char-key "j")
 (setq nu-next-line-key "k")
 (setq nu-forward-char-key "l")
 (setq nu-back-to-indentation-key "h")

 (setq nu-kill-visual-line-key (kbd "C-k"))
 (setq nu-del-backward-char-key (kbd "C-j"))
 (setq nu-del-forward-char-key (kbd "C-l"))

 (nu-push-paddle-to-keymap))

;;
;; vi paddle
;; since sometimes the h serves for help in classic paddle
;; in vi paddle this is i (as in "info")
;;

(defun nu-setup-vi-paddle ()
 (setq nu-previous-line-binding (kbd "M-k"))
 (setq nu-backward-char-binding (kbd "M-h"))
 (setq nu-next-line-binding (kbd "M-j"))
 (setq nu-forward-char-binding (kbd "M-l"))
 (setq nu-back-to-indentation-binding (kbd "M-i"))

 (setq nu-previous-line-key "k")
 (setq nu-backward-char-key "h")
 (setq nu-next-line-key "j")
 (setq nu-forward-char-key "l")
 (setq nu-back-to-indentation-key "i")

 (setq nu-kill-visual-line-key (kbd "C-j"))
 (setq nu-del-backward-char-key (kbd "C-k"))
 (setq nu-del-forward-char-key (kbd "C-l"))

 (nu-push-paddle-to-keymap))


;; framework
;; here is just basic framework
;; other files define alternatives.

(defun nu-setup-basic ()
  ;; completion prompt setup.
  (defalias 'nu-completion-prompt-for-keymap 'nu-completing-read-prompt-for-keymap)

  ;; usual commands.
  (defalias 'nu-search 'nu-isearch-forward-regexp)
  (defalias 'nu-M-x 'execute-extended-command)
  (defalias 'nu-find-files 'find-file)
  (defalias 'nu-buffers-list 'ibuffer)
  (defalias 'nu-describe-function 'describe-function)
  (defalias 'nu-describe-variable 'describe-variable)
  (defalias 'nu-bookmarks 'list-bookmarks)
  (defalias 'nu-recentf 'recentf-open-files)
  (defalias 'nu-browse-kill-ring 'browse-kill-ring)
  (defalias 'nu-view-buffer-other-window 'view-buffer-other-window))

;; framework for ido
(defun nu-setup-ido ()
  ;; completion prompt setup.
  (defalias 'nu-completion-prompt-for-keymap 'nu-completing-read-prompt-for-keymap)

  ;; usual commands.
  (defalias 'nu-search 'nu-isearch-forward-regexp)
  (defalias 'nu-M-x 'execute-extended-command)
  (defalias 'nu-find-files 'ido-find-file)
  (defalias 'nu-buffers-list 'ibuffer)
  (defalias 'nu-describe-function 'describe-function)
  (defalias 'nu-describe-variable 'describe-variable)
  (defalias 'nu-bookmarks 'list-bookmarks)
  (defalias 'nu-recentf 'recentf-open-files)
  (defalias 'nu-browse-kill-ring 'browse-kill-ring)
  (defalias 'nu-view-buffer-other-window 'ido-switch-buffer-other-window))

(setq aw-keys '(?k ?l ?j ?i ?u ?o ?a ?k ?p ?m))

(defvar aw-dispatch-alist
'((?d aw-delete-window " Ace - Delete Window")
    (?s aw-swap-window " Ace- Swap Window")
    (?x aw-flip-window)
    (?c aw-split-window-fair " Ace - Split Fair Window")
    (?v aw-split-window-vert " Ace - Split Vert Window")
    (?b aw-split-window-horz " Ace - Split Horz Window")
    (?g delete-other-windows " Ace - Maximize Window")
    (?n delete-other-windows))
"List of actions for `aw-dispatch-default'.")


(provide 'nu-setup)

