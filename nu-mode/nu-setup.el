(defgroup nu nil
  "Emulate modern key bindings and provides popups."
  :prefix "nu"
  :group 'editing-basics
  :group 'emulations)

;;
;; PADDLE
;;

;; default is
;;    I
;;  J K L
;;
;;  both j / k / l also allow to delete

(defvar nu-previous-line-key (kbd "M-i") "Shortcut to move to left.")
(defvar nu-backward-char-key (kbd "M-j") "Shortcut to move to left.")
(defvar nu-next-line-key (kbd "M-k") "Shortcut to move to down.")
(defvar nu-forward-char-key (kbd "M-l") "Shortcut to move to right.")
(defvar nu-back-to-indentation-key (kbd "M-h") "Shortcut to move to right.")

(defvar nu-kill-visual-line-key (kbd "C-k") "Shortcuts to kill vline.")
(defvar nu-del-backward-char-key (kbd "C-j") "Shortcuts to backspace.")
(defvar nu-del-forward-char-key (kbd "C-l") "Shortcuts to backspace.")

(defun nu-push-paddle-to-keymap ()
  (define-key nu-keymap nu-previous-line-key 'previous-line)
  (define-key nu-keymap nu-backward-char-key 'backward-char)
  (define-key nu-keymap nu-next-line-key 'next-line)
  (define-key nu-keymap nu-forward-char-key 'forward-char)
  (define-key nu-keymap nu-back-to-indentation-key 'nu-back-to-indentation)

  (define-key nu-keymap nu-kill-visual-line-key 'kill-visual-line)
  (define-key nu-keymap nu-del-backward-char-key 'delete-backward-char)
  (define-key nu-keymap nu-del-forward-char-key 'delete-forward-char))


(defun nu-setup-classic-paddle ()
 (setq nu-previous-line-key (kbd "M-i"))
 (setq nu-backward-char-key (kbd "M-j"))
 (setq nu-next-line-key (kbd "M-k"))
 (setq nu-forward-char-key (kbd "M-l"))
 (setq nu-back-to-indentation-key (kbd "M-h"))

 (setq nu-kill-visual-line-key (kbd "C-k"))
 (setq nu-del-backward-char-key (kbd "C-j"))
 (setq nu-del-forward-char-key (kbd "C-l"))

 (nu-push-paddle-to-keymap))

(defun nu-setup-vi-paddle ()
 (setq nu-previous-line-key (kbd "M-k"))
 (setq nu-backward-char-key (kbd "M-h"))
 (setq nu-next-line-key (kbd "M-j"))
 (setq nu-forward-char-key (kbd "M-l"))
 (setq nu-back-to-indentation-key (kbd "M-i"))

 (setq nu-kill-visual-line-key (kbd "C-j"))
 (setq nu-del-backward-char-key (kbd "C-k"))
 (setq nu-del-forward-char-key (kbd "C-l"))

 (nu-push-paddle-to-keymap))


(defun nu-setup-ivy ()
  ;; completion prompt setup.
  (defalias 'nu-completion-prompt-for-keymap 'nu-ivy-prompt-for-keymap)

  ;; usual commands.
  (defalias 'nu-M-x 'counsel-M-x)
  (defalias 'nu-find-files 'counsel-find-file)
  (defalias 'nu-buffers-list 'ivy-switch-buffer)
  (defalias 'nu-describe-function 'counsel-describe-function)
  (defalias 'nu-describe-variable 'counsel-describe-variable)
  (defalias 'nu-bookmarks 'list-bookmarks)
  (defalias 'nu-recentf 'ivy-recentf)
  (defalias 'nu-browse-kill-ring 'counsel-yank-pop))

;; for now, by default nu is ivy
(nu-setup-ivy)

(defun nu-setup-basic ()
  ;; completion prompt setup.
  (defalias 'nu-completion-prompt-for-keymap 'nu-completing-read-prompt-for-keymap)

  ;; usual commands.  
  (defalias 'nu-Mx 'execute-extended-command)
  (defalias 'nu-find-files 'find-file)
  (defalias 'nu-buffers-list 'ibuffer)
  (defalias 'nu-describe-function 'describe-function)
  (defalias 'nu-describe-variable 'describe-variable)
  (defalias 'nu-bookmarks 'list-bookmarks)
  (defalias 'nu-recentf 'recentf-open-files)
  (defalias 'nu-browse-kill-ring 'browse-kill-ring))


(defun nu-setup-helm ()
  ;; completion prompt setup.
  (defalias 'nu-completion-prompt-for-keymap 'nu-helm-prompt-for-keymap)

  ;; usual commands.  
  (defalias 'nu-M-x 'helm-M-x)
  (defalias 'nu-find-files 'helm-find-files)
  (defalias 'nu-buffers-list 'helm-buffers-list)
  (defalias 'nu-describe-function 'describe-function)
  (defalias 'nu-describe-variable 'describe-variable)
  (defalias 'nu-bookmarks 'helm-bookmarks)
  (defalias 'nu-recentf 'helm-recent))


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

