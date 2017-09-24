
(defun nu-setup-ivy ()
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
  (defalias 'nu-Mx 'execute-extended-command)
  (defalias 'nu-find-files 'find-file)
  (defalias 'nu-buffers-list 'ibuffer)
  (defalias 'nu-describe-function 'describe-function)
  (defalias 'nu-describe-variable 'describe-variable)
  (defalias 'nu-bookmarks 'list-bookmarks)
  (defalias 'nu-recentf 'recentf-open-files)
  (defalias 'nu-browse-kill-ring 'browse-kill-ring))


(defun nu-setup-helm ()
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

