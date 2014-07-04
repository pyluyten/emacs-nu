(defun nu-prepare-for-isearch ()
  (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "C-r") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "C-j") 'isearch-repeat-backward))

(defun nu-prepare-for-ibuffer ()
  ; use <space> to go down. i is enough
  (define-key ibuffer-mode-map (kbd "M-i") 'ibuffer-backward-line)
  (define-key ibuffer-mode-map (kbd "M-k") 'ibuffer-forward-line))

;(defun nu-prepare-for-minibuffer ()
;  "Restore tab for completion."
;  (define-key evil-insert-state-map (kbd "C-i") 'minibuffer-complete))


;(defun nu-leave-minibuffer ()
;  "Restore tab for previous."
;  (define-key evil-insert-state-map (kbd "C-i") 'open-line))

(defun nu-prepare-for-dired ()
  ; use <space> to go down. i is enough
  (define-key dired-mode-map  (kbd "M-i") 'dired-previous-line)
  (define-key dired-mode-map  (kbd "M-k") 'dired-next-line))


;(add-hook 'minibuffer-setup-hook 'nu-prepare-for-minibuffer)
;(add-hook 'minibuffer-exit-hook  'nu-leave-minibuffer)
(add-hook 'ibuffer-hook          'nu-prepare-for-ibuffer)
(add-hook 'isearch-mode-hook     'nu-prepare-for-isearch)
(add-hook 'dired-mode-hook       'nu-prepare-for-dired)

(provide 'nu-hooks)
