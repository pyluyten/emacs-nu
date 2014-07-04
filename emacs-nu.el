(require 'nu-common)
(require 'nu-hooks)


(define-minor-mode nu-mode
  "Simple Emacs keys"
  :global t
  :keymap (let ((map (make-sparse-keymap)))


; all _control_ features (mostly, prompts..)

; modern
;  do not respect _any_ emacs convention. Seriously.
;  i should just 
   (define-key map (kbd "C-q") 'keyboard-escape-quit)
   (define-key map (kbd "C-w") 'nu-close-tab)
   (define-key map (kbd "C-e") 'nu-copy-from-above)
   (define-key map (kbd "C-r") 'nu-replace-prompt)
   (define-key map (kbd "C-t") 'nu-new-tab)
   (define-key map (kbd "C-y") 'nu-copy-from-below)
   (define-key map (kbd "C-u") 'backward-kill-word)
   ; C-i is tab.
   (define-key map (kbd "C-o") 'find-file)
   (define-key map (kbd "C-p") 'print-buffer)

   (define-key map (kbd "C-a") 'mark-whole-buffer) ; tbp
   (define-key map (kbd "C-s") 'save-buffer)
   ; C-d is not yet defined. Soon we'll need it.
   (define-key map (kbd "C-f") 'nu-find-prompt)
   (define-key map (kbd "C-g") 'nu-global-prompt)
   (define-key map (kbd "C-j") 'backward-delete-char)
   (define-key map (kbd "C-k") 'kill-visual-line)
   (define-key map (kbd "C-l") 'delete-forward-char)
   ; C-m stands for enter.

   (define-key map (kbd "C-z") 'undo-tree-visualize)
   ; C-x is cua-mode for now
   ; C-c is cua-mode for now
   (define-key map (kbd "C-v") 'yank)



;  _ alt _ features

   (define-key map (kbd "M-q") 'quoted-insert) ; fix minibuf'
;w
;e
;r
;t
;y
   (define-key map (kbd "M-u") 'backward-word)
   (define-key map (kbd "M-i") 'previous-line)
   (define-key map (kbd "M-o") 'forward-word)
;p
;a
;s
;d
;f  -- todo : find char a-la-vim
;g
   (define-key map (kbd "M-j") 'backward-char)
   (define-key map (kbd "M-k") 'next-line)
   (define-key map (kbd "M-l") 'forward-char)
;m
;z
;x
;c
   (define-key map (kbd "M-v") 'yank-pop) ; tbi / tbp
;b
;n
   (define-key map (kbd "M-0") 'nu-back-to-bol)
   (define-key map (kbd "M-^") 'nu-back-to-indentation) ; fails here.
   (define-key map (kbd "M-à") 'nu-back-to-indentation)
   (define-key map (kbd "M-$") 'nu-end-of-line)

; end of map...
           map)

; minor-mode-body...
   (undo-tree-mode 1)
   (cua-mode t))


(provide 'emacs-nu)
