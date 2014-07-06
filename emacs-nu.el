(require 'nu-common)
(require 'nu-hooks)



(define-minor-mode nu-mode
  "Simple Emacs keys"
  :global t
  :keymap (let ((map (make-sparse-keymap)))


;  all _control_ features (mostly, prompts..)
;  do not respect _any_ emacs convention. Seriously.

   (define-key map (kbd "C-q") 'keyboard-escape-quit)
   (define-key map (kbd "C-w") 'nu-close-tab)
   ; e? edit?
   (define-key map (kbd "C-r") 'nu-replace-prompt)
   (define-key map (kbd "C-t") 'nu-new-tab)
   ; y?
   (define-key map (kbd "C-u") 'backward-kill-word)
   ; C-i is tab.
   (define-key map (kbd "C-o") 'find-file)
   (define-key map (kbd "C-p") 'print-buffer)

   (define-key map (kbd "C-a") 'nu-all-prompt) ; tbp: mark paragraph/function/...
   (define-key map (kbd "C-s") 'save-buffer)
   ; C-d is not yet defined. (x to delete, c to copy... remain <D>irect[ion] <D>rill <Do> <Define>...
   (define-key map (kbd "C-f") 'nu-find-prompt)
   (define-key map (kbd "C-g") 'nu-global-prompt)
   (define-key map (kbd "C-h") 'nu-help-prompt)
   (define-key map (kbd "C-j") 'backward-delete-char)
   (define-key map (kbd "C-k") 'kill-visual-line) ; k=kill, but how to advertise it?
   (define-key map (kbd "C-l") 'delete-forward-char)
   ; C-m stands for enter. This is something to study.

   (define-key map (kbd "C-z") 'undo-tree-visualize) ; you don't undo 1 thousand times a day. be smart.
   ; C-x is cua-mode
   ; C-c is cua-mode
   (define-key map (kbd "C-v") 'yank) ;shall we prompt? alt v is fine but we might need a message to advertise it.



;  all _ alt _ features

   (define-key map (kbd "M-q") 'quoted-insert) ; fix minibuf'
   (define-key map (kbd "M-w") 'delete-other-windows)
   (define-key map (kbd "M-e") 'nu-copy-from-above) ; advertise?
;r
;t
   (define-key map (kbd "M-y") 'nu-copy-from-below) ; how to advertise?
   (define-key map (kbd "M-u") 'backward-word)
   (define-key map (kbd "M-i") 'previous-line)
   (define-key map (kbd "M-o") 'forward-word)
;p
   (define-key map (kbd "M-a") 'mark-whole-buffer)
;s
;d
;f  -- todo : find char a-la-vim
;g
   (define-key map (kbd "M-j") 'backward-char)
   (define-key map (kbd "M-k") 'next-line)
   (define-key map (kbd "M-l") 'forward-char)
;m
   (define-key map (kbd "M-z") 'undo)
;x let's keep altx for some time here before to decide.
;c
   (define-key map (kbd "M-v") 'yank-pop) ; tbi / tbp
;b
;n

   (define-key map (kbd "²") 'ibuffer) ; to be advertised (global?)
   (define-key map (kbd "M-²") 'next-buffer) ; to be advertised
   (define-key map (kbd "M-0") 'nu-back-to-bol)
   (define-key map (kbd "M-^") 'nu-back-to-indentation) ; fails here.
   (define-key map (kbd "M-à") 'nu-back-to-indentation)
   (define-key map (kbd "M-$") 'nu-end-of-line)

; end of map...
           map)

; minor-mode-body...
   (set-default 'cursor-type 'bar)
   (undo-tree-mode 1)
   (cua-mode))


(provide 'emacs-nu)
