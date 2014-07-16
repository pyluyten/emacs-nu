(require 'nu-common)
(require 'nu-hooks)


(defvar nu-keymap (make-sparse-keymap) "Emacs nu keymap")



;  all _control_ features (mostly, prompts..)
;  do not respect _any_ emacs convention. Seriously.

   (define-key nu-keymap (kbd "C-q") 'keyboard-escape-quit)
   (define-key nu-keymap (kbd "C-w") 'nu-close-tab)
   ; e? edit?
   (define-key nu-keymap (kbd "C-r") 'nu-replace-prompt)
   (define-key nu-keymap (kbd "C-t") 'nu-new-tab)
   ; y?
   (define-key nu-keymap (kbd "C-u") 'backward-kill-word)
   ; C-i is tab.
   (define-key nu-keymap (kbd "C-o") 'nu-open-prompt)
   (define-key nu-keymap (kbd "C-p") 'print-buffer)

   (define-key nu-keymap (kbd "C-a") 'nu-all-prompt) ; tbp: mark paragraph/function/...
   (define-key nu-keymap (kbd "C-s") 'save-buffer)
   ; C-d is not yet defined. (x to delete, c to copy... remain <D>irect[ion] <D>rill <Do> <Define>...
   (define-key nu-keymap (kbd "C-f") 'nu-find-prompt)
   (define-key nu-keymap (kbd "C-g") 'nu-global-prompt)
   (define-key nu-keymap (kbd "C-h") 'nu-help-prompt)
           ; C-h uses help-keymap. Populate this.
           (define-key help-map (kbd "h") 'nu-help)
   (define-key nu-keymap (kbd "C-j") 'backward-delete-char)
   (define-key nu-keymap (kbd "C-k") 'kill-visual-line) ; k=kill, but how to advertise it?
   (define-key nu-keymap (kbd "C-l") 'delete-forward-char)
   ; C-m stands for enter. This is something to study.

   (define-key nu-keymap (kbd "C-z") 'undo-tree-visualize) ; you don't undo 1 thousand times a day. be smart.
   (define-key nu-keymap (kbd "C-x") 'nu-cut-region-or-line)
   (define-key nu-keymap (kbd "C-c") 'nu-copy-region-or-line)
   (define-key nu-keymap (kbd "C-v") 'nu-yank-pop-or-yank)
;b
   (define-key nu-keymap (kbd "C-n") 'nu-new-empty-buffer)

   (define-key nu-keymap (kbd "C-<SPC>") 'nu-trigger-mode-specific-map) ; C-C = 3
   (define-key nu-keymap (kbd "C-<RET>") 'repeat) ; does not work.
   (define-key nu-keymap (kbd "C-M-<SPC>") 'Control-X-prefix) ; well.. this is defintely not the _goal_ but...

   (define-key nu-keymap (kbd "C-<next>") 'next-buffer)
   (define-key nu-keymap (kbd "C-<prior>") 'previous-buffer)
;   (define-key nu-keymap (kbd "M-<next>") ')
;   (define-key nu-keymap (kbd "M-<prior>") ')



;  all _ alt _ features

   (define-key nu-keymap (kbd "M-q") 'quoted-insert) ; fix minibuf'
   (define-key nu-keymap (kbd "M-w") 'delete-other-windows)
   (define-key nu-keymap (kbd "M-e") 'nu-copy-from-above) ; advertise?
;r
;t
   (define-key nu-keymap (kbd "M-y") 'nu-copy-from-below) ; how to advertise?
   (define-key nu-keymap (kbd "M-u") 'backward-word)
   (define-key nu-keymap (kbd "M-i") 'previous-line)
   (define-key nu-keymap (kbd "M-o") 'forward-word)
;p
   (define-key nu-keymap (kbd "M-a") 'cua-set-mark)
   (define-key nu-keymap (kbd "M-s") 'nu-save-prompt)
   (define-key nu-keymap (kbd "M-d") 'nu-delete-prompt)
   (define-key nu-keymap (kbd "M-f") 'ace-jump-char-mode)
;g
   (define-key nu-keymap (kbd "M-j") 'backward-char)
   (define-key nu-keymap (kbd "M-k") 'next-line)
   (define-key nu-keymap (kbd "M-l") 'forward-char)
;m
   (define-key nu-keymap (kbd "M-z") 'undo)
;x let's keep altx for some time here before to decide.
;c
   (define-key nu-keymap (kbd "M-v") 'nu-insert-prompt)
;b
;n
   (define-key nu-keymap (kbd "²") 'ibuffer) ; to be advertised (global?)
   (define-key nu-keymap (kbd "M-²") 'other-window) ; to be advertised
   (define-key nu-keymap (kbd "M-0") 'nu-back-to-bol)
   (define-key nu-keymap (kbd "M-^") 'nu-back-to-indentation) ; fails here.
   (define-key nu-keymap (kbd "M-à") 'nu-back-to-indentation)
   (define-key nu-keymap (kbd "M-$") 'nu-end-of-line)

   (define-key nu-keymap (kbd "M-<SPC>") 'execute-extended-command)

; F keys

   (define-key nu-keymap (kbd "F10") 'tmm-menubar)



(define-minor-mode nu-mode
  "Simple Emacs keys"
  :global t
  :keymap nu-keymap

   ; this is part of a modern keymap because cursor
   ; is an indicator
   (set-default 'cursor-type 'bar)

   ; a real dependency
   (undo-tree-mode 1)

   ; do not use cua-mode because C-x C-c have specific meaning
   ; TODO: test if the user can enable cua-keys on his .emacs
   (cua-selection-mode 1))


(defun nu-emacs-get-map ()
  (setq x nu-keymap))

(provide 'emacs-nu)
