(require 'nu-common)
(require 'nu-hooks)


; we have the real keymap for real ("nu-keymap").
; menu-map is only there for where-is sake.
; thus we define things twice :
; once a grand tragedy. Once as a farce.
(defvar nu-keymap (make-sparse-keymap) "Emacs nu keymap")
(defvar nu-menu-map (make-sparse-keymap) "Nu Menu Keymap")


(require 'iso-transl); ^ : still does not work

(defun nu-restore-default-keymap ()
  "Populate nu keymap with defaults."
  (interactive)

   ; <menu> is not a modifier. We need a map.
   (define-key nu-keymap (kbd "<menu>") nu-menu-map)

   ;  all _control_ features (mostly, prompts..)
   ;  do not respect _any_ emacs convention. Seriously.

   (define-key nu-keymap (kbd "C-q") 'keyboard-escape-quit)
   (define-key nu-menu-map (kbd "w") 'nu-window-map)
   (define-key nu-keymap (kbd "C-w") 'nu-window-prompt)
   ; e? edit?
   (define-key nu-keymap (kbd "C-r") 'nu-replace-prompt)
   (define-key nu-menu-map (kbd "r") 'nu-replace-map)
   (define-key nu-keymap (kbd "C-t") 'nu-new-tab)
   ; y?
   (define-key nu-keymap (kbd "C-u") 'backward-kill-word)
   (define-key universal-argument-map (kbd "C-u") 'backward-kill-word) ; fix c-u with prefix.
   (define-key universal-argument-map (kbd "M-p") 'universal-argument-more)
   ; C-i is tab.
   (define-key nu-keymap (kbd "C-o") 'nu-open-prompt)
   (define-key nu-menu-map (kbd "o") 'nu-open-map)
   (define-key nu-keymap (kbd "C-p") 'nu-print-prompt)
   (define-key nu-menu-map (kbd "p") 'nu-print-map)

   (define-key nu-keymap (kbd "C-a") 'nu-a-prompt)
   (define-key nu-menu-map (kbd "a") 'nu-a-map)
   (define-key nu-keymap (kbd "C-s") 'save-buffer)
   ; C-d is not yet defined. Currently it is synonym to C-l.
   (define-key nu-keymap (kbd "C-f") 'nu-find-prompt)
   (define-key nu-menu-map (kbd "f") 'nu-find-map)
   (define-key nu-keymap (kbd "C-g") 'nu-global-prompt)
;   (define-key nu-menu-map (kbd "g") 'nu-global-map)
   (define-key nu-keymap (kbd "C-h") 'nu-help-prompt)
   (define-key nu-menu-map (kbd "h") 'help-map)
   (define-key nu-keymap (kbd "C-j") 'backward-delete-char)
   (define-key nu-keymap (kbd "C-k") 'kill-visual-line)
   (define-key nu-keymap (kbd "C-l") 'delete-forward-char)
   ; C-m stands for enter. This is something to study.

   (define-key nu-keymap (kbd "C-z") 'undo-tree-visualize)
   (define-key nu-keymap (kbd "C-x") 'nu-cut-region-or-line)
   (define-key nu-keymap (kbd "C-c") 'nu-copy-region-or-line)
   (define-key nu-keymap (kbd "C-v") 'nu-yank-pop-or-yank)
   ;b
   (define-key nu-keymap (kbd "C-n") 'nu-new-prompt)


   (define-key nu-keymap (kbd "C-$") 'kill-line)

   (define-key nu-keymap (kbd "C-<SPC>") 'nu-trigger-mode-specific-map) ; C-c

   (define-key nu-keymap (kbd "C-<next>") 'next-buffer)
   (define-key nu-keymap (kbd "C-<prior>") 'previous-buffer)
;   (define-key nu-keymap (kbd "M-<next>") ')
;   (define-key nu-keymap (kbd "M-<prior>") ')

   (define-key nu-keymap (kbd "C-<return>") 'repeat) ; do not work.



;  all _ alt _ features

   (define-key nu-keymap (kbd "M-q") 'quoted-insert) ; fix minibuf'
   (define-key nu-keymap (kbd "M-w") 'nu-close-tab)
   (define-key nu-keymap (kbd "M-e") 'nu-copy-from-above)
   ;r  maybe overwrite mode...
   ;t  transpose?
   (define-key nu-keymap (kbd "M-y") 'nu-copy-from-below)
   (define-key nu-keymap (kbd "M-u") 'backward-word)
   (define-key nu-keymap (kbd "M-i") 'previous-line)
   (define-key nu-keymap (kbd "M-o") 'forward-word)
   (define-key nu-keymap (kbd "M-p") 'universal-argument)
   (define-key nu-keymap (kbd "M-a") 'cua-set-mark)
   (define-key nu-keymap (kbd "M-s") 'nu-save-prompt)
   (define-key nu-menu-map (kbd "s") 'nu-save-map)
   (define-key nu-keymap (kbd "M-d") 'nu-delete-prompt)
   (define-key nu-menu-map (kbd "d") 'nu-delete-map)
   (define-key nu-keymap (kbd "M-f") 'ace-jump-char-mode)
   ;g : ace-jump?
   (define-key nu-keymap (kbd "M-h") 'nu-back-to-indentation)
   (define-key nu-keymap (kbd "M-j") 'backward-char)
   (define-key nu-keymap (kbd "M-k") 'next-line)
   (define-key nu-keymap (kbd "M-l") 'forward-char)
   (define-key nu-keymap (kbd "M-m") 'newline-and-indent)
   (define-key nu-keymap (kbd "M-z") 'undo)
   ;x  execute-extended-command
   ;c
   (define-key nu-keymap (kbd "M-v") 'nu-insert-prompt)
   (define-key nu-menu-map (kbd "v") 'nu-insert-map)
   ;b
   (define-key nu-keymap (kbd "M-n") 'nu-new-empty-buffer)
   (define-key nu-keymap (kbd "²") 'ibuffer)
   (define-key nu-keymap (kbd "M-²") 'other-window)
   ;(define-key nu-keymap (kbd "M-0") 'nu-back-to-bol) hmm keep num!
   (define-key nu-keymap (kbd "M-<dead-circumflex>")'nu-back-to-indentation)
   (define-key nu-keymap (kbd "M-à") 'nu-back-to-indentation)
   (define-key nu-keymap (kbd "M-$") 'nu-end-of-line)

   (define-key nu-keymap (kbd "M-<SPC>") 'scroll-up)
   (define-key nu-keymap (kbd "M-<backspace>") 'scroll-down)
 
;  hyper keys - todo: have an arg on these...
;  maybe hyper / super.
;   (define-key nu-keymap (kbd "s-k") 'scroll-down)
;   (define-key nu-keymap (kbd "s-i") 'scroll-up)

; Function  keys

   ; default f10 fires a gui menu which sucks.
   (define-key nu-keymap (kbd "<f10>") 'tmm-menubar)
)


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
   ;
   ; do not keep cua modifier on 'meta
   ; otherwise their rectangle is broken...

   (setq cua-rectangle-mark-key (kbd "C-S-s-<return>"))
   (cua-selection-mode 1)
   (setq cua--rectangle-modifier-key 'control)

   (nu-restore-default-keymap))


(provide 'emacs-nu)
