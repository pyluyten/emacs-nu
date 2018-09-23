(require 'evil)
(require 'nu-mode)

;;
;; normal map
;;

(define-key evil-normal-state-map "i" 'previous-line)
(define-key evil-normal-state-map "j" 'backward-char)
(define-key evil-normal-state-map "k" 'next-line)
(define-key evil-normal-state-map "l" 'forward-char)
(define-key evil-normal-state-map "o" 'forward-word)
(define-key evil-normal-state-map "h" 'nu-back-to-indentation)
(define-key evil-normal-state-map "H" help-map)
(define-key evil-normal-state-map "a" 'nu-a-prompt)
(define-key evil-normal-state-map "z" 'undo-tree-visualize)
(define-key evil-normal-state-map "e" 'nu-copy-from-above)
(define-key evil-normal-state-map "r" 'nu-replace-prompt)
(define-key evil-normal-state-map "t" 'split-window-right)
(define-key evil-normal-state-map "y" 'nu-copy-from-below)
(define-key evil-normal-state-map "u" 'backward-word)
(define-key evil-normal-state-map "p" 'nu-print-prompt)
(define-key evil-normal-state-map (kbd "M-q") 'keyboard-escape-quit)
(define-key evil-normal-state-map "q" 'nu-quit-prompt)
(define-key evil-normal-state-map "s" 'nu-save-prompt)
(define-key evil-normal-state-map "f" 'nu-find-prompt)
(define-key evil-normal-state-map "g" 'nu-goto-prompt)
(define-key evil-normal-state-map "m" 'newline-and-indent)
(define-key evil-normal-state-map "w" 'nu-display-prompt)
(define-key evil-normal-state-map "x" 'nu-delete-prompt)
(define-key evil-normal-state-map "c" 'nu-copy-prompt)
(define-key evil-normal-state-map "v" 'nu-insert-prompt)
(define-key evil-normal-state-map "b" 'nu-change-prompt)
(define-key evil-normal-state-map "n" 'nu-new-prompt)
(define-key evil-normal-state-map "$" 'end-of-line)

(defvar nu-evil-do-map)
(setq nu-evil-do-map (make-sparse-keymap))
(define-key evil-normal-state-map "d" nu-evil-do-map)
(define-key nu-evil-do-map "d" 'nu-M-x)
(define-key nu-evil-do-map "l" 'evil-append)
(define-key nu-evil-do-map "m" 'evil-append-line)
(define-key nu-evil-do-map "j" 'evil-insert)
(define-key nu-evil-do-map "h" 'evil-insert-line)
(define-key nu-evil-do-map "k" 'evil-open-below)
(define-key nu-evil-do-map "i" 'evil-open-above)
(define-key nu-evil-do-map "o" 'nu-open-prompt)
(define-key evil-normal-state-map (kbd "<SPC>") 'nu-trigger-mode-specific-map)

;;
;; insert map
;; 

(define-key evil-insert-state-map [escape] 'evil-normal-state)
(define-key evil-insert-state-map (kbd "²") 'evil-normal-state)

(provide 'nu-mode-evil)
