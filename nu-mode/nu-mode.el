;;; nu-mode.el --- Modern Emacs Keybinding
;;; Emacs-Nu is an emacs mode which wants to makes Emacs easier.
;;; Copyright (C) 2017 Pierre-Yves LUYTEN
;;;  
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;  
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;  
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

(require 'nu-common)
(require 'nu-hooks)
(require 'nu-setup)
(require 'nu-lv)

(require 'help-fns+)
(require 'iso-transl)

(defvar nu-menu-map)

(defun nu-restore-default-keymap ()
  "Populate nu keymap with defaults."
  (interactive)

   ;  do not respect _any_ emacs convention. Seriously.

   ; <menu> is not a modifier. We need a map.
   (define-key nu-keymap (kbd "<menu>") nu-menu-map)


   ;;
   ; azertyuiop. Yes it's the main target, don't care.
   ; anyway it will work as expected onto qwerty as well.
   ;;


   (define-key nu-keymap (kbd "C-a") 'nu-a-prompt)
   (define-key nu-keymap (kbd "C-S-a") 'nu-set-rectangle-mark)
   (define-key nu-keymap (kbd "C-M-a") 'cua-toggle-global-mark)
   (define-key nu-menu-map (kbd "a") 'nu-a-map)
   (define-key nu-keymap (kbd "M-a") 'nu-set-mark)

   (define-key nu-keymap (kbd "M-z") 'undo-tree-undo)
   (define-key nu-keymap (kbd "C-S-z") 'undo-tree-redo)
   (define-key nu-keymap (kbd "C-z") 'undo-tree-visualize)


   (define-key nu-keymap (kbd "M-e") 'nu-copy-from-above)


   (define-key nu-keymap (kbd "M-r") 'replace-regexp)
   (define-key nu-menu-map (kbd "r") 'nu-replace-map)
   (define-key nu-keymap (kbd "C-r") 'nu-replace-prompt)

   (define-key nu-keymap (kbd "M-t") 'split-window-right)
   (define-key nu-keymap (kbd "C-t") 'ido-switch-buffer-other-window)

   ; y? yes? customize?
   (define-key nu-keymap (kbd "M-y") 'nu-copy-from-below)

   (define-key nu-keymap (kbd "C-u") 'backward-kill-word)
   (define-key nu-keymap (kbd "C-S-u") 'nu-backward-kill-block)
   (define-key nu-keymap (kbd "C-M-u") 'backward-list)
   (define-key nu-keymap (kbd "M-u") 'backward-word)
   (define-key universal-argument-map (kbd "C-u") 'backward-kill-word)

   ; C-i is tab. Alt-S-i is selection.
   (define-key nu-keymap (kbd "C-S-i") 'nu-previous-buffer)
   (define-key nu-keymap (kbd "C-M-i") 'beginning-of-defun)
   (define-key nu-keymap (kbd "M-i") 'previous-line)

   (define-key nu-keymap (kbd "C-o") 'nu-open-prompt)
   (define-key nu-menu-map (kbd "o") 'nu-open-map)
   (define-key nu-keymap (kbd "C-S-o") 'helm-mini)
   (define-key nu-keymap (kbd "C-M-o") 'forward-list)
   (define-key nu-keymap (kbd "M-o") 'forward-word)

   (define-key nu-keymap (kbd "M-p") 'universal-argument)
   (define-key nu-keymap (kbd "C-M-p") 'eval-last-sexp)
   (define-key nu-menu-map (kbd "p") 'nu-print-map)
   (define-key nu-keymap (kbd "C-p") 'nu-print-prompt)
   (define-key nu-keymap (kbd "M-P") 'async-shell-command)
   (define-key universal-argument-map (kbd "M-p") 'universal-argument-more)


   (define-key nu-keymap (kbd "C-q") 'keyboard-escape-quit)
   (define-key nu-keymap (kbd "C-S-q") 'save-buffers-kill-emacs)
   (define-key nu-menu-map (kbd "q") 'nu-quit-map)
   (define-key nu-keymap (kbd "M-q") 'nu-quit-prompt)


   (define-key nu-keymap (kbd "M-s") 'save-buffer)
   (define-key nu-keymap (kbd "C-S-s") 'org-store-link)
   (define-key nu-keymap (kbd "C-s") 'nu-save-prompt)
   (define-key nu-menu-map (kbd "s") 'nu-save-map)

   (define-key nu-keymap (kbd "C-d") 'kill-word)
   (define-key nu-keymap (kbd "C-S-d") 'nu-kill-block)
   (define-key nu-keymap (kbd "M-d") 'hydra-nu-meta-menu/body)
   (define-key nu-menu-map (kbd "d") 'nu-delete-map)

   (define-key nu-keymap (kbd "M-f") 'nu-isearch-forward-regexp)
   (define-key nu-keymap (kbd "C-S-f") 'avy-goto-char) 
   (define-key nu-keymap (kbd "C-f") 'nu-find-prompt)
   (define-key nu-menu-map (kbd "f") 'nu-find-map)

   (define-key nu-keymap (kbd "M-g") 'ace-window)
   (define-key nu-keymap (kbd "C-g") 'nu-goto-prompt)
   (define-key nu-keymap (kbd "C-M-g") 'recenter-top-bottom)
   (define-key nu-menu-map (kbd "g") 'nu-goto-map)

   (define-key nu-keymap (kbd "C-h") 'nu-help-prompt)
   (define-key nu-menu-map (kbd "h") 'help-map)
   (define-key nu-keymap (kbd "C-M-h") 'nu-prompt-for-major-mode)
   (define-key nu-keymap (kbd "M-h") 'nu-back-to-indentation)

   (define-key nu-keymap (kbd "C-j") 'backward-delete-char)
   (define-key nu-keymap (kbd "C-M-j") 'backward-sexp)
   (define-key nu-keymap (kbd "M-j") 'backward-char)

   (define-key nu-keymap (kbd "C-k") 'kill-visual-line)
   (define-key nu-keymap (kbd "C-S-k") 'nu-next-buffer)
   (define-key nu-keymap (kbd "C-M-k") 'end-of-defun)
   (define-key nu-keymap (kbd "M-k") 'next-line)

   (define-key nu-keymap (kbd "C-l") 'delete-forward-char)
   (define-key nu-keymap (kbd "C-M-l") 'forward-sexp)
   (define-key nu-keymap (kbd "M-l") 'forward-char)

   ; C-m stands for enter.
   (define-key nu-keymap (kbd "M-m") 'newline-and-indent)

   ;;
   ;; wxcvbn
   ;;

   (define-key nu-keymap (kbd "M-w") 'kill-buffer)
   (define-key nu-menu-map (kbd "w") 'nu-window-map)
   (define-key nu-menu-map (kbd "C-S-w") 'transpose-frame)
   (define-key nu-keymap (kbd "C-w") 'nu-window-prompt)

   (define-key nu-keymap (kbd "C-x") 'nu-cut-region-or-line)
   (define-key nu-keymap (kbd "C-S-x") 'nu-global-prompt)
   (define-key nu-keymap (kbd "M-x") 'nu-M-x)

   (define-key nu-keymap (kbd "C-c") 'nu-copy-region-or-line)
   ; no standard M-c ; some mode might need it however.

   (define-key nu-keymap (kbd "C-v") 'nu-yank-pop-or-yank)
   (define-key nu-keymap (kbd "M-v") 'nu-insert-prompt)
   (define-key nu-menu-map (kbd "v") 'nu-insert-map)
   (define-key nu-keymap (kbd "M-V") 'helm-show-kill-ring)
   (define-key nu-keymap (kbd "C-S-v") 'quoted-insert)


   (define-key nu-keymap (kbd "M-b") 'nu-bold)
   (define-key nu-keymap (kbd "C-S-b") 'comment-or-uncomment-region)
   (define-key nu-menu-map (kbd "b") 'nu-bold-map)
   (define-key nu-keymap (kbd "C-b") 'nu-bold-prompt)

   (define-key nu-keymap (kbd "M-n") 'nu-new-empty-buffer)
   (define-key nu-keymap (kbd "C-S-n") 'org-capture)
   (define-key nu-keymap (kbd "C-n") 'nu-new-prompt)
   (define-key nu-keymap (kbd "M-N") 'split-window-horizontally)


   ;;
   ;; ^ $ up down prior next spc backspace
   ;;

   (define-key nu-keymap (kbd "C-$") 'kill-line)
   (define-key nu-keymap (kbd "M-$") 'nu-end-of-line)

   (define-key nu-keymap (kbd "M-<dead-circumflex>") 'nu-back-to-indentation)
   (define-key nu-keymap (kbd "C-<dead-circumflex>") 'nu-backward-kill-line)

   (define-key nu-keymap (kbd "C-<SPC>") 'nu-trigger-mode-specific-map) ; C-c

   (define-key nu-keymap (kbd "C-M-<SPC>") 'scroll-other-window)
   (define-key nu-keymap (kbd "M-<SPC>") 'scroll-up)

   (define-key nu-keymap (kbd "M-<backspace>") 'scroll-down)

   (define-key nu-keymap (kbd "C-<next>") 'next-buffer)
   (define-key nu-keymap (kbd "C-<prior>") 'previous-buffer)
   (define-key nu-keymap (kbd "C-<return>") 'repeat)

   ;;
   ;; FN keys?
   ;;

   (define-key nu-keymap (kbd "<f10>") 'tmm-menubar))


(define-minor-mode nu-mode
  "Modern Emacs keys.

CUA+ : use most important standard shortcuts : C-s, C-f, C-c, C-x, C-v.
Convenient : To move, use M-i M-j M-k M-l.

Still powerful : Trying M-f will prompt for which find related function to run.
C-a : selection, C-r : replacement, M-s (saving), M-d (deletions),
M-g (goto), M-v (insertions).

Both direct keys & prompters will adapt to current mode
eg dired, magit, helm, ...)"
  :global t
  :keymap nu-keymap

  (if nu-mode
      (progn
	; the variable allowing to make keymaps override...
	(setq nu-state t)
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

					; populate all prompts
					; so all maps are defined
	(nu-populate-print)
	(nu-populate-quit)
	(nu-populate-delete)
	(nu-populate-bold-map)
	(nu-populate-insert-map)
	(nu-populate-save-map)
	(nu-populate-new-map)
	(nu-populate-a-map)
	(nu-populate-open-map)
	(nu-populate-goto-map)
	(nu-populate-find-map)
	(nu-populate-replace)

	(nu-restore-default-keymap))
					; if disabled
    (setq nu-state nil)))


(provide 'nu-mode)

;;; nu-mode.el ends here
