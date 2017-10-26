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
(require 'nu-integration)
(require 'nu-setup)
(require 'nu-prompters-lv)
(require 'nu-org)

(require 'iso-transl)

(defun nu-restore-default-keymap ()
  "Populate nu keymap with defaults."
  (interactive)

   ; <menu> is not a modifier. We need a map
   ;; so features like which-key still work fine
   (define-key nu-keymap (kbd "<menu>") nu-menu-map)

   ;;
   ;; setup the paddle. see setup.el.
   ;; this is i + j + k + l + h
   ;;
   ;; if another paddle is set
   ;; this should be same key set.
   ;;
   (nu-setup-classic-paddle)

   (define-key nu-keymap (kbd "C-a") 'nu-a-prompt)
   (define-key nu-menu-map (kbd "a") 'nu-a-map)
   (define-key nu-keymap (kbd "M-a") 'nu-set-mark)

   (define-key nu-keymap (kbd "M-z") 'undo-tree-undo)
   (define-key nu-keymap (kbd "C-z") 'undo-tree-visualize)

   (define-key nu-keymap (kbd "M-e") 'nu-copy-from-above)

   (define-key nu-keymap (kbd "M-r") 'replace-regexp)
   (define-key nu-menu-map (kbd "r") 'nu-replace-map)
   (define-key nu-keymap (kbd "C-r") 'nu-replace-prompt)

   (define-key nu-keymap (kbd "M-t") 'split-window-right)
   (define-key nu-keymap (kbd "C-t") 'nu-tab-prompt)

   ; y? yes? customize?
   (define-key nu-keymap (kbd "M-y") 'nu-copy-from-below)

   (define-key nu-keymap (kbd "C-u") 'backward-kill-word)
   (define-key nu-keymap (kbd "M-u") 'backward-word)
   (define-key universal-argument-map (kbd "C-u") 'backward-kill-word)

   (define-key nu-keymap (kbd "C-o") 'nu-open-prompt)
   (define-key nu-menu-map (kbd "o") 'nu-open-map)
   (define-key nu-keymap (kbd "M-o") 'forward-word)

   (define-key nu-keymap (kbd "M-p") 'universal-argument)
   (define-key nu-menu-map (kbd "p") 'nu-print-map)
   (define-key nu-keymap (kbd "C-p") 'nu-print-prompt)
   (define-key nu-keymap (kbd "M-P") 'async-shell-command)
   (define-key universal-argument-map (kbd "M-p") 'universal-argument-more)


   (define-key nu-keymap (kbd "M-q") 'keyboard-escape-quit)
   (define-key nu-menu-map (kbd "q") 'nu-quit-map)
   (define-key nu-keymap (kbd "C-q") 'nu-quit-prompt)

   (define-key nu-keymap (kbd "M-s") 'save-buffer)
   (define-key nu-keymap (kbd "C-s") 'nu-save-prompt)
   (define-key nu-menu-map (kbd "s") 'nu-save-map)

   (define-key nu-keymap (kbd "C-d") 'nu-M-x)
   (define-key nu-keymap (kbd "M-d") 'hydra-nu-meta-menu/body)

   (define-key nu-keymap (kbd "M-f") 'nu-search)
   (define-key nu-keymap (kbd "C-f") 'nu-find-prompt)
   (define-key nu-menu-map (kbd "f") 'nu-find-map)

   (define-key nu-keymap (kbd "M-g") 'ace-window)
   (define-key nu-keymap (kbd "C-g") 'nu-goto-prompt)
   (define-key nu-menu-map (kbd "g") 'nu-goto-map)

   ; C-h is help-map. Keep this.
   ; but override some
   (define-key help-map "f" 'nu-describe-function)
   (define-key help-map "v" 'nu-describe-variable)
   (define-key help-map (kbd "C-h") nil)

   ; SPC calls nu-keymap. With which-key it's a CheatSheet.
   (define-key help-map (kbd "<SPC>") nu-keymap)
   (define-key nu-menu-map (kbd "h") 'help-map)

   ; C-m stands for enter.
   (define-key nu-keymap (kbd "M-m") 'newline-and-indent)

   ;;
   ;; wxcvbn
   ;;

   (define-key nu-keymap (kbd "M-w") 'kill-buffer)
   (define-key nu-menu-map (kbd "w") 'nu-window-map)
   (define-key nu-keymap (kbd "C-w") 'nu-window-prompt)
 
   (define-key nu-keymap (kbd "C-x") 'nu-delete-prompt)
   (define-key nu-keymap (kbd "M-x") 'nu-cut-region-or-line)
   (define-key nu-menu-map (kbd "x") 'nu-delete-map)

   (define-key nu-keymap (kbd "C-c") 'nu-copy-prompt)
   (define-key nu-keymap (kbd "M-c") 'nu-copy-region-or-line)

   (define-key nu-keymap (kbd "C-v") 'nu-insert-prompt)
   (define-key nu-keymap (kbd "M-v") 'yank)
   (define-key nu-menu-map (kbd "v") 'nu-insert-map)
   (define-key nu-keymap (kbd "C-S-v") 'quoted-insert)

   (define-key nu-keymap (kbd "M-b") 'nu-bold)
   (define-key nu-menu-map (kbd "b") 'nu-bold-map)
   (define-key nu-keymap (kbd "C-b") 'nu-bold-prompt)

   (define-key nu-keymap (kbd "M-n") 'nu-new-empty-buffer)
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

Both direct keys & prompters will adapt to current mode.)"
  :global t
  :keymap nu-keymap
  :lighter " nu"

  (if nu-mode
      (progn
	; the variable allowing to make keymaps override...
	(setq nu-state t)

	; this is part of a modern keymap because cursor
	; is an indicator
	(set-default 'cursor-type 'bar)

	(delete-selection-mode 1)
	(global-undo-tree-mode)
        (which-key-mode)

        ;; completion system
	;; see nu-setup
        (nu-setup-basic)

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

	(nu-restore-default-keymap)

        (if nu-mode-show-welcome-screen
	   (add-hook 'emacs-startup-hook '(lambda ()
             (nu-help-prompt)))))

    ; if disabled
    (setq nu-state nil)))

(provide 'nu-mode)

;;; nu-mode.el ends here
