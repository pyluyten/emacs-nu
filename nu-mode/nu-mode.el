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

(require 'evil)
(require 'iso-transl)

(defmacro nu-mode-dkey (keymap letter immediate def)
  "define-key modifier+letter to invoke def on keymap

modifier is nu-immediate-key if immediate is t,
otherwise   nu-menu-key.

if menu is set and t, also define-key nu-menu-map for letter -> def
TODO : in case of menu, the associated -map needs to be lookup."
    `(progn
       (define-key ,keymap (kbd (concat
			       (if ,immediate nu-immediate-key
				              nu-menu-key) "-" ,letter)) ',def)))

(defun nu-populate-menu-map ()
  "Fill nu menu map. Then a key has to be bound to nu menu map.

This may allow where-is to know where to find functions."

   (define-key nu-menu-map "a" 'nu-mark-map)
   (define-key nu-menu-map "b" 'nu-change-map)
   (define-key nu-menu-map "b" 'nu-change-map)
   (define-key nu-menu-map "c" 'nu-copy-prompt)
   (define-key nu-menu-map "d" 'nu-display-map)
   (define-key nu-menu-map "f" 'nu-find-prompt)
   (define-key nu-menu-map "g" 'nu-goto-map)
   (define-key nu-menu-map "h" 'help-map)
   (define-key nu-menu-map "n" 'nu-new-map)
   (define-key nu-menu-map "o" 'nu-open-map)
   (define-key nu-menu-map "p" 'nu-print-map)
   (define-key nu-menu-map "q" 'nu-quit-map)
   (define-key nu-menu-map "r" 'nu-replace-map)
   (define-key nu-menu-map "s" 'nu-save-map)
   (define-key nu-menu-map "v" 'nu-insert-map)
   (define-key nu-menu-map "x" 'nu-kill-map)
   (define-key nu-menu-map "z" 'undo-tree-visualize))

(defun nu-populate-prompters ()
  "Fill prompters map."

   (define-key nu-menu-map "a" 'nu-mark-prompt)
   (define-key nu-menu-map "b" 'nu-change-prompt)
   (define-key nu-menu-map "f" 'nu-find-prompt)
   (define-key nu-menu-map "g" 'nu-goto-prompt)
   (define-key nu-menu-map "h" 'help-map)
   (define-key nu-menu-map "n" 'nu-new-prompt)
   (define-key nu-menu-map "o" 'nu-open-prompt)
   (define-key nu-menu-map "p" 'nu-print-prompt)
   (define-key nu-menu-map "q" 'nu-quit-prompt)
   (define-key nu-menu-map "r" 'nu-replace-prompt)
   (define-key nu-menu-map "s" 'nu-save-prompt)
   (define-key nu-menu-map "v" 'nu-insert-prompt)
   (define-key nu-menu-map "w" 'nu-display-prompt)
   (define-key nu-menu-map "x" 'nu-kill-prompt))

(defun nu-prompt-for-menus ()
  "Prompt for nu menu map."
  (interactive)
  (nu-prompt-for-keymap nu-menu-map))

(defun nu-restore-default-keymap ()
  "Populate nu keymap with defaults."
  (interactive)

   ; <menu> is not a modifier. We need a map
   ;; so features like which-key still work fine
   (define-key evil-insert-state-map (kbd "<menu>") nu-menu-map)
   (nu-populate-menu-map)

   (define-key evil-insert-state-map (kbd "²") evil-insert-state-map)
   (define-key evil-insert-state-map (kbd "<escape>") 'keyboard-escape-quit)

   ;;
   ;; setup the paddle. see setup.el.
   ;; note the paddle is always Alt+keys.
   ;;
   (if nu-use-vi-paddle
       (nu-setup-vi-paddle evil-insert-state-map)
       (nu-setup-classic-paddle evil-insert-state-map))

   (nu-mode-dkey evil-insert-state-map "a" t evil-visual-line)
   (nu-mode-dkey evil-insert-state-map "a" nil nu-mark-prompt)

   (nu-mode-dkey evil-insert-state-map "z" t undo-tree-undo)
   (nu-mode-dkey evil-insert-state-map "z" nil undo-tree-visualize)

   (define-key evil-insert-state-map (kbd "M-e") 'evil-copy-from-below)

   (nu-mode-dkey evil-insert-state-map "r" t replace-regexp)
   (nu-mode-dkey evil-insert-state-map "r" nil nu-replace-prompt)

   (nu-mode-dkey  evil-insert-state-map "t" t split-window-right)

   (define-key evil-insert-state-map (kbd "M-y") 'evil-copy-from-above)
   (define-key evil-insert-state-map (kbd "C-y") 'evil-execute-in-normal-state)

   ;; u remains as is
   (define-key evil-insert-state-map (kbd "C-u") 'backward-kill-word)
   (define-key evil-insert-state-map (kbd "M-u") 'backward-word)
   (define-key universal-argument-map (kbd "C-u") 'backward-kill-word)

   ;; o remains as is no matter cua or ergonomic.
   (define-key evil-insert-state-map (kbd "C-o") 'nu-open-prompt)
   (define-key evil-visual-state-map (kbd "C-o") 'nu-open-prompt)
   (define-key evil-insert-state-map (kbd "M-o") 'forward-word)

   (nu-mode-dkey evil-insert-state-map "p" t universal-argument)
   (nu-mode-dkey evil-insert-state-map "p" nil nu-print-prompt)
   (define-key universal-argument-map (kbd "M-p") 'universal-argument-more)

   (nu-mode-dkey evil-insert-state-map "q" t keyboard-escape-quit)
   (nu-mode-dkey evil-insert-state-map "q" nil nu-quit-prompt)

   (nu-mode-dkey evil-insert-state-map "s" t save-buffer)
   (nu-mode-dkey evil-insert-state-map "s" nil nu-save-prompt)

   (define-key evil-insert-state-map (kbd "C-d") 'nu-M-x)
   (define-key evil-visual-state-map (kbd "C-d") 'nu-M-x)
   (define-key evil-insert-state-map (kbd "M-d") 'nu-do-prompt)

   (nu-mode-dkey evil-insert-state-map "f" t nu-search)
   (nu-mode-dkey evil-insert-state-map "f" nil nu-find-prompt)

   (nu-mode-dkey evil-insert-state-map "g" t ace-window)
   (nu-mode-dkey evil-visual-state-map "g" t ace-window)
   (nu-mode-dkey evil-emacs-state-map "g" t ace-window)
   (nu-mode-dkey evil-insert-state-map "g" nil nu-goto-prompt)

   ; C-h is help-map. Keep this.
   ; but override some
   (define-key help-map "f" 'nu-describe-function)
   (define-key help-map "v" 'nu-describe-variable)
   (define-key help-map (kbd "C-h") nil)

   ; SPC calls evil-insert-state-map. With which-key it's a CheatSheet.
   (define-key help-map (kbd "<SPC>") 'nu-cheat-sheet)

   ; C-m stands for enter.
   (define-key evil-insert-state-map (kbd "M-m") 'newline-and-indent)

   ;;
   ;; wxcvbn
   ;;

   (nu-mode-dkey evil-insert-state-map "w" t nu-quit-document)
   (nu-mode-dkey evil-insert-state-map "w" nil nu-display-prompt)
 
   (nu-mode-dkey evil-insert-state-map "x" nil nu-kill-prompt)
   (nu-mode-dkey evil-insert-state-map "x" t nu-cut-region-or-line)
   (nu-mode-dkey evil-normal-state-map "x" t evil-delete)

   (nu-mode-dkey evil-insert-state-map "c" nil nu-copy-prompt)
   (nu-mode-dkey evil-insert-state-map "c" t nu-copy-region-or-line)
   (nu-mode-dkey evil-normal-state-map "c" t nu-copy-region-or-line)

   (nu-mode-dkey evil-insert-state-map "v" nil nu-insert-prompt)
   (nu-mode-dkey evil-insert-state-map "v" t yank)

   (nu-mode-dkey evil-insert-state-map "b" t nu-bold)
   (nu-mode-dkey evil-insert-state-map "b" nil nu-change-prompt)

   (nu-mode-dkey evil-insert-state-map "n" t nu-new-empty-buffer)
   (nu-mode-dkey evil-insert-state-map "n" nil nu-new-prompt)

   ;;
   ;; ^ $ up down prior next spc backspace
   ;;

   (define-key evil-insert-state-map (kbd "C-$") 'kill-line)
   (define-key evil-insert-state-map (kbd "M-$") 'nu-end-of-line)

   (define-key evil-insert-state-map (kbd "M-<dead-circumflex>") 'nu-back-to-indentation)
   (define-key evil-insert-state-map (kbd "C-<dead-circumflex>") 'nu-backward-kill-line)

   (define-key evil-insert-state-map (kbd "C-<SPC>") 'nu-trigger-mode-specific-map) ; C-c

   (define-key evil-insert-state-map (kbd "M-<SPC>") 'scroll-up)
   (define-key evil-insert-state-map (kbd "M-<backspace>") 'scroll-down))

(defun nu-populate-all-menus ()
"populate all menus"
	(nu-populate-print)
	(nu-populate-quit)
	(nu-populate-kill)
	(nu-populate-change-map)
	(nu-populate-insert-map)
	(nu-populate-save-map)
	(nu-populate-new-map)
	(nu-populate-mark-map)
	(nu-populate-open-map)
	(nu-populate-goto-map)
	(nu-populate-find-map)
	(nu-populate-replace)
        (nu-populate-display))

(defun nu-initialize ()
"Initialize nu,

either for nu-mode or other, like evil mode using nu prompts.
yeah like spacemacs."
	; the variable allowing to make keymaps override...
	(setq nu-state t)

        ;; completion system
	;; see nu-setup
        (nu-setup-basic)

	(global-undo-tree-mode)
        (which-key-mode)

        ; populate all prompts
	; so all maps are defined
        (nu-populate-all-menus))


  (defun nu-visual-block ()
    (interactive)
    (run-with-timer 0.01 nil 'evil-visual-block))


(defun nu-mode ()
"Activate nu bindings. These are non modal, CUA friendly bindings,
extensively using which-key or similar menus.

Technically this function activates evil-mode, but not vi bindings."
	(evil-mode)
        (nu-initialize)
        (setq evil-default-state 'insert)
	(setq evil-emacs-state-modes nil)
	(setq evil-motion-state-modes nil)
	(nu-restore-default-keymap)

        (if nu-show-welcome-screen
	   (add-hook 'emacs-startup-hook '(lambda ()
             (nu-help-prompt))))

  ;;
  ;; now adapt menus
  ;;

  (add-hook 'nu-populate-hook '(lambda ()
    (progn

       ;; inserted inside open menu
       (define-key nu-open-map "o" 'evil-jump-backward)

       ;; reset!
       (nu-define-prefix 'nu-mark-map)
       (define-key nu-mark-map (kbd "r") 'nu-visual-block)
       (define-key nu-mark-map (kbd "l") 'evil-visual-line)
       (define-key nu-mark-map (kbd "k") 'evil-visual-char)))))


(provide 'nu-mode)

;;; nu-mode.el ends here
