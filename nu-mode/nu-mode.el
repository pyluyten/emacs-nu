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

(defmacro nu-mode-dkey (keymap letter immediate def menu)
  "define-key modifier+letter to invoke def on keymap

modifier is nu-immediate-key if immediate is t,
otherwise   nu-menu-key.

if menu is set and t, also define-key nu-menu-map for letter -> def
TODO : in case of menu, the associated -map needs to be lookup."
    `(progn
       (define-key ,keymap (kbd (concat
			       (if ,immediate nu-immediate-key
				              nu-menu-key) "-" ,letter)) ',def)
       (unless ,immediate
         (if ,menu
	     (define-key nu-menu-map ,letter ',def)))))

(defun nu-fill-mode-map-with-nu-menus ()
  "Make Control+c <key> call the associated nu menu.

This func is not called by nu-mode nor nu-state.
This is designed to be used in vanilla Emacs, or ErgoEmacs."
  (interactive)

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

  (global-set-key (kbd "C-c a") 'nu-a-prompt)
  (global-set-key (kbd "C-c b") 'nu-bold-prompt)
  (global-set-key (kbd "C-c c") 'nu-copy-prompt)
  (global-set-key (kbd "C-c d") 'nu-do-prompt)
  (global-set-key (kbd "C-c f") 'nu-find-prompt)
  (global-set-key (kbd "C-c g") 'nu-goto-prompt)
  (global-set-key (kbd "C-c n") 'nu-new-prompt)
  (global-set-key (kbd "C-c o") 'nu-open-prompt)
  (global-set-key (kbd "C-c p") 'nu-print-prompt)
  (global-set-key (kbd "C-c q") 'nu-quit-prompt)
  (global-set-key (kbd "C-c r") 'nu-replace-prompt)
  (global-set-key (kbd "C-c s") 'nu-replace-prompt)
  (global-set-key (kbd "C-c v") 'nu-save-prompt)
  (global-set-key (kbd "C-c w") 'nu-window-prompt)
  (global-set-key (kbd "C-c x") 'nu-delete-prompt)
  (global-set-key (kbd "C-c z") 'undo-tree-visualize))

(defun nu-restore-default-keymap ()
  "Populate nu keymap with defaults."
  (interactive)

   ; <menu> is not a modifier. We need a map
   ;; so features like which-key still work fine
   (define-key evil-insert-state-map (kbd "<menu>") nu-menu-map)
   (define-key evil-insert-state-map (kbd "²") evil-insert-state-map)
   (define-key evil-insert-state-map (kbd "<escape>") 'keyboard-escape-quit)

   ;;
   ;; setup the paddle. see setup.el.
   ;; note the paddle is always Alt+keys.
   ;;
   (if nu-use-vi-paddle
       (nu-setup-vi-paddle)
       (nu-setup-classic-paddle))

   (define-key nu-menu-map (kbd "a") 'nu-a-map)
   (nu-mode-dkey evil-insert-state-map "a" t evil-visual-line nil)
   (nu-mode-dkey evil-insert-state-map "a" nil nu-a-prompt nil)

   (nu-mode-dkey evil-insert-state-map "z" t undo-tree-undo nil)
   (nu-mode-dkey evil-insert-state-map "z" nil undo-tree-visualize nil)

   (define-key evil-insert-state-map (kbd "M-e") 'evil-copy-from-below)

   (define-key nu-menu-map (kbd "r") 'nu-replace-map)
   (nu-mode-dkey evil-insert-state-map "r" t replace-regexp nil)
   (nu-mode-dkey evil-insert-state-map "r" nil nu-replace-prompt nil)

   (nu-mode-dkey  evil-insert-state-map "t" t split-window-right nil)

   (define-key evil-insert-state-map (kbd "M-y") 'evil-copy-from-above)
   (define-key evil-insert-state-map (kbd "C-y") 'evil-execute-in-normal-state)

   ;; u remains as is
   (define-key evil-insert-state-map (kbd "C-u") 'backward-kill-word)
   (define-key evil-insert-state-map (kbd "M-u") 'backward-word)
   (define-key universal-argument-map (kbd "C-u") 'backward-kill-word)

   ;; o remains as is no matter cua or ergonomic.
   (define-key evil-insert-state-map (kbd "C-o") 'nu-open-prompt)
   (define-key evil-visual-state-map (kbd "C-o") 'nu-open-prompt)
   (define-key nu-menu-map (kbd "o") 'nu-open-map)
   (define-key evil-insert-state-map (kbd "M-o") 'forward-word)

   (nu-mode-dkey evil-insert-state-map "p" t universal-argument nil)
   (define-key nu-menu-map (kbd "p") 'nu-print-map)
   (nu-mode-dkey evil-insert-state-map "p" nil nu-print-prompt nil)
   (define-key universal-argument-map (kbd "M-p") 'universal-argument-more)

   (nu-mode-dkey evil-insert-state-map "q" t keyboard-escape-quit nil)
   (define-key nu-menu-map (kbd "q") 'nu-quit-map)
   (nu-mode-dkey evil-insert-state-map "q" nil nu-quit-prompt nil)

   (nu-mode-dkey evil-insert-state-map "s" t save-buffer nil)
   (nu-mode-dkey evil-insert-state-map "s" nil nu-save-prompt nil)
   (define-key nu-menu-map (kbd "s") 'nu-save-map)

   (define-key evil-insert-state-map (kbd "C-d") 'nu-M-x)
   (define-key evil-visual-state-map (kbd "C-d") 'nu-M-x)
   (define-key evil-insert-state-map (kbd "M-d") 'nu-do-prompt)

   (nu-mode-dkey evil-insert-state-map "f" t nu-search nil)
   (nu-mode-dkey evil-insert-state-map "f" nil nu-find-prompt nil)
   (define-key nu-menu-map (kbd "f") 'nu-find-map)

   (nu-mode-dkey evil-insert-state-map "g" t ace-window nil)
   (nu-mode-dkey evil-visual-state-map "g" t ace-window nil)
   (nu-mode-dkey evil-emacs-state-map "g" t ace-window nil)
   (nu-mode-dkey evil-insert-state-map "g" nil nu-goto-prompt nil)
   (define-key nu-menu-map (kbd "g") 'nu-goto-map)

   ; C-h is help-map. Keep this.
   ; but override some
   (define-key help-map "f" 'nu-describe-function)
   (define-key help-map "v" 'nu-describe-variable)
   (define-key help-map (kbd "C-h") nil)

   ; SPC calls evil-insert-state-map. With which-key it's a CheatSheet.
   (define-key help-map (kbd "<SPC>") 'nu-cheat-sheet)
   (define-key nu-menu-map (kbd "h") 'help-map)

   ; C-m stands for enter.
   (define-key evil-insert-state-map (kbd "M-m") 'newline-and-indent)

   ;;
   ;; wxcvbn
   ;;

   (nu-mode-dkey evil-insert-state-map "w" t nu-quit-document nil)
   (define-key nu-menu-map (kbd "w") 'nu-window-map)
   (nu-mode-dkey evil-insert-state-map "w" nil nu-window-prompt nil)
 
   (nu-mode-dkey evil-insert-state-map "x" nil nu-delete-prompt nil)
   (nu-mode-dkey evil-insert-state-map "x" t evil-delete nil)
   (nu-mode-dkey evil-normal-state-map "x" t evil-delete nil)
   (define-key nu-menu-map (kbd "x") 'nu-delete-prompt)

   (nu-mode-dkey evil-insert-state-map "c" nil nu-copy-prompt nil)
   (nu-mode-dkey evil-insert-state-map "c" t nu-copy-region-or-line nil)
   (nu-mode-dkey evil-normal-state-map "c" t nu-copy-region-or-line nil)

   (nu-mode-dkey evil-insert-state-map "v" nil nu-insert-prompt nil)
   (nu-mode-dkey evil-insert-state-map "v" t yank nil)
   (define-key nu-menu-map (kbd "v") 'nu-insert-map)

   (nu-mode-dkey evil-insert-state-map "b" t nu-bold nil)
   (define-key nu-menu-map (kbd "b") 'nu-bold-map)
   (nu-mode-dkey evil-insert-state-map "b" nil nu-bold-prompt nil)

   (nu-mode-dkey evil-insert-state-map "n" t nu-new-empty-buffer nil)
   (nu-mode-dkey evil-insert-state-map "n" nil nu-new-prompt nil)

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
	(nu-populate-replace))


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
       (nu-define-prefix 'nu-a-map)
       (define-key nu-a-map (kbd "r") 'nu-visual-block)
       (define-key nu-a-map (kbd "l") 'evil-visual-line)
       (define-key nu-a-map (kbd "k") 'evil-visual-char)))))


(provide 'nu-mode)

;;; nu-mode.el ends here
