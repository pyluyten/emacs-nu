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

;;
;; nu state
;;
;; this state redefines evil insert state
;; it does force new buffers being in insert state
;; it does enforce the proper hooks with visual mode (lv-message)
;;
;; integration is very limited since insert state do not
;; conflict with read only modes like ibuffer
;;

(require 'evil)
(require 'nu-mode)

(defun nu-state-help-prompt ()
  (interactive)
  (lv-message
    (concat
      (propertize "\n Welcome to nu-state\n\n" 'face 'bold)
      " This screen does provide some help to use nu-state.\n It is shown at startup.\n"
      " Enter any key to quit this prompt or "(propertize "Space" 'face 'nu-face-shortcut)
      " to obtain the cheat sheet."
      "\n To disable this screen, put this in your init file\n\n"
        (propertize " (require 'nu-state)\n" 'face 'italic)
	(propertize " (setq nu-show-welcome-screen nil)\n" 'face 'error)
      "\n\n To obtain Help, use "
      (propertize "Control+h" 'face 'nu-face-shortcut)
      "\n For example, to obtain a Cheat Sheet, use "
      (propertize "Control+h Space" 'face 'nu-face-shortcut)
      "\n\n To enter a command (M-x in vanilla Emacs), use "
      (propertize "Control+d" 'face 'nu-face-shortcut)
      ".\n To quit a command, use "
      (propertize "Alt+q" 'face 'nu-face-shortcut)))
  (setq answer (read-key ""))
  (lv-delete-window)
  (if (eq answer 32)
      (nu-cheat-sheet)))


(defun nu-state-set-ctrl-func ()
  "Define some control keys. By default do not override vi keys."

  ;; Control+o (alternative is space o)
  ;; push the vi keys to open menu;
  ;; use control+o to invoke this menu
  ;; do not shadown native C-o.
   (define-key evil-normal-state-map (kbd "C-o") 'nu-open-prompt)

  ;; C-c
   (global-set-key (kbd "C-<SPC>") 'nu-trigger-mode-specific-map))



(defun nu-state-set-alt-func ()
 "populate alt keys, with vi disposition, to invoke
nu specific immediate funcs and menus.

This includes the paddle."

   ;; TODO : see if required here
   (global-set-key (kbd "<menu>") nu-menu-map)

   ;;
   ;; setup the paddle. see setup.el.
   ;;
   (nu-setup-vi-paddle)

   (global-set-key (kbd "M-a") 'evil-normal-state)
   (global-set-key (kbd "M-b") 'backward-word)
   (global-set-key (kbd "M-c") 'nu-bold-prompt)
   (global-set-key (kbd "M-d") 'nu-delete-prompt)
   (global-set-key (kbd "M-e") 'forward-word)
   (global-set-key (kbd "M-f") 'nu-find-prompt)
   (global-set-key (kbd "M-g") 'ace-window) ; menu is rare => space g
   (global-set-key (kbd "M-h") 'backward-char)
   (global-set-key (kbd "M-i") 'nu-back-to-indentation)
   (global-set-key (kbd "M-j") 'next-line)
   (global-set-key (kbd "M-k") 'previous-line)
   (global-set-key (kbd "M-l") 'forward-char)
   (global-set-key (kbd "M-m") 'newline-and-indent)
   (global-set-key (kbd "M-n") 'nu-new-prompt)
   (global-set-key (kbd "M-o") 'nu-do-prompt)
   (global-set-key (kbd "M-p") 'yank) ; menu is rare => space p 
   (global-set-key (kbd "M-q") 'nu-print-prompt)
   (global-set-key (kbd "M-r") 'nu-replace-prompt)
   (global-set-key (kbd "M-s") 'save-buffer) ; menu is rare. space s
   (global-set-key (kbd "M-t") 'split-window-right)
   (global-set-key (kbd "M-u") 'undo-tree-visualize)
   (global-set-key (kbd "M-w") 'nu-quit-document) ; menu is space
   (global-set-key (kbd "M-x") 'nu-M-x)
   (global-set-key (kbd "M-y") 'nu-copy-region-or-line) ; menu is space
   (global-set-key (kbd "M-z") 'nu-quit-prompt)

   (global-set-key (kbd "M-<SPC>") 'scroll-up)
   (global-set-key (kbd "M-<backspace>") 'scroll-down)

   (define-key help-map "f" 'nu-describe-function)
   (define-key help-map "v" 'nu-describe-variable)
   (define-key help-map (kbd "<SPC>") 'nu-cheat-sheet))


(defun nu-state ()
  "Requires evil and nu and enables evil mode.

Redefines evil insert state.
Enforces new buffers being insert state."

  ;; which key mode + menus init
  (nu-initialize)
  (defalias 'nu-prompt-for-keymap 'nu-which-key-prompt-for-keymap)

  ;; TODO : fix this help prompt
  (when nu-show-welcome-screen
	   (add-hook 'emacs-startup-hook '(lambda ()
             (nu-state-help-prompt))))

  ;; because we have alt to navigate in mode like
  ;; ibuffer, dired, mu4e, or so.
  ;; so, really, we do not need emacs mode.
  ;(evil-set-initial-state 'which-key-mode 'normal)
  (setq evil-default-state 'normal)

  (nu-state-set-alt-func)
  (nu-state-set-ctrl-func)

  ;; which key key = ²
  (global-set-key "²" 'which-key-show-top-level)

  ;; SPACE key
  (setq nu-evil-map (make-sparse-keymap))
  (define-key evil-normal-state-map (kbd "<SPC>") nu-evil-map)
  (define-key nu-evil-map "b" 'nu-bold-prompt)
  (define-key nu-evil-map "d" 'nu-delete-prompt)
  (define-key nu-evil-map "e" 'nu-do-prompt)
  (define-key nu-evil-map "f" 'nu-find-prompt)
  (define-key nu-evil-map "g" 'nu-goto-prompt)
  (define-key nu-evil-map "h" 'help-map)
  (define-key nu-evil-map "n" 'nu-new-prompt)
  (define-key nu-evil-map "o" 'nu-open-prompt)
  (define-key nu-evil-map "p" 'nu-insert-prompt)
  (define-key nu-evil-map "q" 'nu-print-prompt)
  (define-key nu-evil-map "r" 'nu-replace-prompt)
  (define-key nu-evil-map "s" 'nu-save-prompt)
  (define-key nu-evil-map "p" 'nu-insert-prompt)
  (define-key nu-evil-map "w" 'nu-window-prompt)
  (define-key nu-evil-map "y" 'nu-copy-prompt)
  (define-key nu-evil-map "z" 'undo-tree-visualize)
  (define-key nu-evil-map (kbd "<SPC>") 'nu-M-x)

  ;;
  ;; now adapt menus
  ;;
  (add-hook 'nu-populate-hook '(lambda ()
    (progn

       ;; inserted inside open menu
       (define-key nu-open-map "o" 'evil-jump-backward)

       ;; reset!
       (nu-define-prefix 'nu-a-map)
       (define-key nu-a-map (kbd "r") 'evil-visual-block)
       (define-key nu-a-map (kbd "l") 'evil-visual-line)
       (define-key nu-a-map (kbd "k") 'evil-visual-char))))

  ;; finally trigger evil-mode
  (evil-mode))

(provide 'nu-state)

