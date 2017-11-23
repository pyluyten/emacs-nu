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

  ;; force insert state everywhere.
  (evil-set-initial-state 'which-key-mode 'insert)

  ;; redefine insert state map 
  ;; easiest is to just take nu-mode like keymap,
  ;; then slightly adapt
  (setq nu-use-vi-paddle t)
  (nu-restore-default-keymap)
  (setq evil-insert-state-map nu-keymap)

  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "M-a") 'evil-visual-char)
  
  ;; which key key = ²
  (define-key evil-normal-state-map (kbd "²") evil-normal-state-map)
  (define-key evil-insert-state-map (kbd "²") evil-insert-state-map)
  (define-key evil-visual-state-map (kbd "²") evil-visual-state-map)

  ;; SPACE key
  (setq nu-evil-map (make-sparse-keymap))
  (define-key evil-normal-state-map (kbd "<SPC>") nu-evil-map)

  (define-key nu-evil-map "z" 'undo-tree-visualize)
  (define-key nu-evil-map "r" 'nu-replace-prompt)
  (define-key nu-evil-map "p" 'nu-print-prompt)
  (define-key nu-evil-map "q" 'nu-quit-prompt)
  (define-key nu-evil-map "s" 'nu-save-prompt)
  (define-key nu-evil-map "d" 'hydra-nu-meta-menu/body)
  (define-key nu-evil-map "f" 'nu-find-prompt)
  (define-key nu-evil-map "g" 'nu-goto-prompt)
  (define-key nu-evil-map "h" 'help-map)
  (define-key nu-evil-map "w" 'nu-window-prompt)
  (define-key nu-evil-map "x" 'nu-delete-prompt)
  (define-key nu-evil-map "c" 'nu-copy-prompt)
  (define-key nu-evil-map "v" 'nu-insert-prompt)
  (define-key nu-evil-map "b" 'nu-bold-prompt)
  (define-key nu-evil-map "o" 'nu-open-prompt)
  (define-key nu-evil-map "n" 'nu-new-prompt)
  (define-key nu-evil-map (kbd "<SPC>") 'nu-M-x)

  ;;
  ;; now adapt menus
  ;;
  (add-hook 'nu-populate-hook '(lambda ()
    (progn
       ;; reset!
       (nu-define-prefix 'nu-a-map)
       ;; so instead we use evil

        ;; todo
        ;; a => all buffer
        ;; s mark sexp
        ;; g paragraph
        ;; p page
        ;; f defun
       (define-key nu-a-map (kbd "r") 'evil-visual-block)
       (define-key nu-a-map (kbd "l") 'evil-visual-line)
       (define-key nu-a-map (kbd "k") 'evil-visual-char))))

  ;; finally trigger evil-mode
  (evil-mode))

(provide 'nu-state)

