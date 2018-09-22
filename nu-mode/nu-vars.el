;;; nu-mode.el --- Modern Emacs Keybinding
;;; Emacs-Nu is an emacs mode which wants to makes Emacs easier.kk
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

(defvar nu-show-welcome-screen
"If true, nu-mode does display a help buffer at startup.")

(defvar nu-immediate-key "M"
"Default modifier for invoking most current functions.
By default it is Alt.")

(defvar nu-menu-key "C"
"Default modifier for invoking menus or less current functions.
By default it is Control.")

(defvar nu-use-vi-paddle nil
  "If true, nu uses h j k l. Otherwise it uses i j k l.")

(defvar nu-state t "Used by overriding maps alist.")

;;
;; paddles
;;

(defvar nu-previous-line-binding (kbd "M-i") "Shortcut to move up.")
(defvar nu-backward-char-binding (kbd "M-j") "Shortcut to move left.")
(defvar nu-next-line-binding (kbd "M-k") "Shortcut to move down.")
(defvar nu-forward-char-binding (kbd "M-l") "Shortcut to move right.")
(defvar nu-back-to-indentation-binding (kbd "M-h") "Shortcut to move to bol.")

(defvar nu-previous-line-key "i" "Char to move up.")
(defvar nu-backward-char-key "j" "Char to move left.")
(defvar nu-next-line-key "k" "Char to move down.")
(defvar nu-forward-char-key "l" "Char to move right.")
(defvar nu-back-to-indentation-key "h" "Char to move to bol")

(defvar nu-kill-visual-line-key (kbd "C-k") "Shortcuts to kill vline.")
(defvar nu-del-backward-char-key (kbd "C-j") "Shortcuts to backspace.")
(defvar nu-del-forward-char-key (kbd "C-l") "Shortcuts to backspace.")

;;
;; keymaps
;;

(defvar nu-keymap (make-sparse-keymap) "nu-mode keymap")
(defvar nu-menu-map (make-sparse-keymap) "Nu Menu Keymap")
(defvar nu-major-mode nil "Major mode menus are being populated for.")

(defvar nu-visual-map (make-sparse-keymap) "Nu Selection Keymap")

(defvar nu-quit-map (make-sparse-keymap) "Nu Quit Keymap")
(defvar nu-print-map (make-sparse-keymap) "Nu Print, Eval Keymap")
(defvar nu-delete-map (make-sparse-keymap) "Nu Kill Keymap")
(defvar nu-insert-map (make-sparse-keymap) "Nu Insert Keymap")
(defvar nu-save-map (make-sparse-keymap) "Nu Save, Store, Register Keymap")
(defvar nu-open-map (make-sparse-keymap) "Nu Open Keymap")
(defvar nu-goto-map (make-sparse-keymap) "Nu Goto Keymap")
(defvar nu-replace-map (make-sparse-keymap) "Nu Replace, Change, Sort Keymap")
(defvar nu-display-map (make-sparse-keymap) "Nu Window, Frame Keymap")
(defvar nu-new-map (make-sparse-keymap) "Nu New, Create Keymap")
(defvar nu-a-map (make-sparse-keymap) "Nu Selection Keymap")
(defvar nu-find-map (make-sparse-keymap) "Nu Find, Grep Keymap")
(defvar nu-copy-map (make-sparse-keymap) "Nu Copy Keymap")
(defvar nu-tab-map (make-sparse-keymap) "Nu Tab Keymap")
(defvar nu-bold-map (make-sparse-keymap) "Nu Bold, Comment, Indent Keymap")




(provide 'nu-vars)
