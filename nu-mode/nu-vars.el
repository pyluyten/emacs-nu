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

(defvar nu-mode-show-welcome-screen
"If true, nu-mode does display a help buffer at startup.")

(defvar nu-state t "Used by overriding maps alist.")

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
(defvar nu-window-map (make-sparse-keymap) "Nu Window, Frame Keymap")
(defvar nu-new-map (make-sparse-keymap) "Nu New, Create Keymap")
(defvar nu-a-map (make-sparse-keymap) "Nu Selection Keymap")
(defvar nu-find-map (make-sparse-keymap) "Nu Find, Grep Keymap")
(defvar nu-copy-map (make-sparse-keymap) "Nu Copy Keymap")
(defvar nu-tab-map (make-sparse-keymap) "Nu Tab Keymap")
(defvar nu-bold-map (make-sparse-keymap) "Nu Bold, Comment, Indent Keymap")




(provide 'nu-vars)
