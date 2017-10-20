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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA



; we have the real keymap for real ("nu-keymap").
; menu-map is only there for where-is sake.
; thus we define things twice :
; once a grand tragedy. Once as a farce.
(defvar nu-keymap (make-sparse-keymap) "Emacs nu keymap")
(defvar nu-menu-map (make-sparse-keymap) "Nu Menu Keymap")

(require 'nu-prompters)
(require 'nu-prompters-lv)
(require 'nu-helm)
(require 'nu-ivy)


; external parts
(require 'transpose-frame) ; play with frames
(require 'nu-tile) ; still...

; internal parts
(require 'nu-menus) ; open-keymap, replace-keymap,...
(require 'nu-commands) ; not emacs native commands

(require 'nu-integration) ; how to use other modes
(require 'nu-ibuffer)
(require 'nu-isearch)

(provide 'nu-common)
