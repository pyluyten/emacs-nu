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

(require 'nu-prompters)

(defun nu-ivy-prompt-for-keymap (keymap)
  "Use ivy mode to prompt for a keymap."
 (interactive)
 (setq nu-current-keymap keymap
       nu-keymap-list nil
       nu-describe-bind-mode "completion")
   (map-keymap 'nu-insert-binding-row keymap)
   (setq nu-last-command
      (intern-soft
          (replace-regexp-in-string "\\(\\w\\) .*" "\\1"
             (ivy-read "Execute :" nu-keymap-list))))
   (ignore-errors (call-interactively nu-last-command))
   (setq nu-repeat-prompt nil))

(provide 'nu-prompters-ivy)
