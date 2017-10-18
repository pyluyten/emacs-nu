
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

(defvar ibuffer-mode-map)

(defun nu-prepare-for-ibuffer ()
  ""
  (define-key ibuffer-mode-map (kbd "h") ibuffer-mode-map)

  (define-key ibuffer-mode-map (kbd "M-i") 'ibuffer-backward-line)
  (define-key ibuffer-mode-map (kbd "M-k") 'ibuffer-forward-line)
  (define-key ibuffer-mode-map (kbd "M-l") 'ibuffer-visit-buffer)
  (define-key ibuffer-mode-map (kbd "M-j") 'ibuffer-visit-buffer-other-window-noselect)

  ; cancel bindings then make override.
  (nu-make-overriding-map ibuffer-mode-map
			  '("C-o" "C-y" "M-g" "M-n" "M-p" "M-s")
			  nil))


(add-hook 'ibuffer-hook          'nu-prepare-for-ibuffer)


(provide 'nu-ibuffer)
