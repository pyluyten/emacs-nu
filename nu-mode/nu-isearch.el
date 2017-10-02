
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

(defun nu-prepare-for-isearch ()
  "Vanilla search feature."
  (define-key isearch-mode-map (kbd "M-f") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "M-k") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "M-i") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "M-p") 'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "M-n") 'isearch-ring-advance)
  (define-key isearch-mode-map (kbd "M-v") 'isearch-yank-kill)
  (define-key isearch-mode-map (kbd "M-a") 'isearch-yank-word-or-char)
  (define-key isearch-mode-map (kbd "M-g") 'isearch-yank-line)
  (define-key isearch-mode-map (kbd "M-h") '(lambda ()
					     (interactive)
					     (describe-keymap isearch-mode-map t)))
  (define-key isearch-mode-map (kbd "M-s") 'isearch-edit-string)
  (define-key isearch-mode-map (kbd "M-d") 'isearch-cancel)
  (define-key isearch-mode-map (kbd "M-d") 'isearch-cancel)

  (lv-message "M-f or M-k / M-i : search forward / backward.\nM-q or M-d cancel search. M-h for more help"))



(defun nu-isearch-exit ()
  ""
  (lv-delete-window))


(add-hook 'isearch-mode-hook     'nu-prepare-for-isearch)
(add-hook 'isearch-mode-end-hook 'nu-isearch-exit)


(provide 'nu-isearch)
