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

(defun nu-state ()
  "Requires evil and nu and enables evil mode.

Redefines evil insert state.
Enforces new buffers being insert state."

  ;; which key mode + menus init
  (nu-initialize)
  (defalias 'nu-prompt-for-keymap 'nu-which-key-prompt-for-keymap)

  ;; TODO : fix this help prompt
  (when nu-mode-show-welcome-screen
	   (add-hook 'emacs-startup-hook '(lambda ()
             (nu-help-prompt))))

  ;; force insert state everywhere.
  (evil-set-initial-state 'which-key-mode 'insert)

  ;; redefine insert state map 
  ;; easiest is to just take nu-mode like keymap,
  ;; then slightly adapt
  (nu-restore-default-keymap)
  (nu-setup-vi-paddle)
  (setq evil-insert-state-map nu-keymap)

  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "M-a") 'evil-visual-block)
  
  ;; fix menus
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

