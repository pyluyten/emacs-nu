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

;; setup

(defun nu-setup-ivy ()
  ;; completion prompt setup.
  (defalias 'nu-completion-prompt-for-keymap 'nu-ivy-prompt-for-keymap)

  ;; usual commands.
  (defalias 'nu-search 'swiper)
  (defalias 'nu-M-x 'counsel-M-x)
  (defalias 'nu-find-files 'counsel-find-file)
  (defalias 'nu-buffers-list 'ivy-switch-buffer)
  (defalias 'nu-describe-function 'counsel-describe-function)
  (defalias 'nu-describe-variable 'counsel-describe-variable)
  (defalias 'nu-bookmarks 'list-bookmarks)
  (defalias 'nu-recentf 'ivy-recentf)
  (defalias 'nu-browse-kill-ring 'counsel-yank-pop))

;; prompt

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

;; minibuffer

(defun nu-set-minibuffer-ivy ()
  ""

  ;; ivy minibuffer map needs tweaking,
  ;; swiper map does not needs.

  (if (boundp 'ivy-minibuffer-map)
      (progn
	;; standard ivy map
        ;; normaly M-i	= ivy-insert-current -> rebind to M-l

        (define-key ivy-minibuffer-map (kbd "M-h") ivy-minibuffer-map)
        (define-key ivy-minibuffer-map (kbd "M-i") 'previous-line)
        (define-key ivy-minibuffer-map (kbd "M-k") 'next-line)
        (define-key ivy-minibuffer-map (kbd "M-m") 'ivy-immediate-done)
        (define-key ivy-minibuffer-map (kbd "M-l") 'ivy-insert-current))))

(defadvice nu-prepare-for-minibuffer (after nu-prepare-for-minibuffer-ivy-advice ())
  (nu-set-minibuffer-ivy))

(ad-activate 'nu-prepare-for-minibuffer)

;; ivy read advice

(defun nu-prepare-for-ivy ()
  (lv-message
   (concat
    "Type or press "
    (propertize "M-h" 'face 'nu-face-shortcut) " for help, "
    (propertize "M-q" 'face 'nu-face-shortcut) " to quit "
    (propertize "C-M-j" 'face 'nu-face-shortcut)" to validate current input.")))

(defadvice ivy-read (before nu-prepare-for-ivy-read-advice ())
  (nu-prepare-for-ivy))

(ad-activate 'ivy-read)

(provide 'nu-ivy)
