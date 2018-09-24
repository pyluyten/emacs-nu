;;; nu-markdown.el --- Modern Emacs Keybinding
;;; Emacs-Nu is an emacs mode which wants to makes Emacs easier.
;;; Copyright (C) 2018 Pierre-Yves LUYTEN
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
;; nu-mode / markdown-mode integration
;;

(defun nu-prepare-markdown-mode ()


   (add-hook 'nu-populate-hook '(lambda ()
     (if (eq nu-major-mode 'markdown-mode)
   	(progn

         ;; BOLD MAP
         (define-key nu-change-map (kbd "B") 'markdown-blockquote-region)
         (define-key nu-change-map (kbd "D") 'markdown-demote)
         (define-key nu-change-map (kbd "C") 'markdown-toggle-gfm-checkbox)
         (define-key nu-change-map (kbd "I") 'markdown-indent-region)
         (define-key nu-change-map (kbd "L") 'markdown-move-down)
         (define-key nu-change-map (kbd "M") 'markdown-move-up)
         (define-key nu-change-map (kbd "O") 'markdown-outdent-region)
         (define-key nu-change-map (kbd "P") 'markdown-pre-region)
         (define-key nu-change-map (kbd "R") 'markdown-promote)
         (define-key nu-change-map (kbd "S") 'markdown-table-sort-lines)
         (define-key nu-change-map (kbd "T") 'markdown-table-transpose)
	 
	 ;; PRINT MAP
         (define-key nu-print-map (kbd "I") 'ispell-complete-word)
         (define-key nu-print-map (kbd "C") 'markdown-check-refs)
         (define-key nu-print-map (kbd "D") 'markdown-cleanup-list-numbers)
         (define-key nu-print-map (kbd "E") 'markdown-export-and-preview)
         (define-key nu-print-map (kbd "F") 'markdown-complete-buffer)
         (define-key nu-print-map (kbd "G") 'markdown-complete)
         (define-key nu-print-map (kbd "H") 'markdown-export)
         (define-key nu-print-map (kbd "I") 'markdown-indent-region)
         (define-key nu-print-map (kbd "J") 'markdown-kill-ring-save)
         (define-key nu-print-map (kbd "O") 'markdown-other-window)
         (define-key nu-print-map (kbd "P") 'markdown-preview)
         (define-key nu-print-map (kbd "T") 'markdown-table-convert-region)
         (define-key nu-print-map (kbd "U") 'markdown-unused-refs)

         ;; DELETE MAP
         (define-key nu-kill-map (kbd "K") 'markdown-kill-thing-at-point)
   	 (define-key nu-kill-map (kbd "T") 'markdown-table-delete-column)
   	 (define-key nu-kill-map (kbd "R") 'markdown-table-delete-row)
	 
         ;; INSERT MAP
         (define-key nu-insert-map (kbd "A") 'markdown-insert-blockquote)
         (define-key nu-insert-map (kbd "B") 'markdown-insert-bold)
         (define-key nu-insert-map (kbd "C") 'markdown-insert-code)
         (define-key nu-insert-map (kbd "D") 'markdown-insert-footnote)
         (define-key nu-insert-map (kbd "E") 'markdown-insert-gfm-code-block)
         (define-key nu-insert-map (kbd "F") 'markdown-insert-gfm-checkbox)
         (define-key nu-insert-map (kbd "1") 'markdown-insert-header-atx-1)
         (define-key nu-insert-map (kbd "2") 'markdown-insert-header-atx-2)
         (define-key nu-insert-map (kbd "3") 'markdown-insert-header-atx-3)
         (define-key nu-insert-map (kbd "4") 'markdown-insert-header-atx-4)
         (define-key nu-insert-map (kbd "5") 'markdown-insert-header-atx-5)
         (define-key nu-insert-map (kbd "6") 'markdown-insert-header-atx-6)
         (define-key nu-insert-map (kbd "G") 'markdown-insert-header-dwim)
         (define-key nu-insert-map (kbd "H") 'markdown-insert-header-setext-1)
         (define-key nu-insert-map (kbd "I") 'markdown-insert-header-setext-2)
         (define-key nu-insert-map (kbd "J") 'markdown-insert-header-setext-dwim)
         (define-key nu-insert-map (kbd "K") 'markdown-insert-hr)
         (define-key nu-insert-map (kbd "L") 'markdown-insert-image)
         (define-key nu-insert-map (kbd "M") 'markdown-insert-italic)
         (define-key nu-insert-map (kbd "N") 'markdown-insert-kbd)
         (define-key nu-insert-map (kbd "O") 'markdown-insert-link)
         (define-key nu-insert-map (kbd "P") 'markdown-insert-list-item)
         (define-key nu-insert-map (kbd "Q") 'markdown-insert-pre)
         (define-key nu-insert-map (kbd "S") 'markdown-insert-strike-through)
         (define-key nu-insert-map (kbd "U") 'markdown-insert-uri)
         (define-key nu-insert-map (kbd "W") 'markdown-insert-wiki-link)
         (define-key nu-insert-map (kbd "W") 'markdown-insert-wiki-link)
         (define-key nu-insert-map (kbd "W") 'markdown-insert-wiki-link)
         (define-key nu-insert-map (kbd "X") 'markdown-table-insert-row)
         (define-key nu-insert-map (kbd "Y") 'markdown-table-insert-column)

         ;; OPEN MAP
         (define-key nu-open-map (kbd "E") 'markdown-edit-code-block)
         (define-key nu-open-map (kbd "O") 'markdown-open)

         ;; DISPLAY PROMPT
	 (define-key nu-display-map (kbd "B") 'markdown-narrow-to-block)
	 (define-key nu-display-map (kbd "S") 'markdown-narrow-to-subtree)

          ;; SWITCHES
          (define-key nu-switch-map (kbd "L") 'markdown-live-preview-mode)
          (define-key nu-switch-map (kbd "I") 'markdown-toggle-inline-images)
          (define-key nu-switch-map (kbd "F") 'markdown-toggle-fontify-code-blocks-natively)
          (define-key nu-switch-map (kbd "M") 'markdown-toggle-markup-hiding)
          (define-key nu-switch-map (kbd "T") 'markdown-toggle-math)
          (define-key nu-switch-map (kbd "U") 'markdown-toggle-url-hiding)

          ;; MARK
	  (define-key nu-mark-map (kbd "B") 'markdown-mark-block)
	  (define-key nu-mark-map (kbd "F") 'markdown-mark-subtree)

         ;; GOTO MAP
         (define-key nu-goto-map (kbd "B") 'markdown-backward-block)
         (define-key nu-goto-map (kbd "D") 'markdown-do)
         (define-key nu-goto-map (kbd "F") 'markdown-follow-thing-at-point)
         (define-key nu-goto-map (kbd "G") 'markdown-forward-block)
         (define-key nu-goto-map (kbd "N") 'markdown-next-link)
         (define-key nu-goto-map (kbd "R") 'markdown-outline-next)
         (define-key nu-goto-map (kbd "O") 'markdown-outline-next-same-level)
         (define-key nu-goto-map (kbd "S") 'markdown-outline-previous)
         (define-key nu-goto-map (kbd "Q") 'markdown-outline-previous-same-level)
         (define-key nu-goto-map (kbd "T") 'markdown-outline-up)
         (define-key nu-goto-map (kbd "U") 'markdown-previous-link))))))

;; add hook
(add-hook 'markdown-mode-hook 'nu-prepare-markdown-mode)

(provide 'nu-markdown)
