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

;;
;; not mapped
;; ~~~~~~~~~~
;;
;; markdown-enter-key ;; bound to enter
;; markdown-outdent-or-delete ;; bound to backspace
;; markdown-shifttab  ;; S-tab
;; markdown-table-backward-cell ; M-tab
;; markdown-table-forward-cell ; tab
;;
;; markdown-move-list-item-down ;; markdown-move-down
;; markdown-move-list-item-up ;; markdown-move-up
;; markdown-move-subtree-down ;; markdown-move-down
;; markdown-move-subtree-up ;; markdown-move-up
;; markdown-table-move-row-down ;; same!
;; markdown-table-move-row-up ;; same!
;; markdown-table-move-row ;; same!
;; markdown-table-move-row-up ;; same!
;; markdown-cycle ;; tab or shifttab
;; markdown-backward-cell ;; shifttab
;; markdown-forward-cell ;; cycle
;;
;; markdown-exdent-or-delete ;; obsolete
;; markdown-exdent-region ;; obsolete
;; markdown-insert-inline-link-dwim ;; obsolete
;; markdown-insert-reference-link-dwim ;; obsolete
;;
;; markdown-view-mode ;; triggered when necessary
;; markdown-mode-menu ;; not interactive

;; mapping
;; ~~~~~~~

(defun nu-prepare-markdown-mode-internal ()
   ;; CHANGE-BOLD MAP
   (define-key nu-change-map (kbd "A") 'markdown-fill-paragraph)
   (define-key nu-change-map (kbd "B") 'markdown-blockquote-region)
   (define-key nu-change-map (kbd "C") 'markdown-toggle-gfm-checkbox)
   (define-key nu-change-map (kbd "D") 'markdown-demote)
   (define-key nu-change-map (kbd "E") 'markdown-table-move-column-left)
   (define-key nu-change-map (kbd "F") 'markdown-table-move-column-right)
   (define-key nu-change-map (kbd "G") 'markdown-demote-list-item)
   (define-key nu-change-map (kbd "H") 'markdown-demote-subtree)
   (define-key nu-change-map (kbd "I") 'markdown-indent-region)
   (define-key nu-change-map (kbd "J") 'markdown-table-align)
   (define-key nu-change-map (kbd "L") 'markdown-move-down)
   (define-key nu-change-map (kbd "M") 'markdown-move-up)
   (define-key nu-change-map (kbd "N") 'markdown-indent-line)
   (define-key nu-change-map (kbd "O") 'markdown-outdent-region)
   (define-key nu-change-map (kbd "P") 'markdown-pre-region)
   (define-key nu-change-map (kbd "R") 'markdown-promote)
   (define-key nu-change-map (kbd "U") 'markdown-promote-list-item)
   (define-key nu-change-map (kbd "V") 'markdown-promote-subtree)

   ;; REPLACE MAP
   (define-key nu-replace-map (kbd "S") 'markdown-table-sort-lines)
   (define-key nu-replace-map (kbd "T") 'markdown-table-transpose)

   ;; PRINT MAP
   (define-key nu-print-map (kbd "A") 'markdown-complete-at-point)
   (define-key nu-print-map (kbd "B") 'markdown-live-preview-export)
   (define-key nu-print-map (kbd "C") 'markdown-check-refs)
   (define-key nu-print-map (kbd "D") 'markdown-cleanup-list-numbers)
   (define-key nu-print-map (kbd "E") 'markdown-export-and-preview)
   (define-key nu-print-map (kbd "F") 'markdown-complete-buffer)
   (define-key nu-print-map (kbd "G") 'markdown-complete)
   (define-key nu-print-map (kbd "H") 'markdown-export)
   (define-key nu-print-map (kbd "I") 'markdown-indent-region)
   (define-key nu-print-map (kbd "J") 'markdown-kill-ring-save)
   (define-key nu-print-map (kbd "K") 'markdown-standalone)
   (define-key nu-print-map (kbd "O") 'markdown-other-window)
   (define-key nu-print-map (kbd "P") 'markdown-preview)
   (define-key nu-print-map (kbd "R") 'markdown-complete-region)
   (define-key nu-print-map (kbd "T") 'markdown-table-convert-region)
   (define-key nu-print-map (kbd "U") 'markdown-unused-refs)
   (define-key nu-print-map (kbd "V") 'markdown-live-preview-re-export)

   ;; KILL MAP
   (define-key nu-kill-map (kbd "C") 'markdown-table-delete-column)
   (define-key nu-kill-map (kbd "E") 'markdown-remove-header)
   (define-key nu-kill-map (kbd "F") 'markdown-footnote-kill)
   (define-key nu-kill-map (kbd "R") 'markdown-table-delete-row)
   (define-key nu-kill-map (kbd "T") 'markdown-kill-thing-at-point)
 
   ;; INSERT MAP
   (define-key nu-insert-map (kbd "1") 'markdown-insert-header-atx-1)
   (define-key nu-insert-map (kbd "2") 'markdown-insert-header-atx-2)
   (define-key nu-insert-map (kbd "3") 'markdown-insert-header-atx-3)
   (define-key nu-insert-map (kbd "4") 'markdown-insert-header-atx-4)
   (define-key nu-insert-map (kbd "5") 'markdown-insert-header-atx-5)
   (define-key nu-insert-map (kbd "6") 'markdown-insert-header-atx-6)
   (define-key nu-insert-map (kbd "A") 'markdown-insert-blockquote)
   (define-key nu-insert-map (kbd "B") 'markdown-insert-bold)
   (define-key nu-insert-map (kbd "C") 'markdown-insert-code)
   (define-key nu-insert-map (kbd "D") 'markdown-insert-footnote)
   (define-key nu-insert-map (kbd "E") 'markdown-insert-gfm-code-block)
   (define-key nu-insert-map (kbd "F") 'markdown-insert-gfm-checkbox)
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
   (define-key nu-insert-map (kbd "R") 'markdown-electric-backquote)
   (define-key nu-insert-map (kbd "S") 'markdown-insert-strike-through)
   (define-key nu-insert-map (kbd "U") 'markdown-insert-uri)
   (define-key nu-insert-map (kbd "W") 'markdown-insert-wiki-link)
   (define-key nu-insert-map (kbd "X") 'markdown-table-insert-row)
   (define-key nu-insert-map (kbd "Y") 'markdown-table-insert-column)

   ;; OPEN MAP
   (define-key nu-open-map (kbd "B") 'markdown-edit-code-block)
   (define-key nu-open-map (kbd "O") 'markdown-open)
   (define-key nu-open-map (kbd "P") 'markdown-live-preview-switch-to-output)

   ;; DISPLAY PROMPT
   (define-key nu-display-map (kbd "A") 'markdown-narrow-to-page)
   (define-key nu-display-map (kbd "B") 'markdown-narrow-to-block)
   (define-key nu-display-map (kbd "C") 'markdown-show-children)
   (define-key nu-display-map (kbd "E") 'markdown-hide-sublevels)
   (define-key nu-display-map (kbd "S") 'markdown-hide-subtree)
   (define-key nu-display-map (kbd "G") 'markdown-show-all)
   (define-key nu-display-map (kbd "I") 'markdown-display-inline-images)
   (define-key nu-display-map (kbd "J") 'markdown-reload-extensions)
   (define-key nu-display-map (kbd "S") 'markdown-show-subtree)
   (define-key nu-display-map (kbd "L") 'markdown-show-version)
   (define-key nu-display-map (kbd "M") 'markdown-remove-inline-images)
   (define-key nu-display-map (kbd "N") 'markdown-unfontify-region-wiki-links)
   (define-key nu-display-map (kbd "Z") 'markdown-narrow-to-subtree)
   (define-key nu-display-map (kbd "D") 'markdown-fontify-buffer-wiki-links)
   (define-key nu-display-map (kbd "W") 'markdown-check-change-for-wiki-link)
   (define-key nu-display-map (kbd "X") 'markdown-mode-info)
   (define-key nu-display-map (kbd "Y") 'markdown-hide-body)

   ;; SWITCHES
   (define-key nu-switch-map (kbd "F") 'markdown-toggle-fontify-code-blocks-natively)
   (define-key nu-switch-map (kbd "H") 'markdown-toggle-math)
   (define-key nu-switch-map (kbd "I") 'markdown-toggle-inline-images)
   (define-key nu-switch-map (kbd "L") 'markdown-live-preview-mode)
   (define-key nu-switch-map (kbd "M") 'markdown-toggle-markup-hiding)
   (define-key nu-switch-map (kbd "U") 'markdown-toggle-url-hiding)
   (define-key nu-switch-map (kbd "W") 'markdown-toggle-wiki-links)

   ;; MARK
   (define-key nu-mark-map (kbd "A") 'markdown-mark-page)
   (define-key nu-mark-map (kbd "B") 'markdown-mark-block)
   (define-key nu-mark-map (kbd "H") 'markdown-mark-paragraph)
   (define-key nu-mark-map (kbd "S") 'markdown-mark-subtree)
   (define-key nu-mark-map (kbd "T") 'markdown-mark-text-block)

   ;; GOTO MAP
   (define-key nu-goto-map (kbd "L") 'markdown-follow-link-at-point)
   (define-key nu-goto-map (kbd "T") 'markdown-follow-thing-at-point)
   (define-key nu-goto-map (kbd "W") 'markdown-follow-wiki-link-at-point)

   ;; MOVE-TO MAP
;;- [ ] markdown-reference-goto-link
;;- [ ] markdown-table-next-row
   ;; backward
   (define-key nu-move-map (kbd "A") 'markdown-backward-page)
   (define-key nu-move-map (kbd "B") 'markdown-backward-block)
   (define-key nu-move-map (kbd "E") 'markdown-previous-visible-heading)
   (define-key nu-move-map (kbd "F") 'markdown-footnote-return)
   (define-key nu-move-map (kbd "H") 'markdown-backward-paragraph)
   (define-key nu-move-map (kbd "L") 'markdown-up-list)
   (define-key nu-move-map (kbd "N") 'markdown-previous-link)
   (define-key nu-move-map (kbd "O") 'markdown-outline-previous)
   (define-key nu-move-map (kbd "P") 'markdown-outline-previous-same-level)
   (define-key nu-move-map (kbd "R") 'markdown-reference-goto-link)
   (define-key nu-move-map (kbd "S") 'markdown-backward-same-level)
   (define-key nu-move-map (kbd "U") 'markdown-outline-up)
   (define-key nu-move-map (kbd "Y") 'markdown-up-heading)

   ;; forward
   (define-key nu-move-map (kbd "a") 'markdown-forward-page)
   (define-key nu-move-map (kbd "b") 'markdown-forward-block)
   (define-key nu-move-map (kbd "d") 'markdown-do)
   (define-key nu-move-map (kbd "e") 'markdown-next-visible-heading)
   (define-key nu-move-map (kbd "f") 'markdown-footnote-goto-text)
   (define-key nu-move-map (kbd "h") 'markdown-forward-paragraph)
   (define-key nu-move-map (kbd "n") 'markdown-next-link)
   (define-key nu-move-map (kbd "o") 'markdown-outline-next)
   (define-key nu-move-map (kbd "p") 'markdown-outline-next-same-level)
   (define-key nu-move-map (kbd "r") 'markdown-reference-goto-definition)
   (define-key nu-move-map (kbd "t") 'markdown-table-next-row)
   (define-key nu-move-map (kbd "s") 'markdown-forward-same-level)

   ;; prefix (beginning/end)
   (define-key nu-move-map (kbd "g B") 'markdown-beginning-of-text-block)
   (define-key nu-move-map (kbd "g L") 'markdown-beginning-of-list)
   (define-key nu-move-map (kbd "g b") 'markdown-end-of-text-block)
   (define-key nu-move-map (kbd "g l") 'markdown-end-of-list))


(defun nu-prepare-markdown-mode ()

   (add-hook 'nu-populate-hook '(lambda ()
     (if (eq nu-major-mode 'markdown-mode)
   	(nu-prepare-markdown-mode-internal)))))

;; add hook
(add-hook 'markdown-mode-hook 'nu-prepare-markdown-mode)

(provide 'nu-markdown)
