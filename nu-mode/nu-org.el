;;; nu-org.el --- Modern Emacs Keybinding
;;; Emacs-Nu is an emacs mode which wants to makes Emacs easier.kk
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
;; nu-mode / org-mode integration
;;

;; http://orgmode.org/tmp/worg/org-configs/org-hooks.html

;; FIXME -org-mode as a minor mode is not handled.

;; TODO : 1st, some emacs keys are remapped in org
;;        maybe let this, as some people will use evil-org anyway
;;      C-y org-yank
;;      C-a org-beginning-of-line
;;      C-e org-end-of-line
;;      C-k org-kill-line
;;      M-a org-backward-sentence
;;      M-e org-forward-sentence
;;      M-h org-mark-element
;;      M { org backward element
;;      M } org forward element
;;
;;      2nd, some func are too context dependant
;;           and do not make much sense in a menu
;;           org-kill-note-or-show-branches
;;
;;      3rd, some emacs func are remapped, but evil funcs are not the same
;;           maybe it's the same, if user wants something he does something.
;;           it's not nu-menu scope to rebind normal keys

;; <remap> <backward-paragraph>			org-backward-paragraph
;; <remap> <comment-dwim>			org-comment-dwim
;; <remap> <delete-backward-char>		org-delete-backward-char
;; <remap> <delete-char>			org-delete-char
;; <remap> <fill-paragraph>			org-fill-paragraph
;; <remap> <forward-paragraph>			org-forward-paragraph
;; <remap> <open-line>				org-open-line
;; <remap> <outline-backward-same-level>	org-backward-heading-same-level
;; <remap> <outline-demote>			org-demote-subtree
;; <remap> <outline-forward-same-level>		org-forward-heading-same-level
;; <remap> <outline-insert-heading>		org-ctrl-c-ret
;; <remap> <outline-mark-subtree>		org-mark-subtree
;; <remap> <outline-next-visible-heading>	org-next-visible-heading
;; <remap> <outline-previous-visible-heading>	org-previous-visible-heading
;; <remap> <outline-promote>			org-promote-subtree
;; <remap> <outline-show-branches>		org-kill-note-or-show-branches
;; <remap> <outline-show-subtree>		org-show-subtree
;; <remap> <self-insert-command>		org-self-insert-command
;; <remap> <show-children>			org-show-children
;; <remap> <transpose-words>			org-transpose-words


(defun nu-prepare-org-mode ()

  ;; FIXME NO DEFADVICE
   (defadvice nu-set-bold-f (after nu-set-bold-f-for-org ())
     (if (eq major-mode 'org-mode)
	 (defalias 'nu-bold-function 'org-emphasize)))
   (ad-activate 'nu-set-bold-f)

   (add-hook 'nu-populate-hook '(lambda ()
     (if (eq nu-major-mode 'org-mode)
   	(progn

          ;; CHANGE
          (define-key nu-change-map (kbd "B") 'org-babel-demarcate-block)
          (define-key nu-change-map (kbd "D") 'org-decrease-number-at-point)
          (define-key nu-change-map (kbd "J") 'org-shiftleft)
          (define-key nu-change-map (kbd "K") 'org-shiftdown)
          (define-key nu-change-map (kbd "L") 'org-shiftright)
          (define-key nu-change-map (kbd "I") 'org-shiftup)
          (define-key nu-change-map (kbd "T") 'org-todo)
          (define-key nu-change-map (kbd "C") 'org-toggle-checkbox)
          (define-key nu-change-map (kbd "U") 'org-dblock-update)
          (define-key nu-change-map (kbd "E") 'org-emphasize)
          (define-key nu-change-map (kbd "S") 'org-list-make-subtree)

          ;; COPY
          (define-key nu-copy-map (kbd "O") 'org-copy)
          (define-key nu-copy-map (kbd "S") 'org-copy-special)
          (define-key nu-copy-map (kbd "V") 'org-copy-visible)

	  ;; DELETE
	  (define-key nu-kill-map (kbd "C") 'org-table-delete-column)
          (define-key nu-kill-map (kbd "R") 'org-table-kill-row)
          (define-key nu-kill-map (kbd "S") 'org-cut-special)
          (define-key nu-kill-map (kbd "T") 'org-cut-subtree)
          (define-key nu-kill-map (kbd "B") 'org-babel-remove-result-one-or-many)

	  ;; DISPLAY
	  (define-key nu-display-map (kbd "S") 'org-sparse-tree)
	  (define-key nu-display-map (kbd "C") 'org-clock-display)

	  ;; GOTO
          (define-key nu-goto-map (kbd "I") 'org-backward-heading-same-level)
          (define-key nu-goto-map (kbd "K") 'org-forward-heading-same-level)
          (define-key nu-goto-map (kbd "J") 'org-backward-element)
          (define-key nu-goto-map (kbd "L") 'org-forward-element)
          (define-key nu-goto-map (kbd "H") 'org-babel-goto-src-block-head)
          (define-key nu-goto-map (kbd "B") 'org-babel-next-src-block)
          (define-key nu-goto-map (kbd "P") 'org-babel-previous-src-block)
          (define-key nu-goto-map (kbd "R") 'org-babel-goto-named-result)
          (define-key nu-goto-map (kbd "N") 'org-babel-goto-named-src-block)
          (define-key nu-goto-map (kbd "O") 'org-goto)
          (define-key nu-goto-map (kbd "C") 'org-goto-calendar)
          (define-key nu-mark-map (kbd "M") 'org-mark-ring-goto)

	  ;; INSERT
          (define-key nu-insert-map (kbd "A") 'org-attach)
          (define-key nu-insert-map (kbd "B") 'org-babel-insert-header-arg)
          (define-key nu-insert-map (kbd "C") 'org-table-insert-column)
          (define-key nu-insert-map (kbd "E") 'org-date-from-calendar)
          (define-key nu-insert-map (kbd "D") 'org-deadline)
          (define-key nu-insert-map (kbd "H") 'org-table-insert-hline)
          (define-key nu-insert-map (kbd "L") 'org-insert-link)
          (define-key nu-insert-map (kbd "M") 'org-time-stamp)
          (define-key nu-insert-map (kbd "P") 'org-paste-special)
          (define-key nu-insert-map (kbd "R") 'org-table-insert-row)
          (define-key nu-insert-map (kbd "S") 'org-paste-subtree)
          (define-key nu-insert-map (kbd "T") 'org-insert-todo-heading)
          (define-key nu-insert-map (kbd "O") 'org-insert-all-links)
          (define-key nu-insert-map (kbd "W") 'org-insert-drawer)

          ;; MARK
          (define-key nu-mark-map (kbd "S") 'org-mark-subtree)
          (define-key nu-mark-map (kbd "U") 'outline-mark-subtree-up)
          (define-key nu-mark-map (kbd "D") 'outline-mark-subtree-down)
          (define-key nu-mark-map (kbd "B") 'org-babel-mark-block)
          (define-key nu-mark-map (kbd "E") 'org-mark-element)

          ;; NEW
          (define-key nu-new-map (kbd "N") 'org-add-note)
          (define-key nu-new-map (kbd "S") 'org-match-sparse-tree)

	  ;; OPEN
	  ;; note A is already org-agenda, but implemented in nu-menus.el
          (define-key nu-open-map (kbd "G") 'org-cycle-agenda-files)
          (define-key nu-open-map (kbd "B") 'org-babel-describe-bindings)
          (define-key nu-open-map (kbd "C") 'org-cycle)
          (define-key nu-open-map (kbd "L") 'org-open-at-point)
          (define-key nu-open-map (kbd "S") 'org-babel-open-src-block-result)

          ;; PRINT EVAL
          (define-key nu-print-map (kbd "L") 'pcomplete)
          (define-key nu-print-map (kbd "O") 'org-babel-load-in-session)
          (define-key nu-print-map (kbd "B") 'org-bacel-execute-buffer)
          (define-key nu-print-map (kbd "M") 'org-bacel-execute-maybe)
          (define-key nu-print-map (kbd "S") 'org-babel-execute-subtree)
          (define-key nu-print-map (kbd "C") 'org-bacel-check-src-block)
          (define-key nu-print-map (kbd "X") 'org-bacel-execute-src-block)
          (define-key nu-print-map (kbd "K") 'org-babel-do-key-sequence-in-edit-buffer)
          (define-key nu-print-map (kbd "T") 'org-evaluate-time-range)

          ;; QUIT ARCHIVE
          (define-key nu-quit-map (kbd "S") 'org-archive-subtree)
          (define-key nu-quit-map (kbd "D") 'org-archive-subtree-default)
          (define-key nu-quit-map (kbd "B") 'org-archive-to-archive-sibling)
          (define-key nu-quit-map (kbd "C") 'org-clock-cancel)

	  ;; REPLACE
          (define-key nu-replace-map (kbd "B") 'org-metaleft)
          (define-key nu-replace-map (kbd "F") 'org-metaright)
          (define-key nu-replace-map (kbd "N") 'org-metadown)
          (define-key nu-replace-map (kbd "P") 'org-metaup)
          (define-key nu-replace-map (kbd "S") 'org-sort)
          (define-key nu-replace-map (kbd "T") 'org-table-sort-lines)
          (define-key nu-replace-map (kbd "L") 'org-table-blank-field)
          (define-key nu-replace-map (kbd "P") 'org-transpose-element)

	  ;; SAVE
          (define-key nu-save-map (kbd "O") 'org-refile)
          (define-key nu-save-map (kbd "A") 'org-save-all-org-buffers)
	  (define-key nu-save-map (kbd "G") 'org-agenda-file-to-front)
          (define-key nu-save-map (kbd "M") 'org-mark-ring-push)
          (define-key nu-save-map (kbd "M") 'org-babel-lob-ingest)

          ;; SWITCH
          (define-key nu-switch-map (kbd "A") 'org-toggle-archive-tag)
          (define-key nu-switch-map (kbd "D") 'org-toggle-comment)
          (define-key nu-switch-map (kbd "F") 'org-toggle-fixed-width)
          (define-key nu-switch-map (kbd "G") 'org-agenda-set-restriction-lock)
          (define-key nu-switch-map (kbd "I") 'org-toggle-inline-images)
          (define-key nu-switch-map (kbd "L") 'org-toggle-latex-fragment)
          (define-key nu-switch-map (kbd "O") 'org-toggle-ordered-property)
          (define-key nu-switch-map (kbd "P") 'org-toggle-pretty-entities)
          (define-key nu-switch-map (kbd "T") 'org-toggle-tags-groups)
          (define-key nu-switch-map (kbd "U") 'org-toggle-time-stamp-overlays))))))

;; add hook
(add-hook 'org-mode-hook 'nu-prepare-org-mode)


(provide 'nu-org)
