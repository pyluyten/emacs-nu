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
;;      M { org backward elements
;;      M } org forward element
;;
;;      2nd, some emacs func are remapped, but evil funcs are not the same
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
          (define-key nu-change-map (kbd "D") 'org-decrease-number-at-point)
          (define-key nu-change-map (kbd "J") 'org-shiftleft)
          (define-key nu-change-map (kbd "K") 'org-shiftdown)
          (define-key nu-change-map (kbd "L") 'org-shiftright)
          (define-key nu-change-map (kbd "I") 'org-shiftup)

          ;; COPY
          (define-key nu-copy-map (kbd "O") 'org-copy)
          (define-key nu-copy-map (kbd "S") 'org-copy-special)
          (define-key nu-copy-map (kbd "S") 'org-copy-visible)

	  ;; DELETE
	  (define-key nu-kill-map (kbd "C") 'org-table-delete-column)
          (define-key nu-kill-map (kbd "R") 'org-table-kill-row)
          (define-key nu-kill-map (kbd "S") 'org-cut-special)
          (define-key nu-kill-map (kbd "T") 'org-cut-subtree)

	  ;; GOTO
          (define-key nu-goto-map (kbd "I") 'org-backward-heading-same-level)
          (define-key nu-goto-map (kbd "K") 'org-forward-heading-same-level)
          (define-key nu-goto-map (kbd "J") 'org-backward-element)
          (define-key nu-goto-map (kbd "L") 'org-forward-element)

	  ;; INSERT
          (define-key nu-insert-map (kbd "L") 'org-insert-link)
          (define-key nu-insert-map (kbd "A") 'org-attach)
          (define-key nu-insert-map (kbd "H") 'org-table-insert-hline)
          (define-key nu-insert-map (kbd "C") 'org-table-insert-column)
          (define-key nu-insert-map (kbd "R") 'org-table-insert-row)
          (define-key nu-insert-map (kbd "S") 'org-paste-subtree)
          (define-key nu-insert-map (kbd "P") 'org-paste-special)
          (define-key nu-insert-map (kbd "M") 'org-time-stamp)
          (define-key nu-insert-map (kbd "D") 'org-deadline)
          (define-key nu-insert-map (kbd "T") 'org-insert-todo-heading)

          ;; MARK
          (define-key nu-mark-map ("S") 'outline-mark-subtree)
          (define-key nu-mark-map ("U") 'outline-mark-subtree-up)
          (define-key nu-mark-map ("D") 'outline-mark-subtree-down)

          ;; NEW
          (define-key nu-new-map (kbd "N") 'org-add-note)

	  ;; OPEN
          (define-key nu-open-map (kbd "A") 'org-cycle-agenda-files)
          (define-key nu-open-map (kbd "C") 'org-cycle)
          (define-key nu-open-map (kbd "L") 'org-open-at-point)

          ;; PRINT EVAL
          (define-key nu-print-map (kbd "L") 'pcomplete)
          (define-key nu-print-map (kbd "S") 'org-bacel-check-src-block)

          ;; QUIT ARCHIVE
          (define-key nu-quit-map (kbd "S") 'org-archive-subtree)
          (define-key nu-quit-map (kbd "D") 'org-archive-subtree-default)
          (define-key nu-quit-map (kbd "B") 'org-archive-to-archive-sibling)

	  ;; REPLACE
          (define-key nu-replace-map (kbd "B") 'org-metaleft)
          (define-key nu-replace-map (kbd "F") 'org-metaright)
          (define-key nu-replace-map (kbd "P") 'org-metaup)
          (define-key nu-replace-map (kbd "N") 'org-metadown)
          (define-key nu-replace-map (kbd "T") 'org-table-sort-lines)

	  ;; SAVE
          (define-key nu-save-map (kbd "O") 'org-refile)
          (define-key nu-save-map (kbd "A") 'org-save-all-org-buffers)
	  (define-key nu-save-map (kbd "G") 'org-agenda-file-to-front))))))

;; add hook
(add-hook 'org-mode-hook 'nu-prepare-org-mode)


(provide 'nu-org)
