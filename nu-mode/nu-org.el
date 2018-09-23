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


(defun nu-prepare-org-mode ()

  ;; FIXME NO DEFADVICE
   (defadvice nu-set-bold-f (after nu-set-bold-f-for-org ())
     (if (eq major-mode 'org-mode)
	 (defalias 'nu-bold-function 'org-emphasize)))
   (ad-activate 'nu-set-bold-f)

   (add-hook 'nu-populate-hook '(lambda ()
     (if (eq nu-major-mode 'org-mode)
   	(progn

	  ;; REPLACE
          (define-key nu-replace-map (kbd "C-j") 'org-metaleft)
          (define-key nu-replace-map (kbd "C-l") 'org-metaright)
          (define-key nu-replace-map (kbd "C-u") 'org-metaup)
          (define-key nu-replace-map (kbd "C-o") 'org-metadown)
          (define-key nu-replace-map (kbd "T") 'org-table-sort-lines)
	  
          ;; BOLD
          (define-key nu-change-map (kbd "J") 'org-shiftleft)
          (define-key nu-change-map (kbd "K") 'org-shiftdown)
          (define-key nu-change-map (kbd "L") 'org-shiftright)
          (define-key nu-change-map (kbd "I") 'org-shiftup)

          ;; PRINT
          (define-key nu-print-map (kbd "l") 'pcomplete)

	  ;; DELETE
	  (define-key nu-delete-map (kbd "!") 'org-table-delete-column)
          (define-key nu-delete-map (kbd "r") 'org-table-kill-row)
          (define-key nu-delete-map (kbd "*") 'org-cut-special)
          (define-key nu-delete-map (kbd "M-k") 'org-cut-subtree)

	  ;; INSERT
          (define-key nu-insert-map (kbd "L") 'org-insert-link)
          (define-key nu-insert-map (kbd "H") 'org-table-insert-hline)
          (define-key nu-insert-map (kbd "o") 'org-table-insert-column)
          (define-key nu-insert-map (kbd "O") 'org-table-insert-row)
          (define-key nu-insert-map (kbd "M-s") 'org-paste-subtree)
          (define-key nu-insert-map (kbd "M-o") 'org-paste-special)
          (define-key nu-insert-map (kbd "m") 'org-time-stamp)
          (define-key nu-insert-map (kbd "d") 'org-deadline)
          (define-key nu-insert-map (kbd "t") 'org-insert-todo-heading)

	  ;; SAVE
          (define-key nu-save-map (kbd "o") 'org-refile)
          (define-key nu-save-map (kbd "M-o") 'org-save-all-org-buffers)

	  ;; OPEN
          (define-key nu-open-map (kbd "L") 'org-open-at-point)

	  ;; GOTO
          (define-key nu-goto-map (kbd "I") 'org-backward-heading-same-level)
          (define-key nu-goto-map (kbd "K") 'org-forward-heading-same-level)
          (define-key nu-goto-map (kbd "J") 'org-backward-element)
          (define-key nu-goto-map (kbd "L") 'org-forward-element))))))

;; add hook
(add-hook 'org-mode-hook 'nu-prepare-org-mode)


(provide 'nu-org)
