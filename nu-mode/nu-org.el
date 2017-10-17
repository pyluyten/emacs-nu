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

;; 
;; nu-mode / org-mode integration
;;

;; http://orgmode.org/tmp/worg/org-configs/org-hooks.html


(defun nu-prepare-org-mode ()
  ;; nu bold needs to check advice

   (defadvice nu-set-bold-f (after nu-set-bold-f-for-org ())
     (if (eq major-mode 'org-mode)
	 (defalias 'nu-bold-function 'org-emphasize)))
   (ad-activate 'nu-set-bold-f)


  ;; advice menus to make them useful.

   (defadvice nu-populate-replace (after nu-populate-replace-for-org ())
     (if (eq major-mode 'org-mode)
	 (progn
          (define-key nu-replace-map (kbd "J") 'org-shiftleft)
          (define-key nu-replace-map (kbd "K") 'org-shiftdown)
          (define-key nu-replace-map (kbd "L") 'org-shiftright)
          (define-key nu-replace-map (kbd "I") 'org-shiftup)
          (define-key nu-replace-map (kbd "C-j") 'org-metaleft)
          (define-key nu-replace-map (kbd "C-l") 'org-metaright)
          (define-key nu-replace-map (kbd "C-u") 'org-metaup)
          (define-key nu-replace-map (kbd "C-o") 'org-metadown))))
   (ad-activate 'nu-populate-replace)

  (defadvice nu-populate-print (after nu-populate-print-for-org ())
     (if (eq major-mode 'org-mode)
	 (progn
          (define-key nu-print-map (kbd "l") 'pcomplete))))
   (ad-activate 'nu-populate-print)

  (defadvice nu-populate-delete (after nu-populate-delete-for-org ())
     (if (eq major-mode 'org-mode)
	 (progn
	   (define-key nu-delete-map (kbd "!") 'org-table-delete-column)
           (define-key nu-delete-map (kbd "r") 'org-table-kill-row)
           (define-key nu-delete-map (kbd "*") 'org-cut-special)
           (define-key nu-delete-map (kbd "M-k") 'org-cut-subtree))))
   (ad-activate 'nu-populate-delete)

  (defadvice nu-populate-insert-map (after nu-populate-insert-for-org ())
        (if (eq major-mode 'org-mode)
          (progn
            (define-key nu-insert-map (kbd "L") 'org-insert-link)
            (define-key nu-insert-map (kbd "o") 'org-table-insert-column)
            (define-key nu-insert-map (kbd "O") 'org-table-insert-row)
            (define-key nu-insert-map (kbd "M-s") 'org-paste-subtree)
            (define-key nu-insert-map (kbd "M-o") 'org-paste-special)
            (define-key nu-insert-map (kbd "m") 'org-time-stamp)
            (define-key nu-insert-map (kbd "d") 'org-deadline)
            (define-key nu-insert-map (kbd "t") 'org-insert-todo-heading))))
   (ad-activate 'nu-populate-insert-map)

  (defadvice nu-populate-save-map (after nu-populate-save-for-org ())
    (if (eq major-mode 'org-mode)
      (progn
        (define-key nu-save-map (kbd "o") 'org-refile)
        (define-key nu-save-map (kbd "M-o") 'org-save-all-org-buffers))))  
   (ad-activate 'nu-populate-save-map)

  (defadvice nu-populate-open-map (after nu-populate-open-for-org ())
       (if (eq major-mode 'org-mode)
    (define-key nu-open-map (kbd "L") 'org-open-at-point)))
   (ad-activate 'nu-populate-open-map)

  (defadvice nu-populate-goto-map (after nu-populate-goto-for-org ())
       (if (eq major-mode 'org-mode)
       (progn
         (define-key nu-goto-map (kbd "I") 'org-backward-heading-same-level)
         (define-key nu-goto-map (kbd "K") 'org-forward-heading-same-level)
         (define-key nu-goto-map (kbd "J") 'org-backward-element)
         (define-key nu-goto-map (kbd "L") 'org-forward-element))))
   (ad-activate 'nu-populate-goto-map))

;; add hook
(add-hook 'org-mode-hook 'nu-prepare-org-mode)


(provide 'nu-org)
