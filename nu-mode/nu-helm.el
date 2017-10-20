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

;; nu-setup

(defun nu-setup-helm ()
  ;; completion prompt setup.
  (defalias 'nu-completion-prompt-for-keymap 'nu-helm-prompt-for-keymap)

  ;; usual commands.  
  (defalias 'nu-search 'nu-isearch-forward-regexp)
  (defalias 'nu-M-x 'helm-M-x)
  (defalias 'nu-find-files 'helm-find-files)
  (defalias 'nu-buffers-list 'helm-buffers-list)
  (defalias 'nu-describe-function 'describe-function)
  (defalias 'nu-describe-variable 'describe-variable)
  (defalias 'nu-bookmarks 'helm-bookmarks)
  (defalias 'nu-recentf 'helm-recentf))


;; prompters

(defun nu-helm-prompt-for-keymap (keymap)
  "Use helm mode to prompt for a keymap."
 (interactive)
 (setq nu-current-keymap keymap
       nu-keymap-list nil
       nu-describe-bind-mode "completion")
   (map-keymap 'nu-insert-binding-row keymap)
   (setq nu-last-command
      (intern-soft
          (replace-regexp-in-string "\\(\\w\\) .*" "\\1"
             (helm-comp-read "Execute :" nu-keymap-list
                             :must-match t))))
   (ignore-errors (call-interactively nu-last-command))
   (setq nu-repeat-prompt nil))


;; minibuffer hook
;; rather quick because helm has itw own map.

(defun nu-set-minibuffer-helm ()
  ""
   (if (boundp 'helm-alive-p)
     (if helm-alive-p
       (progn
         (define-key minibuffer-local-map (kbd "M-i") 'helm-previous-line)
         (define-key minibuffer-local-map (kbd "M-k") 'helm-next-line)
         (define-key minibuffer-local-map (kbd "M-<dead-circumflex>") 'previous-history-element)
         (define-key minibuffer-local-map (kbd "M-$") 'next-history-element)))))

(defadvice nu-prepare-for-minibuffer (after nu-prepare-for-minibuffer-helm-advice ())
  (nu-set-minibuffer-helm))

(ad-activate 'nu-prepare-for-minibuffer)

;; helm map

(eval-after-load "helm-mode" ;; TODO = helm-M-x-map
  '(progn
    ;; qwertyuiop
    (define-key helm-map (kbd "C-q") 'helm-keyboard-quit)
    (define-key helm-find-files-map (kbd "C-r") 'helm-ff-run-rename-file) ; dired is better as this, no?
    (define-key helm-buffer-map (kbd "C-r") 'helm-buffer-run-query-replace) ; absurd func! well...

    (define-key helm-find-files-map (kbd "C-u") 'helm-find-files-up-one-level)
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; does expand, except Mx.
    (define-key helm-map (kbd "M-i") 'helm-previous-line)
    (define-key helm-find-files-map (kbd "M-i") 'helm-previous-line)
    (define-key helm-generic-files-map (kbd "M-i") 'helm-previous-line)
    (define-key helm-find-files-map (kbd "C-o") 'helm-execute-persistent-action)
    (define-key helm-generic-files-map (kbd "C-o") 'helm-execute-persistent-action)
    (define-key helm-find-files-map (kbd "C-p") 'helm-ff-run-switch-to-eshell)
    (define-key helm-map (kbd "M-p") 'universal-argument)

    ;asdfghjkl. Use Alt-g for goto.
    (define-key helm-map (kbd "C-a") 'helm-mark-all) ; for once a mark all makes sense...
    (define-key helm-map (kbd "M-a") 'helm-toggle-visible-mark)
    (define-key helm-find-files-map (kbd "M-a") 'helm-toggle-visible-mark)
    (define-key helm-buffer-map (kbd "M-a") 'helm-toggle-visible-mark)
    (define-key helm-find-files-map (kbd "M-d") 'helm-ff-run-delete-file) ; ok
    (define-key helm-buffer-map (kbd "M-d") 'helm-buffer-run-kill-buffers) ; ok but no confirm???!

    (define-key helm-map (kbd "S-<backspace>") 'helm-previous-source)
    (define-key helm-buffer-map (kbd "S-<backspace>") 'helm-previous-source)
    (define-key helm-map (kbd "S-<SPC>") 'helm-next-source)
    (define-key helm-buffer-map (kbd "S-<SPC>") 'helm-next-source)

    (define-key helm-map (kbd "M-<dead-circumflex>") 'previous-history-element) ; not most frequent...
    (define-key helm-map (kbd "M-$") 'next-history-element) ; not most frequent...
    (define-key helm-map (kbd "M-k") 'helm-next-line) ; ok

    ; zxcvbn
    (define-key helm-map (kbd "C-x") 'helm-delete-minibuffer-contents) ; stick to this.
    (define-key helm-map (kbd "C-c") 'nu-copy-region-or-line) ; stick to this
    (define-key helm-find-files-map (kbd "M-c") 'helm-ff-run-copy-file) ; lame
    (define-key helm-map (kbd "C-v") 'helm-yank-text-at-point) ; stick to this.

    ; non char
    (define-key helm-map (kbd "M-<SPC>") 'helm-next-page)
    (define-key helm-map (kbd "M-<backspace>") 'helm-previous-page)
    (define-key helm-find-files-map (kbd "M-=") 'helm-ff-properties-persistent) ; lame

    (define-key helm-map (kbd "M-<RET>") 'helm-select-action)
    (define-key helm-buffer-map (kbd "M-<RET>") 'helm-select-action)
    (define-key helm-find-files-map (kbd "M-<RET>") 'helm-select-action)))

 ;helm-copy-to-buffer?
 ;helm-yank-selection

(provide 'nu-helm)
