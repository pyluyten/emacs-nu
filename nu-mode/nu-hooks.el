
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
;; hooks and eval-after-load stuff.
;;
;;

(defvar ibuffer-mode-map)
(defvar dired-mode-map)
(defvar nu-keymap-backup)


(defun nu-prepare-for-isearch ()
  "I still need to replace this isearch turd."
  (define-key isearch-mode-map (kbd "M-k") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "M-i") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "M-v") 'isearch-yank-kill)
  (define-key isearch-mode-map (kbd "M-a") 'isearch-yank-word-or-char)
  (define-key isearch-mode-map (kbd "M-g") 'isearch-yank-line)
  (define-key isearch-mode-map (kbd "M-s") 'isearch-edit-string)
  (define-key isearch-mode-map (kbd "M-d") 'isearch-cancel))

(defun nu-prepare-for-ibuffer ()
  "Should mainly be used for organization / multi buffers actions
since helm-buffers-list allows quick stuff."
  (define-key ibuffer-mode-map (kbd "M-i") 'ibuffer-backward-line)
  (define-key ibuffer-mode-map (kbd "M-k") 'ibuffer-forward-line)
  (define-key ibuffer-mode-map (kbd "M-l") 'ibuffer-visit-buffer)
  (define-key ibuffer-mode-map (kbd "M-j") 'ibuffer-visit-buffer-other-window-noselect)

  ; cancel bindings then make override.
  (nu-make-overriding-map ibuffer-mode-map
			  '("C-o" "C-y" "M-g" "M-n" "M-p" "M-s")
			  nil))

(defun nu-prepare-for-minibuffer ()
  "Minibuffer.

Minibuffer should use same keys are fundamental mode.
Helm has few shortcuts because it has its maps
(see eval after load helm mode)"

  ; kill nu keymap
  (setcdr (assoc 'nu-mode minor-mode-map-alist) nil)

  ; if helm
  (if (boundp 'helm-alive-p)

      (if (helm-alive-p)
	  (progn
           (define-key minibuffer-local-map (kbd "M-i") 'helm-previous-line)
           (define-key minibuffer-local-map (kbd "M-k") 'helm-next-line)
           (define-key minibuffer-local-map (kbd "M-<dead-circumflex>") 'previous-history-element)
           (define-key minibuffer-local-map (kbd "M-$") 'next-history-element))

    (progn
      (define-key minibuffer-local-map (kbd "M-i") 'previous-history-element)
      (define-key minibuffer-local-map (kbd "M-k") 'next-history-element))))

  ; if ivy
  (if (boundp 'ivy-minibuffer-map)
      (progn
	;; standard ivy map
	
        (define-key ivy-minibuffer-map (kbd "M-v") 'ivy-yank-word)
	(define-key ivy-minibuffer-map (kbd "M-i") 'ivy-previous-line)
	(define-key ivy-minibuffer-map (kbd "M-k") 'ivy-next-line)
        (define-key ivy-minibuffer-map (kbd "M-s") 'ivy-dispatching done)
	(define-key ivy-minibuffer-map (kbd "M-m") 'ivy-immediate-done)
	(define-key ivy-minibuffer-map (kbd "M-g") 'ivy-partial-or-done)
	(define-key ivy-minibuffer-map (kbd "M-f") 'ivy-avy)
        (define-key ivy-minibuffer-map (kbd "M-p") 'ivy-insert-current)
	(define-key ivy-minibuffer-map (kbd "M-d") 'hydra-ivy/body)))

  (nu-make-overriding-map minibuffer-local-map nil "M-q" 'abort-recursive-edit))


(defun nu-prepare-for-term-raw ()
  "Respect term raw map principle to be an emulator,

thus we only trick C-c."
  (nu-drop-overriding-map term-mode-map)
  (define-key term-raw-map (kbd "C-c") 'nu-prompt-for-term)
  (nu-make-overriding-map term-raw-map nil))



(defun nu-prepare-for-term-line ()
  "Adapt term line mode map to nu-style."
  (nu-drop-overriding-map term-raw-map)
  (define-key term-mode-map (kbd "C-c") 'nu-tmp-prompt-for-term-line-c-c)
  (nu-make-overriding-map term-mode-map nil))



(defadvice term-line-mode (after nu-prepare-for-term-line-advice ())
  (nu-prepare-for-term-line))

(ad-activate 'term-line-mode)


(defadvice term-char-mode (after nu-prepare-for-term-char-advice ())
  (nu-prepare-for-term-raw))

(ad-activate 'term-char-mode)



(defun nu-prepare-for-term ()
  "Review terminal.

Always start at char mode."
  (nu-prepare-for-term-raw))


(defun nu-prepare-for-dired ()
  "Most dired adaptation is done using prompts.

Still, some keys here help."
  (define-key dired-mode-map  (kbd "M-i") 'dired-previous-line)
  (define-key dired-mode-map  (kbd "M-l") 'dired-find-file)
  (define-key dired-mode-map  (kbd "M-j") 'dired-up-directory)
  (define-key dired-mode-map  (kbd "M-k") 'dired-next-line)

  (define-key dired-mode-map  (kbd "C-z") 'dired-undo)
  (define-key dired-mode-map  (kbd "M-s") 'nu-save-prompt)
  (define-key dired-mode-map  (kbd "C-o") 'nu-open-prompt)
  (define-key dired-mode-map  (kbd "C-c") 'nu-copy-prompt)
  (nu-make-overriding-map dired-mode-map nil))


(defun nu-minibuffer-exit ()
  "restore nu"
  (setcdr (assoc 'nu-mode minor-mode-map-alist) nu-keymap))


(add-hook 'term-mode-hook        'nu-prepare-for-term)
(add-hook 'minibuffer-setup-hook 'nu-prepare-for-minibuffer t)
(add-hook 'minibuffer-exit-hook 'nu-minibuffer-exit t)
(add-hook 'ibuffer-hook          'nu-prepare-for-ibuffer)
(add-hook 'isearch-mode-hook     'nu-prepare-for-isearch)
(add-hook 'dired-mode-hook       'nu-prepare-for-dired)


(eval-after-load "undo-tree"
  '(progn
     (define-key undo-tree-visualizer-mode-map (kbd "i") 'undo-tree-visualize-undo)
     (define-key undo-tree-visualizer-mode-map (kbd "k") 'undo-tree-visualize-redo)
     (define-key undo-tree-visualizer-mode-map (kbd "j") 'undo-tree-visualize-switch-branch-left)
     (define-key undo-tree-visualizer-mode-map (kbd "l") 'undo-tree-visualize-switch-branch-right)

     (define-key undo-tree-visualizer-mode-map (kbd "M-q")   'undo-tree-visualizer-abort)))


(eval-after-load "auto-complete"
  '(progn
     (define-key ac-completing-map (kbd "M-k") 'ac-next)
     (define-key ac-completing-map (kbd "M-i") 'ac-previous)))


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

(provide 'nu-hooks)
