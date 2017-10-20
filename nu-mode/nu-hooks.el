
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


(defvar dired-mode-map)
(defvar nu-keymap-backup)

(defun nu-prepare-lisp-interaction-mode ()
   (defadvice nu-set-bold-f (after nu-set-bold-f-for-lisp-interaction ())
     (if (eq major-mode 'lisp-interaction-mode)
	 (defalias 'nu-bold-function '(lambda () (call-interactively 'comment-dwim))))
   (ad-activate 'nu-set-bold-f)))

(add-hook 'lisp-interaction-mode-hook 'nu-prepare-lisp-interaction-mode)


(defun nu-prepare-emacs-lisp-mode ()
   (defadvice nu-set-bold-f (after nu-set-bold-f-for-emacs-lisp ())
     (if (eq major-mode 'emacs-lisp-mode)
	 (defalias 'nu-bold-function '(lambda () (call-interactively 'comment-dwim))))
   (ad-activate 'nu-set-bold-f)))

(add-hook 'emacs-lisp-mode-hook 'nu-prepare-emacs-lisp-mode)


(defun nu-prepare-for-minibuffer ()
  "Minibuffer.

Minibuffer should use same keys are fundamental mode."

  ; kill nu keymap
  (setcdr (assoc 'nu-mode minor-mode-map-alist) nil)

  (define-key minibuffer-local-map (kbd "M-<dead-circumflex>") 'previous-history-element)
  (define-key minibuffer-local-map (kbd "M-$") 'next-history-element)

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
  (define-key dired-mode-map  (kbd "h") dired-mode-map)
 
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
  (lv-delete-window)
  (setcdr (assoc 'nu-mode minor-mode-map-alist) nu-keymap))


(defun nu-add-mark-hook ()
  "Give the user some input about visual mode."
  (lv-message (concat
	       (propertize "M-a" 'face 'nu-face-shortcut)
	       " : selection keys / "
	       (propertize "M-q" 'face 'nu-face-shortcut)
               " : quit visual mode")))



(defun nu-magit-w-editor-mode-hook ()
  "Assign keys for with-editor-finish / cancel"
  (define-key global-map (kbd "C-c C-c") 'with-editor-finish)
  (define-key global-map (kbd "C-c C-k") 'with-editor-cancel))

(add-hook 'with-editor-mode-hook 'nu-magit-w-editor-mode-hook)
(add-hook 'term-mode-hook        'nu-prepare-for-term)
(add-hook 'minibuffer-setup-hook 'nu-prepare-for-minibuffer t)
(add-hook 'minibuffer-exit-hook 'nu-minibuffer-exit t)
(add-hook 'dired-mode-hook       'nu-prepare-for-dired)
(add-hook 'activate-mark-hook    'nu-add-mark-hook)
(add-hook 'deactivate-mark-hook  'lv-delete-window)

(eval-after-load "undo-tree"
  '(progn
     (define-key undo-tree-visualizer-mode-map (kbd "i") 'undo-tree-visualize-undo)
     (define-key undo-tree-visualizer-mode-map (kbd "k") 'undo-tree-visualize-redo)
     (define-key undo-tree-visualizer-mode-map (kbd "j") 'undo-tree-visualize-switch-branch-left)
     (define-key undo-tree-visualizer-mode-map (kbd "l") 'undo-tree-visualize-switch-branch-right)

     (define-key undo-tree-visualizer-mode-map (kbd "M-q")   'undo-tree-visualizer-abort)
     (define-key undo-tree-map (kbd "C-x") nil)))


(eval-after-load "auto-complete"
  '(progn
     (define-key ac-completing-map (kbd "M-k") 'ac-next)
     (define-key ac-completing-map (kbd "M-i") 'ac-previous)))



(provide 'nu-hooks)
