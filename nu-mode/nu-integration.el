
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
;; most often, the principle is : 
;; when the mode loads, we add a function to nu-populate-hook,
;;                      to add the mode related features to nu menus
;;
;;
;; another override is nu-bold, which is a dwim like func
;;
;; finally hooks might provide user visual help to
;; use several stuff in Emacs
;;
;; Modes with too much code, or outside or emacs,
;; shall have their own files. eg org-mode, ivy
;; here is (alphabetical order) :
;; 
;; Info
;; auto-complete
;; bookmark
;; c mode
;; company
;; dired
;; emacs lisp mode
;; help
;; ibuffer
;; isearch
;; lisp interaction mode
;; mark-hook
;; minibuffer
;; proced
;; term (term-line-mode, term-char-mode)
;; texinfo
;; undo-tree

(require 'nu-vars)

(defvar nu-keymap-backup)

;;
;; Info
;;

(define-key Info-mode-map (kbd "j") 'Info-help)
(define-key Info-mode-map (kbd "h") Info-mode-map)

;;
;; autocomplete
;;

(eval-after-load "auto-complete"
  '(progn
     (define-key ac-completing-map nu-next-line-binding 'ac-next)
     (define-key ac-completing-map nu-previous-line-binding 'ac-previous)))


;;
;; bookmark
;;

  ; the classical one!
(eval-after-load "bookmark"
  '(progn
    (define-key bookmark-bmenu-mode-map "h" bookmark-bmenu-mode-map)))

;;
;; c mode
;;

(defun nu-prepare-c-mode ()
   (defadvice nu-set-bold-f (after nu-set-bold-f-for-emacs-lisp ())
     (if (eq major-mode 'c-mode)
	 (defalias 'nu-bold-function '(lambda () (call-interactively 'comment-dwim)))))
   (ad-activate 'nu-set-bold-f)
   
   (add-hook 'nu-populate-hook '(lambda ()
     (message "testing if c mode")
     (if (eq nu-major-mode 'c-mode)
	(progn
	  (message "prepareing for c mode")
          (define-key nu-replace-map (kbd "y") 'c-set-style)))
          (define-key nu-bold-map (kbd "M-c") 'comment-or-uncomment-region)
          (define-key nu-bold-map (kbd "c") 'comment-dwim)
          (define-key nu-bold-map (kbd "m") 'comment-indent-new-line)
          (define-key nu-bold-map (kbd "l") 'comment-indent))))

(add-hook 'c-mode-hook 'nu-prepare-c-mode)

;;
;; company
;;

(define-key company-active-map "²" company-active-map)


;;
;; dired
;;

(defvar dired-mode-map)

(defun nu-prepare-for-dired ()
  "Most dired adaptation is done using prompts.

Still, some keys here help."
  (define-key dired-mode-map  (kbd "h") dired-mode-map)

  (define-key dired-mode-map nu-previous-line-binding 'dired-previous-line)
  (define-key dired-mode-map nu-forward-char-binding 'dired-find-file)
  (define-key dired-mode-map nu-backward-char-binding 'dired-up-directory)
  (define-key dired-mode-map nu-next-line-binding 'dired-next-line)

  (define-key dired-mode-map  (kbd "C-z") 'dired-undo)
  (define-key dired-mode-map  (kbd "M-s") 'nu-save-prompt)
  (define-key dired-mode-map  (kbd "C-o") 'nu-open-prompt)
  (define-key dired-mode-map  (kbd "C-c") 'nu-copy-prompt)
  (nu-make-overriding-map dired-mode-map nil))

  (add-hook 'dired-mode-hook       'nu-prepare-for-dired)

;;
;; emacs lisp mode
;;

(defun nu-prepare-emacs-lisp-mode ()
   (defadvice nu-set-bold-f (after nu-set-bold-f-for-emacs-lisp ())
     (if (eq major-mode 'emacs-lisp-mode)
	 (defalias 'nu-bold-function '(lambda () (call-interactively 'comment-dwim))))
   (ad-activate 'nu-set-bold-f))
   
   (add-hook 'nu-populate-hook '(lambda ()
     (if (eq nu-major-mode 'emacs-lisp-mode)
	(progn
          (define-key nu-bold-map (kbd "M-c") 'comment-or-uncomment-region)
          (define-key nu-bold-map (kbd "c") 'comment-dwim)
          (define-key nu-bold-map (kbd "m") 'comment-indent-new-line)
          (define-key nu-bold-map (kbd "l") 'comment-indent)
          (define-key nu-print-map (kbd "s") 'eval-last-sexp)
          (define-key nu-print-map (kbd "b") 'eval-buffer)
          (define-key nu-print-map (kbd "a") 'eval-defun)
          (define-key nu-print-map (kbd "r") 'eval-region))))))

(add-hook 'emacs-lisp-mode-hook 'nu-prepare-emacs-lisp-mode)

;;
;; ethan whitespace untabify

(defun nu-populate-for-ethan-whitespace ()
  (unless (not (fboundp 'ethan-wspace-untabify))
    (define-key nu-replace-map (kbd "w") 'ethan-wspace-untabify)))


;;
;; flyspell
;;

(defun nu-populate-for-flyspell ()
  (when (bound-and-true-p flyspell-mode)
      (progn
         (define-key nu-replace-map (kbd "B") 'flyspell-buffer)
         (define-key nu-replace-map (kbd "R") 'flyspell-region) ;; in region keys?
         (define-key nu-replace-map (kbd "C") 'flyspell-correct-word-before-point)
         (define-key nu-replace-map (kbd "A") 'flyspell-auto-correct-word)
         (define-key nu-replace-map (kbd "E") 'flyspell-goto-next-error)
         (define-key nu-replace-map (kbd "P") 'flyspell-auto-correct-previous-word))))

(add-hook 'nu-populate-hook 'nu-populate-for-flyspell ())

;;
;; help
;;

(add-hook 'help-mode-hook '(lambda ()
  (add-hook 'nu-populate-hook '(lambda ()
    (if (eq nu-major-mode 'help-mode)
	(progn
          (define-key nu-goto-map (kbd "L") 'forward-button)
          (define-key nu-goto-map (kbd "J") 'backward-button)
          (define-key nu-goto-map (kbd "o") 'push-button)
          (define-key nu-goto-map (kbd "u") 'help-go-back)))))))

;;
;; ibuffer
;;


(defvar ibuffer-mode-map)

(defun nu-prepare-for-ibuffer ()
  ""
  (define-key ibuffer-mode-map "h" ibuffer-mode-map)

  (define-key ibuffer-mode-map nu-previous-line-binding 'ibuffer-backward-line)
  (define-key ibuffer-mode-map nu-next-line-binding 'ibuffer-forward-line)
  (define-key ibuffer-mode-map nu-forward-char-binding 'ibuffer-visit-buffer)
  (define-key ibuffer-mode-map nu-previous-line-binding 'ibuffer-visit-buffer-other-window-noselect)

  ; cancel bindings then make override.
  (nu-make-overriding-map ibuffer-mode-map
			  '("C-o" "C-y" "M-g" "M-n" "M-p" "M-s")
			  nil))

(add-hook 'ibuffer-hook 'nu-prepare-for-ibuffer)

;;
;; isearch
;;

(defun nu-prepare-for-isearch ()
  "Vanilla search feature."
  (define-key isearch-mode-map (kbd "M-f") 'isearch-repeat-forward)

  (define-key isearch-mode-map nu-next-line-binding 'isearch-repeat-forward)
  (define-key isearch-mode-map nu-previous-line-binding 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "M-p") 'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "M-n") 'isearch-ring-advance)
  (define-key isearch-mode-map (kbd "M-v") 'isearch-yank-kill)
  (define-key isearch-mode-map (kbd "M-a") 'isearch-yank-word-or-char)
  (define-key isearch-mode-map (kbd "M-g") 'isearch-yank-line)
  (define-key isearch-mode-map (kbd "M-h") '(lambda ()
					     (interactive)
					     (describe-keymap isearch-mode-map t)))
  (define-key isearch-mode-map (kbd "M-s") 'isearch-edit-string)
  (define-key isearch-mode-map (kbd "M-d") 'isearch-cancel)

  (lv-message
     (concat "M-f or M-" nu-next-line-key
	     " / M-" nu-previous-line-key
	     ": search forward / backward.\nM-q or M-d cancel search. M-h for more help")))

(defun nu-isearch-exit ()
  ""
  (lv-delete-window))


(add-hook 'isearch-mode-hook     'nu-prepare-for-isearch)
(add-hook 'isearch-mode-end-hook 'nu-isearch-exit)


;; lisp interaction mode
;;

(defun nu-prepare-lisp-interaction-mode ()
   (defadvice nu-set-bold-f (after nu-set-bold-f-for-lisp-interaction ())
     (if (eq major-mode 'lisp-interaction-mode)
	 (defalias 'nu-bold-function '(lambda () (call-interactively 'comment-dwim))))
   (ad-activate 'nu-set-bold-f))

   (add-hook 'nu-populate-hook '(lambda ()
     (if (eq nu-major-mode 'lisp-interaction-mode)
	  (progn
            (define-key nu-bold-map (kbd "M-c") 'comment-or-uncomment-region)
            (define-key nu-bold-map (kbd "c") 'comment-dwim)
            (define-key nu-bold-map (kbd "m") 'comment-indent-new-line)
            (define-key nu-bold-map (kbd "l") 'comment-indent)
            (define-key nu-print-map (kbd "s") 'eval-last-sexp)
            (define-key nu-print-map (kbd "b") 'eval-buffer)
            (define-key nu-print-map (kbd "a") 'eval-defun)
            (define-key nu-print-map (kbd "r") 'eval-region))))))

(add-hook 'lisp-interaction-mode-hook 'nu-prepare-lisp-interaction-mode)



;;
;; minibuffer
;;

(defun nu-prepare-for-minibuffer ()
  "Minibuffer.

Minibuffer should use same keys are fundamental mode."

  ; kill nu keymap
  (setcdr (assoc 'nu-mode minor-mode-map-alist) nil)

  (define-key minibuffer-local-map (kbd "M-<dead-circumflex>") 'previous-history-element)
  (define-key minibuffer-local-map (kbd "M-$") 'next-history-element)

  (nu-make-overriding-map minibuffer-local-map nil "M-q" 'abort-recursive-edit))




(defun nu-minibuffer-exit ()
  "restore nu"
  (lv-delete-window)
  (setcdr (assoc 'nu-mode minor-mode-map-alist) nu-keymap))


(add-hook 'minibuffer-setup-hook 'nu-prepare-for-minibuffer t)
(add-hook 'minibuffer-exit-hook  'nu-minibuffer-exit t)

;;
;; mark
;;

(defun nu-add-mark-hook ()
  "Give the user some input about visual mode."

  ; populate the maps
  (nu-populate-visual-map)
  (nu-populate-replace)
  (define-key nu-keymap (kbd "M-a") nu-visual-map)

  ; message (todo customize this so can be turned off)
  (lv-message (concat
	       (propertize "M-a" 'face 'nu-face-shortcut)
	       " actions / "
	       (propertize "M-q" 'face 'nu-face-shortcut)
               " quit.")))

(defun nu-deactivate-mark-hook ()
  ""
  (lv-delete-window)
  (define-key nu-keymap (kbd "M-a") 'nu-set-mark))

;;
;; proced
;;

(add-hook 'proced-mode-hook '(lambda ()
				(define-key proced-mode-map "h" proced-mode-map)))


;;
;; term
;;

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

(add-hook 'term-mode-hook        'nu-prepare-for-term)

;;
;; texinfo
;;

(add-hook 'texinfo-mode-hook '(lambda ()
   (add-hook 'nu-populate-hook '(lambda ()
     (if (eq nu-major-mode 'texinfo-mode)
         (progn
           (define-key nu-print-map (kbd "i") 'makeinfo-buffer)
           (define-key nu-print-map (kbd "a") 'nu-texi2pdf)
	   (define-key nu-insert-map (kbd "M-u") 'texinfo-insert-@url)
	   (define-key nu-insert-map (kbd "M-k") 'texinfo-insert-@kbd)))))))

;;
;; undo-tree
;;

(eval-after-load "undo-tree"
  '(progn
     (define-key undo-tree-visualizer-mode-map nu-back-to-indentation-key undo-tree-visualizer-mode-map)

     (define-key undo-tree-visualizer-mode-map nu-previous-line-key 'undo-tree-visualize-undo)
     (define-key undo-tree-visualizer-mode-map nu-next-line-key 'undo-tree-visualize-redo)
     (define-key undo-tree-visualizer-mode-map nu-backward-char-key 'undo-tree-visualize-switch-branch-left)
     (define-key undo-tree-visualizer-mode-map nu-forward-char-key 'undo-tree-visualize-switch-branch-right)

     (define-key undo-tree-visualizer-mode-map (kbd "M-q")   'undo-tree-visualizer-abort)
     (define-key undo-tree-map (kbd "C-x") nil)))


(provide 'nu-integration)
