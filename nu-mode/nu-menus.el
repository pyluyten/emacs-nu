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
;; prompts : first define keys from "common case"
;; that might be shallowed by other modes
;;

(require 'nu-vars)
(require 'windmove)
(require 'nu-prompters)
(require 'hydra)

 (autoload 'zap-up-to-char "misc"
"Kill up to, but not including ARGth occurrence of CHAR." t)



(defun nu-populate-window ()
   (nu-define-prefix 'nu-window-map)
   (define-key nu-window-map (kbd "t") 'kill-buffer)
   (define-key nu-window-map (kbd "b") 'view-buffer-other-window)
   (define-key nu-window-map (kbd "d") 'scroll-other-window-down)
   (define-key nu-window-map (kbd "u") 'scroll-left)
   (define-key nu-window-map (kbd "o") 'scroll-right)
   (define-key nu-window-map (kbd "e") 'delete-window)
   (define-key nu-window-map (kbd "r") 'recenter-top-bottom)
   (define-key nu-window-map (kbd "Q") 'save-buffers-kill-emacs)

   (define-key nu-window-map (kbd "f") 'transpose-frame)
   (define-key nu-window-map (kbd "i") 'enlarge-window)
   (define-key nu-window-map (kbd "j") 'shrink-window-horizontally)
   (define-key nu-window-map (kbd "k") 'shrink-window)
   (define-key nu-window-map (kbd "l") 'enlarge-window-horizontally)
   (define-key nu-window-map (kbd "n")   'nu-next-window)
   (define-key nu-window-map (kbd "s") 'scroll-other-window)
   (define-key nu-window-map (kbd "p")   'nu-previous-window)
   (define-key nu-window-map (kbd "w") 'delete-other-windows)
   (define-key nu-window-map (kbd "x") 'nu-close-document))

(defun nu-window-prompt ()
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-window)
  (nu-prompt-for-keymap nu-window-map))


(defun nu-populate-print ()
  (nu-define-prefix 'nu-print-map)

  (cond
   ((or (eq nu-major-mode 'emacs-lisp-mode)
	(eq nu-major-mode 'lisp-interaction-mode))
    (define-key nu-print-map (kbd "s") 'eval-last-sexp)
    (define-key nu-print-map (kbd "b") 'eval-buffer)
    (define-key nu-print-map (kbd "a") 'eval-defun)
    (define-key nu-print-map (kbd "r") 'eval-region))
   ((eq nu-major-mode 'magit-status-mode)
    (define-key nu-print-map (kbd "p") 'magit-shell-command)
    (define-key nu-print-map (kbd ":") 'magit-git-command))
   ((eq nu-major-mode 'dired-mode)
    (define-key nu-print-map (kbd "s") 'dired-do-print)
    (define-key nu-print-map (kbd "b") 'dired-do-byte-compile)
    (define-key nu-print-map (kbd "p")   'dired-do-async-shell-command)
    (define-key nu-print-map (kbd "P")   'dired-do-shell-command)
    (define-key nu-print-map (kbd "d")   'dired-diff))
   ((eq nu-major-mode 'texinfo-mode)
    (define-key nu-print-map (kbd "i") 'makeinfo-buffer)
    (define-key nu-print-map (kbd "a") 'nu-texi2pdf))
   ((eq nu-major-mode 'ibuffer-mode)
    (define-key nu-print-map (kbd "s") 'ibuffer-diff-with-file)
    (define-key nu-print-map (kbd "P") 'ibuffer-do-shell-command-pipe)
    (define-key nu-print-map (kbd "a") 'ibuffer-do-shell-command-file)
    (define-key nu-print-map (kbd "r") 'ibuffer-do-print)))

  ; common case
  (define-key nu-print-map (kbd "e") 'print-buffer)
  (define-key nu-print-map (kbd "p") 'async-shell-command)
  (define-key nu-print-map (kbd "d") 'ediff)
  (define-key nu-print-map (kbd "c") 'subword-mode)
  (define-key nu-print-map (kbd "f") 'find-grep)
  (define-key nu-print-map (kbd "g") 'grep)
  (define-key nu-print-map (kbd "k") 'kmacro-end-or-call-macro)
  (define-key nu-print-map (kbd "m") 'compile)
  (define-key nu-print-map (kbd "n") 'negative-argument)
  (define-key nu-print-map (kbd "u") 'universal-argument)
  (define-key nu-print-map (kbd "w") 'pwd))


(defun nu-print-prompt ()
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-print)
  (nu-prompt-for-keymap nu-print-map))


(defun nu-populate-quit ()
  "Populate quit map."
 (nu-define-prefix 'nu-quit-map)
 (define-key nu-quit-map (kbd "C-m") 'save-buffers-kill-emacs)
 (define-key nu-quit-map (kbd "e") 'kill-emacs)
 (define-key nu-quit-map (kbd "s") 'suspend-emacs)
 (define-key nu-quit-map (kbd "t") 'suspend-frame)
 (define-key nu-quit-map (kbd "f") 'delete-frame)
 (define-key nu-quit-map (kbd "q") 'keyboard-escape-quit)
 (define-key nu-quit-map (kbd "w") 'quit-window))


(defun nu-quit-prompt ()
  "Prompt to quit."
  (interactive)
  (setq nu-major-mode major-mode)
  (if mark-active
      (cua-set-mark)
      (progn
        (nu-populate-quit)
        (nu-prompt-for-keymap nu-quit-map))))

(defun nu-populate-delete ()
  "Populate nu-delete-map."
  (nu-define-prefix 'nu-delete-map)

  (cond
   ((eq nu-major-mode 'magit-status-mode)
    (define-key nu-delete-map (kbd "b") 'magit-delete-branch)
    (define-key nu-delete-map (kbd "h") 'magit-discard-item))
   ((eq nu-major-mode 'ibuffer-mode)
    (define-key nu-delete-map (kbd "M-d") 'ibuffer-do-delete)
    (define-key nu-delete-map (kbd "M-k") 'ibuffer-do-kill-lines)
    (define-key nu-delete-map (kbd "c") 'ibuffer-copy-filename-as-kill)
    (define-key nu-delete-map (kbd "d") 'ibuffer-do-delete)
    (define-key nu-delete-map (kbd "k") 'ibuffer-kill-line)
    (define-key nu-delete-map (kbd "m") 'ibuffer-mark-for-delete))
   ((eq nu-major-mode 'dired-mode)
    (define-key nu-delete-map (kbd "d") 'dired-flag-file-deletion)
    (define-key nu-delete-map (kbd "k") 'dired-do-flagged-delete)
    (define-key nu-delete-map (kbd "o") 'dired-do-delete))
   (t
    (define-key nu-delete-map (kbd "h") 'delete-horizontal-space)
    (define-key nu-delete-map (kbd "x") 'fixup-whitespace)
    (define-key nu-delete-map (kbd "u") 'flush-lines)
    (define-key nu-delete-map (kbd "i") 'kill-line)
    (define-key nu-delete-map (kbd "a") 'nu-delete-all)
    (define-key nu-delete-map (kbd "b") 'delete-blank-lines)
    (define-key nu-delete-map (kbd "k") 'kill-buffer)
    (define-key nu-delete-map (kbd "d") 'kill-whole-line)
    (define-key nu-delete-map (kbd "e") 'kill-sentence)
    (define-key nu-delete-map (kbd "f") 'nu-delete-defun)
    (define-key nu-delete-map (kbd "h") 'delete-horizontal-space)
    (define-key nu-delete-map (kbd "s") 'kill-sexp)
    (define-key nu-delete-map (kbd "o") 'kill-word)
    (define-key nu-delete-map (kbd "t") 'delete-trailing-whitespace)
    (define-key nu-delete-map (kbd "z")  'zap-up-to-char)))

  ; common cases
  (define-key nu-delete-map (kbd "F") 'delete-file)
  (define-key nu-delete-map (kbd "j") 'delete-other-windows)
  (define-key nu-delete-map (kbd "l") 'delete-window)

  (if mark-active
    (define-key nu-delete-map (kbd "<RET>") 'kill-region)))

(defun nu-delete-prompt ()
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-delete)
  (nu-prompt-for-keymap nu-delete-map))


(defun nu-populate-bold-map ()
 "Populate bold map"
  (nu-define-prefix 'nu-bold-map)
  (define-key nu-bold-map (kbd "a") 'align)
  (define-key nu-bold-map (kbd "f") 'fill-paragraph)
  (define-key nu-bold-map (kbd "n") 'set-fill-column)
  (define-key nu-bold-map (kbd "r") 'set-fill-prefix)
  (define-key nu-bold-map (kbd "i") 'indent)
  (define-key nu-bold-map (kbd "e") 'facemenu-face-menu)
  (define-key nu-bold-map (kbd "s") 'indent-sexp)
  (define-key nu-bold-map (kbd "o") 'indent-for-comment)
  (define-key nu-bold-map (kbd "p") 'capitalize-word)
  (define-key nu-bold-map (kbd "d") 'downcase-word)
  (define-key nu-bold-map (kbd "u") 'upcase-word)

  (if (or (eq nu-major-mode 'c-mode)
          (eq nu-major-mode 'lisp-interaction-mode)
          (eq nu-major-mode 'emacs-lisp-mode))
      (progn
          (define-key nu-bold-map (kbd "M-c") 'comment-or-uncomment-region)
          (define-key nu-bold-map (kbd "c") 'comment-dwim)
          (define-key nu-bold-map (kbd "m") 'comment-indent-new-line)
          (define-key nu-bold-map (kbd "l") 'comment-indent))))

(defun nu-bold-prompt ()
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-bold-map)
  (nu-prompt-for-keymap nu-bold-map))


(defun nu-populate-insert-map ()
 "Populate insert map	.	"
  (nu-define-prefix 'nu-insert-map)

  (if (eq nu-major-mode 'dired-mode)
        (progn
           (define-key nu-insert-map (kbd "v") 'dired-maybe-insert-subdir)
           (define-key nu-insert-map (kbd "M-v") 'dired-create-directory))
         ; else
        (define-key nu-insert-map (kbd "M-v") 'expand-abbrev)
        (define-key nu-insert-map (kbd "V") 'nu-yank-pop-or-yank)
        (define-key nu-insert-map (kbd "b") 'insert-buffer)
        (define-key nu-insert-map (kbd "c") 'quoted-insert)
        (define-key nu-insert-map (kbd "e") 'insert-register)
        (define-key nu-insert-map (kbd "f") 'insert-file)
        (define-key nu-insert-map (kbd "i") 'nu-insert-line-above)
        (define-key nu-insert-map (kbd "k") 'nu-insert-line-below)
        (define-key nu-insert-map (kbd "r") 'yank-rectangle)
        (define-key nu-insert-map (kbd "M-r") 'open-rectangle)
        (define-key nu-insert-map (kbd "v") 'yank)

        (define-key nu-insert-map (kbd "x") 'expand-abbrev)
        (define-key nu-insert-map (kbd "v") 'yank)

        ; addon
        (if (eq nu-major-mode 'texinfo-mode)
          (progn
            (define-key nu-insert-map (kbd "M-u") 'texinfo-insert-@url)
            (define-key nu-insert-map (kbd "M-k") 'texinfo-insert-@kbd))))

  ; anycase
  (define-key nu-insert-map (kbd "S") 'shell-command)
  (define-key nu-insert-map (kbd "g") 'define-global-abbrev)
  (define-key nu-insert-map (kbd "h")  'nu-browse-kill-ring)
  (define-key nu-insert-map (kbd "s") 'async-shell-command))


(defun nu-insert-prompt ()
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-insert-map)
  (nu-prompt-for-keymap nu-insert-map))


(defun nu-populate-save-map ()
 "Populate save map	.	"
  (nu-define-prefix 'nu-save-map)
  (define-key nu-save-map (kbd "L") 'org-store-link)
  (define-key nu-save-map (kbd "e") 'frame-configuration-to-register)
  (define-key nu-save-map (kbd "h") 'write-file)
  (define-key nu-save-map (kbd "i") 'recover-session)
  (define-key nu-save-map (kbd "b") 'bookmark-set)
  (define-key nu-save-map (kbd "g") 'nu-toggle-goal-column)
  (define-key nu-save-map (kbd "r") 'rename-buffer)
  (define-key nu-save-map (kbd "s") 'save-buffer)
  (define-key nu-save-map (kbd "t") 'point-to-register)
  (define-key nu-save-map (kbd "a") 'revert-buffer)
  (define-key nu-save-map (kbd "w") 'window-configuration-to-register)
  (define-key nu-save-map (kbd "k") 'kmacro-start-macro-or-insert-counter)
  (define-key nu-save-map (kbd "f") 'nu-create-tags)

  (define-key nu-save-map (kbd "d") 'add-global-abbrev)
  (define-key nu-save-map (kbd "j") 'add-mode-abbrev)

  (if (eq nu-major-mode 'magit-status-mode)
      (progn
        (define-key nu-save-map (kbd "d") 'magit-diff)
        (define-key nu-save-map (kbd "c") 'magit-commit)
        (define-key nu-save-map (kbd "p") 'nu-git-push)
        (define-key nu-save-map (kbd "t") 'magit-push-tags)
        (define-key nu-save-map (kbd "x") 'git-commit-commit))
      ; else
      (define-key nu-save-map (kbd "m") 'magit-status)))


(defun nu-save-prompt ()
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-save-map)
  (nu-prompt-for-keymap nu-save-map))


(defun nu-populate-new-map ()
  (nu-define-prefix 'nu-new-map)

  (if (eq nu-major-mode 'dired-mode)
      ; switch what does d according to mode
      (define-key nu-new-map (kbd "d") 'dired-create-directory)
      (define-key nu-new-map (kbd "d") 'make-directory))

  (if (eq nu-major-mode 'magit-status-mode)
      (progn
      (define-key nu-new-map (kbd "b") 'magit-create-branch)
      (define-key nu-new-map (kbd "a") 'magit-annotated-tag)))

  (define-key nu-new-map (kbd "h") 'split-window-right)
  (define-key nu-new-map (kbd "i") 'ibuffer-other-window)
  (define-key nu-new-map (kbd "m") 'compose-mail)
  (define-key nu-new-map (kbd "n") 'nu-new-empty-buffer)
  (define-key nu-new-map (kbd "o")   'org-capture)
  (define-key nu-new-map (kbd "s") 'eshell)
  (define-key nu-new-map (kbd "t") 'term)
  (define-key nu-new-map (kbd "v") 'split-window-below)
  (define-key nu-new-map (kbd "k") 'kmacro-start-macro)
  (define-key nu-new-map (kbd "l") 'kmacro-end-macro)
  (define-key nu-new-map (kbd "u") 'name-last-kbd-macro)
  (define-key nu-new-map (kbd "x") 'kmacro-call-macro)
  (define-key nu-new-map (kbd "y") 'insert-kbd-macro)

  (define-key nu-new-map (kbd "f") 'make-frame-command))


(defun nu-new-prompt ()
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-new-map)
  (nu-prompt-for-keymap nu-new-map))


(defun nu-populate-visual-map ()
  (setq nu-visual-map (make-sparse-keymap))

  (define-key nu-visual-map (kbd "x") 'exchange-point-and-mark)
  (define-key nu-visual-map (kbd "q") 'pop-to-mark-command)
  (define-key nu-visual-map (kbd "r")  nu-replace-map)
  (define-key nu-visual-map (kbd "u") 'upcase-region)
  (define-key nu-visual-map (kbd "l") 'lowercase-region)
  (define-key nu-visual-map (kbd "o") 'copy-to-register)

  (if (bound-and-true-p rectangle-mark-mode)
      ;; rectangle selection
      (progn
	(define-key nu-visual-map (kbd "c") 'copy-rectangle-as-kill)
	(define-key nu-visual-map (kbd "k") 'kill-rectangle)
	(define-key nu-visual-map (kbd "t") 'string-rectangle)
	(define-key nu-visual-map (kbd "p") 'open-rectangle)
	(define-key nu-visual-map (kbd "z") 'clear-rectangle)
	(define-key nu-visual-map (kbd "v") 'string-insert-rectangle))

      ;; classical selection
      (progn
	(define-key nu-visual-map (kbd "f") 'flyspell-region)
	(define-key nu-visual-map (kbd "i") 'indent-region)
	(define-key nu-visual-map (kbd "k") 'kill-region)
	(define-key nu-visual-map (kbd "c") 'copy-region-as-kill)
	(define-key nu-visual-map (kbd "s") 'shell-command-on-region)
	(define-key nu-visual-map (kbd "p") 'eval-region)
	(define-key nu-visual-map (kbd "g") 'indent-rigidly))))

(defun nu-populate-a-map ()
  (nu-define-prefix 'nu-a-map)

  (define-key nu-a-map (kbd "C-f") 'cd)
    
  (cond
   ((eq nu-major-mode 'proced)
    (define-key nu-a-map (kbd "M-a") 'proced-unmark-all)
    (define-key nu-a-map (kbd "a") 'proced-mark-all)
    (define-key nu-a-map (kbd "c") 'proced-mark-children)
    (define-key nu-a-map (kbd "l") 'proced-mark)
    (define-key nu-a-map (kbd "p") 'proced-mark-parents)
    (define-key nu-a-map (kbd "t") 'proced-toggle-marks)
    (define-key nu-a-map (kbd "u") 'proced-unmark))
   ((eq nu-major-mode 'ibuffer-mode)
    (define-key nu-a-map (kbd "D") 'ibuffer-mark-dired-buffers)
    (define-key nu-a-map (kbd "H") 'ibuffer-mark-help-buffers)
    (define-key nu-a-map (kbd "M-d") 'ibuffer-mark-for-delete-backwards)
    (define-key nu-a-map (kbd "M-f") 'ibuffer-mark-dissociated-buffers)
    (define-key nu-a-map (kbd "M-i") 'ibuffer-backwards-next-marked)
    (define-key nu-a-map (kbd "M-k") 'ibuffer-forward-next-marked)
    (define-key nu-a-map (kbd "M-m") 'ibuffer-mark-by-mode)
    (define-key nu-a-map (kbd "M-r") 'nu-set-rectangle-mark)
    (define-key nu-a-map (kbd "M-u") 'ibuffer-unmark-all)
    (define-key nu-a-map (kbd "S") 'ibuffer-mark-special-buffers)
    (define-key nu-a-map (kbd "c") 'ibuffer-mark-unsaved-buffers)
    (define-key nu-a-map (kbd "d") 'ibuffer-mark-for-delete)
    (define-key nu-a-map (kbd "e") 'ibuffer-mark-modified-buffers)
    (define-key nu-a-map (kbd "f") 'ibuffer-mark-by-file-name-regexp)
    (define-key nu-a-map (kbd "i") 'ibuffer-unmark-backward)
    (define-key nu-a-map (kbd "k") 'ibuffer-mark-forward)
    (define-key nu-a-map (kbd "m") 'ibuffer-mark-by-mode-regexp)
    (define-key nu-a-map (kbd "n") 'ibuffer-mark-by-name-regexp)
    (define-key nu-a-map (kbd "o") 'ibuffer-mark-old-buffers)
    (define-key nu-a-map (kbd "r") 'ibuffer-mark-read-only-buffers)
    (define-key nu-a-map (kbd "t") 'ibuffer-toggle-marks)
    (define-key nu-a-map (kbd "u") 'ibuffer-unmark-forward)
    (define-key nu-a-map (kbd "x") 'ibuffer-do-kill-on-deletion-marks)
    (define-key nu-a-map (kbd "z") 'ibuffer-mark-compressed-file-buffers))
   ((eq nu-major-mode 'dired-mode)
    (define-key nu-a-map (kbd "M-r") 'dired-mark-files-containing-regexp)
    (define-key nu-a-map (kbd "M-u") 'dired-unmark-all-marks)
    (define-key nu-a-map (kbd "d") 'dired-flag-file-deletion)
    (define-key nu-a-map (kbd "m") 'dired-mark)
    (define-key nu-a-map (kbd "r") 'dired-flag-files-regexp)
    (define-key nu-a-map (kbd "r") 'dired-mark-files-regexp)
    (define-key nu-a-map (kbd "s") 'nu-mark-subdirs-files)
    (define-key nu-a-map (kbd "u") 'dired-unmark)
    (define-key nu-a-map (kbd "x") 'dired-toggle-marks))
   (t
     (define-key nu-a-map (kbd "a") 'nu-mark-whole-buffer)
     (define-key nu-a-map (kbd "s") 'nu-mark-sexp)
     (define-key nu-a-map (kbd "g") 'nu-mark-paragraph)
     (define-key nu-a-map (kbd "p") 'nu-mark-page)
     (define-key nu-a-map (kbd "f") 'nu-mark-defun)
     (define-key nu-a-map (kbd "i") 'nu-set-mark)
     (define-key nu-a-map (kbd "r") 'nu-set-rectangle-mark))))

(defun nu-a-prompt ()
  "Triggers nu-a-map.

But if mark is active, exchange point and mark."
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-a-map)
  (nu-prompt-for-keymap nu-a-map))

; on #master this is C,
; on #noxpaddle this is M...
(defun nu-populate-open-map ()
"Populate open map."
  (setq nu-open-map nil)
  (nu-define-prefix 'nu-open-map)

  ;; common case
  (define-key nu-open-map (kbd "B")  'bookmark-jump)
  (define-key nu-open-map (kbd "c")  'calc)
  (define-key nu-open-map (kbd "C-f")  'find-file-other-window)
  (define-key nu-open-map (kbd "e")  'find-file-read-only)
  (define-key nu-open-map (kbd "E")  'find-file-read-only-other-window)
  (define-key nu-open-map (kbd "C-j")   'nu-previous-buffer)
  (define-key nu-open-map (kbd "C-l")  'nu-next-buffer)
  (define-key nu-open-map (kbd "C-o") 'ido-switch-buffer)
  (define-key nu-open-map (kbd "a")   'org-agenda)
  (define-key nu-open-map (kbd "b")  'nu-bookmarks)
  (define-key nu-open-map (kbd "f")  'nu-find-files)
  (define-key nu-open-map (kbd "i")   'ibuffer)
  (define-key nu-open-map (kbd "l")   'nu-buffers-list)
  (define-key nu-open-map (kbd "m")   'menu-bar-read-mail)
  (define-key nu-open-map (kbd "p") 'package-list-packages)
  (define-key nu-open-map (kbd "r")  'nu-recentf)
  (define-key nu-open-map (kbd "R")  'recentf-open-files)
  (define-key nu-open-map (kbd "s") 'org-iswitchb)
  (define-key nu-open-map (kbd "t") 'visit-tags-table)
  (define-key nu-open-map (kbd "u")  'browse-url)
  (define-key nu-open-map (kbd "z")  'customize)
  (define-key nu-open-map (kbd "x")  'list-registers)
  (define-key nu-open-map (kbd "d")  'dired)
  (define-key nu-open-map (kbd "d")  'dired-other-window)

  (cond
   ((eq nu-major-mode 'magit-status-mode)
    (define-key nu-open-map (kbd "g") 'magit-log-long)
    (define-key nu-open-map (kbd "C-b") 'magit-branch-manager))
   ((eq nu-major-mode 'dired-mode)
    (define-key nu-open-map (kbd "d") 'dired-find-file)
    (define-key nu-open-map (kbd "C-d") 'dired-find-file-other-window))
   ((eq nu-major-mode 'ibuffer-mode)
    (define-key nu-open-map (kbd "h") 'ibuffer-do-view-horizontally)
    (define-key nu-open-map (kbd "i") 'ibuffer-find-file))))


(defun nu-open-prompt ()
"Maybe temporary prompt."
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-open-map)
  (nu-prompt-for-keymap nu-open-map))




(defun nu-populate-goto-map ()
"Populate goto map."
  (setq nu-goto-map nil)
  (nu-define-prefix 'nu-goto-map)

  ;; actually this case is : all read only modes...
  (cond
   ((eq nu-major-mode 'dired-mode)
     ; i should go parent dir. k should try to persistent-action subdir.
    (define-key nu-goto-map (kbd "u") 'dired-prev-marked-file)
    (define-key nu-goto-map (kbd "o") 'dired-next-marked-file))
   ((eq nu-major-mode 'help-mode)
    (define-key nu-goto-map (kbd "L") 'forward-button)
    (define-key nu-goto-map (kbd "J") 'backward-button)
    (define-key nu-goto-map (kbd "o") 'push-button)
    (define-key nu-goto-map (kbd "u") 'help-go-back))
   ((eq nu-major-mode 'ibuffer-mode)
    (define-key nu-goto-map (kbd "J") 'ibuffer-jump-to-buffer))
   (t
      ; else - default goto map.
      (define-key nu-goto-map (kbd "L") 'forward-sentence)
      (define-key nu-goto-map (kbd "J") 'backward-sentence)
      (define-key nu-goto-map (kbd "u") 'backward-paragraph)
      (define-key nu-goto-map (kbd "o") 'forward-paragraph)
      (define-key nu-goto-map (kbd "f") 'end-of-defun)
      (define-key nu-goto-map (kbd "M-f") 'beginning-of-defun)
      (define-key nu-goto-map (kbd "d") 'forward-sexp)
      (define-key nu-goto-map (kbd "h") 'backward-sexp)
      (define-key nu-goto-map (kbd "*") 'forward-list)
      (define-key nu-goto-map (kbd "M-*") 'backward-list)))

   ;
   ; common keys
   ;

   (define-key nu-goto-map (kbd "M-e") 'previous-error)
   (define-key nu-goto-map (kbd "M-g") 'nu-goto-line-previousbuffer)

   (define-key nu-goto-map (kbd "M-s") 'org-mark-ring-goto)
   (define-key nu-goto-map (kbd "e") 'next-error)
   (define-key nu-goto-map (kbd "g") 'ace-window)
   (define-key nu-goto-map (kbd "m") 'avy-goto-line)
   (define-key nu-goto-map (kbd "n") 'goto-line)
   (define-key nu-goto-map (kbd "t") 'goto-char)
   (define-key nu-goto-map (kbd "r") 'jump-to-register)
   (define-key nu-goto-map (kbd "v") 'view-register)
   (define-key nu-goto-map (kbd "i") 'beginning-of-buffer)
   (define-key nu-goto-map (kbd "k") 'end-of-buffer)
   (define-key nu-goto-map (kbd "s") 'nu-find-previous-mark)
   (define-key nu-goto-map (kbd "a") 'forward-page)
   (define-key nu-goto-map (kbd "c") 'backward-page)
   (define-key nu-goto-map (kbd "b") 'nu-buffers-list))


(defun nu-goto-prompt ()
"Offer to goto wherever wished."
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-goto-map)
  (nu-prompt-for-keymap nu-goto-map))


(defun nu-global-prompt ()
"Offer x keymap. Temporary function."
  (interactive)
  (nu-prompt-for-keymap ctl-x-map))


(defun nu-help-prompt ()
  (interactive)
  (lv-message
    (concat
      (propertize "\n Welcome to nu-mode\n\n" 'face 'bold)
      " This screen does provide some help to use nu-mode.\n It is shown at startup.\n"
      " Enter any key to quit this prompt or "(propertize "Space" 'face 'nu-face-shortcut)
      " to obtain the cheat sheet."
      "\n To disable this screen, put this in your init file\n\n"
        (propertize " (require 'nu-mode)\n" 'face 'italic)
	(propertize " (setq nu-mode-show-welcome-screen nil)\n" 'face 'error)
	(propertize " (nu-mode)" 'face 'italic)
      "\n\n To obtain Help, use "
      (propertize "Control+h" 'face 'nu-face-shortcut)
      "\n For example, to obtain a Cheat Sheet, use "
      (propertize "Control+h Space" 'face 'nu-face-shortcut)
      "\n\n To enter a command (M-x in vanilla Emacs), use "
      (propertize "Control+d" 'face 'nu-face-shortcut)
      " or " (propertize "Alt+d Return" 'face 'nu-face-shortcut)
      ".\n To quit a command, use "
      (propertize "Alt+q" 'face 'nu-face-shortcut)))
  (setq answer (read-key ""))
  (lv-delete-window)
  (if (eq answer 32)
      (nu-cheat-sheet)))

(defun nu-populate-find-map ()
  (nu-define-prefix 'nu-find-map)

  (cond
   ((eq nu-major-mode 'dired-mode)
    (define-key nu-find-map (kbd "f") 'dired-mark-files-containing-regexp)
    (define-key nu-find-map (kbd "s") 'dired-isearch-filenames)
    (define-key nu-find-map (kbd "M-s") 'dired-do-isearch)
    (define-key nu-find-map (kbd "%") 'dired-isearch-filenames-regexp)
    (define-key nu-find-map (kbd "x") 'dired-do-isearch-regexp))
   ((eq nu-major-mode 'ibuffer-mode)
    (define-key nu-find-map (kbd "M-o") 'ibuffer-do-occur))
   (t
    (define-key nu-find-map (kbd "F") 'nu-isearch-forward)
    (define-key nu-find-map (kbd "M-F") 'search-forward-regexp)
    (define-key nu-find-map (kbd "M-R") 'search-backward-regexp)
    (define-key nu-find-map (kbd "M-f") 'nu-isearch-forward-regexp)
    (define-key nu-find-map (kbd "M-z") 'nu-find-char-backward)
    (define-key nu-find-map (kbd "R") 'nu-isearch-backward)
    (define-key nu-find-map (kbd "f") 'ace-jump-char-mode)
    (define-key nu-find-map (kbd "l") 'ace-jump-line-mode)
    (define-key nu-find-map (kbd "r") 'nu-isearch-backward-regexp)
    (define-key nu-find-map (kbd "w") 'ace-jump-word-mode)))

  ; common keys
  (define-key nu-find-map (kbd "b") 'regexp-builder)
  (define-key nu-find-map (kbd "g") 'rgrep)
  (define-key nu-find-map (kbd "o") 'occur)
  (define-key nu-find-map (kbd "t") 'find-tag)
  (define-key nu-find-map (kbd "T") 'find-tag-other-window)
  (define-key nu-find-map (kbd "u") 'tags-search)
  (define-key nu-find-map (kbd "z") 'nu-find-char))

(defun nu-find-prompt ()
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-find-map)
  (nu-prompt-for-keymap nu-find-map))


(defun nu-toggle-read-only ()
 "Toggle read only mode."
 (interactive)
 (run-with-timer 0.1 nil 'lambda ()
              (if (eq buffer-read-only t)
                  (read-only-mode -1)
                  (read-only-mode 1)))
 (message "read only toggled."))


(defun nu-populate-copy-map ()
  (nu-define-prefix 'nu-copy-map)
  (cond
   ((eq nu-major-mode 'dired-mode)
    (define-key nu-copy-map (kbd "c") 'dired-do-copy)
    (define-key nu-copy-map (kbd "C-c") 'dired-copy-filename-as-kill)
    (define-key nu-copy-map (kbd "h") 'dired-do-hardlink)
    (define-key nu-copy-map (kbd "s") 'dired-do-symlink))
   (t
    (define-key nu-copy-map (kbd "k") 'avy-kill-ring-save-whole-line)
    (define-key nu-copy-map (kbd "i") 'avy-copy-region)
    (define-key nu-copy-map (kbd "l") 'avy-copy-line)

    (define-key nu-copy-map (kbd "r") 'copy-rectangle-as-kill)
    (define-key nu-copy-map (kbd "c") 'nu-copy-region-or-line)
    (define-key nu-copy-map (kbd "e") 'nu-copy-from-above)
    (define-key nu-copy-map (kbd "y") 'nu-copy-from-below))))


(defun nu-copy-prompt ()
 (interactive)
  (setq nu-major-mode major-mode)
 (nu-populate-copy-map)
 (nu-prompt-for-keymap nu-copy-map))


(defalias 'toggle-sorting-by-date-or-name 'dired-sort-toggle-or-edit)

(defun nu-populate-replace-dired ()
  (setq nu-replace-map nil)
  (nu-define-prefix 'nu-replace-map)
  (define-key nu-replace-map (kbd "c") 'dired-do-chmod)
  (define-key nu-replace-map (kbd "d") 'dired-downcase)
  (define-key nu-replace-map (kbd "g") 'dired-do-chgrp)
  (define-key nu-replace-map (kbd "o") 'dired-do-chown)
  (define-key nu-replace-map (kbd "r") 'dired-do-rename)
  (define-key nu-replace-map (kbd "s") 'toggle-sorting-by-date-or-name)
  (define-key nu-replace-map (kbd "i") 'dired-do-touch)
  (define-key nu-replace-map (kbd "u") 'dired-upcase)
  (define-key nu-replace-map (kbd "w") 'wdired-change-to-wdired-mode)
  (define-key nu-replace-map (kbd "z") 'dired-do-compress))

(defun nu-populate-replace-ibuffer ()
  (setq nu-replace-map nil)
  (nu-define-prefix 'nu-replace-map)
  (define-key nu-replace-map (kbd "M-r") 'ibuffer-do-replace-regexp)
  (define-key nu-replace-map (kbd "a") 'ibuffer-do-sort-by-alphabetic)
  (define-key nu-replace-map (kbd "f") 'ibuffer-do-sort-by-filename/process)
  (define-key nu-replace-map (kbd "i") 'ibuffer-invert-sorting)
  (define-key nu-replace-map (kbd "m") 'ibuffer-do-sort-by-major-mode)
  (define-key nu-replace-map (kbd "n") 'ibuffer-do-rename-uniquely)
  (define-key nu-replace-map (kbd "q") 'ibuffer-do-query-replace)
  (define-key nu-replace-map (kbd "r") 'ibuffer-do-revert)
  (define-key nu-replace-map (kbd "s") 'ibuffer-do-sort-by-size)
  (define-key nu-replace-map (kbd "t") 'ibuffer-do-toggle-read-only)
  (define-key nu-replace-map (kbd "v") 'ibuffer-do-sort-by-recency))

(defalias 'git-checkout-item 'magit-discard-item)
(defalias 'git-pull-rebase 'magit-rebase-step)
(defalias 'git-toggle-amend-next-commit 'magit-log-edit-toggle-amending)


(defun nu-populate-replace-magit ()
  (setq nu-replace-map nil)
  (nu-define-prefix 'nu-replace-map)
  (define-key nu-replace-map (kbd "a") 'git-toggle-amend-next-commit)
  (define-key nu-replace-map (kbd "b") 'magit-move-branch)
  (define-key nu-replace-map (kbd "k") 'git-checkout-item)
  (define-key nu-replace-map (kbd "r") 'git-pull-rebase))


(defun nu-populate-replace ()
  "Create replace-keymap."
  (setq nu-replace-map nil)
  (nu-define-prefix 'nu-replace-map)

  (define-key nu-replace-map (kbd "J")  'join-line)
  (define-key nu-replace-map (kbd "M-f")  'sort-regexp-fields)
  (define-key nu-replace-map (kbd "M-r")  'query-replace-regexp)
  (define-key nu-replace-map (kbd "M-s")  'sort-lines)
  (define-key nu-replace-map (kbd "R")  'query-replace)
  (define-key nu-replace-map (kbd "a")  'revert-buffer)
  (define-key nu-replace-map (kbd "b")  'transpose-chars)
  (define-key nu-replace-map (kbd "i")  'transpose-words)
  (define-key nu-replace-map (kbd "l")  'transpose-sexps)
  (define-key nu-replace-map (kbd "o")  'transpose-sentences)
  (define-key nu-replace-map (kbd "c")  'split-line)
  (define-key nu-replace-map (kbd "e")  'keep-lines)
  (define-key nu-replace-map (kbd "f")  'sort-fields)
  (define-key nu-replace-map (kbd "g")  'clear-rectangle)
  (define-key nu-replace-map (kbd "G")  'delete-whitespace-rectangle)
  (define-key nu-replace-map (kbd "M-g")  'string-rectangle)
  (define-key nu-replace-map (kbd "j")  'nu-join-with-following-line)
  (define-key nu-replace-map (kbd "k")  'overwrite-mode)
  (define-key nu-replace-map (kbd "m") 'nu-toggle-read-only)
  (define-key nu-replace-map (kbd "n")  'sort-numeric-fields)
  (define-key nu-replace-map (kbd "r")  'replace-regexp)
  (define-key nu-replace-map (kbd "s")  'replace-string)
  (define-key nu-replace-map (kbd "t")  'transpose-lines)
  (define-key nu-replace-map (kbd "u") 'tags-query-replace)
  (define-key nu-replace-map (kbd "x") 'nu-rot-reg-or-toggle-rot)
  (define-key nu-replace-map (kbd "y") 'flyspell-mode)
  (if (bound-and-true-p flyspell-mode)
      (progn
         (define-key nu-replace-map (kbd "B") 'flyspell-buffer)
         (define-key nu-replace-map (kbd "R") 'flyspell-region) ;; in region keys?

         (define-key nu-replace-map (kbd "C") 'flyspell-correct-word-before-point)
         (define-key nu-replace-map (kbd "A") 'flyspell-auto-correct-word)
         (define-key nu-replace-map (kbd "E") 'flyspell-goto-next-error)
         (define-key nu-replace-map (kbd "P") 'flyspell-auto-correct-previous-word)))


  (if (not (fboundp 'ethan-wspace-untabify))
    (defun ethan-wspace-untabify ()
      (interactive)
      (message "Please install ethan-wspace mode!")))
  (define-key nu-replace-map (kbd "w") 'ethan-wspace-untabify)

  (if (eq nu-major-mode 'c-mode)
      (define-key nu-replace-map (kbd "y") 'c-set-style)))


(defun nu-replace-prompt ()
  (interactive)
  (setq nu-major-mode major-mode)
      (cond
        ((eq nu-major-mode 'dired-mode)
         (nu-populate-replace-dired)
	 (nu-prompt-for-keymap nu-replace-map))
	((eq nu-major-mode 'ibuffer-mode)
	 (nu-populate-replace-ibuffer)
         (nu-prompt-for-keymap nu-replace-map))
        ((eq nu-major-mode 'magit-status-mode)
         (nu-populate-replace-magit)
         (nu-prompt-for-keymap nu-replace-map))
        ((or (eq overwrite-mode 'overwrite-mode-textual)
             (eq overwrite-mode 'overwrite-mode-binary))
         (overwrite-mode -1))
        ((eq buffer-read-only t)
         (nu-toggle-read-only))
        (t
         (nu-populate-replace)
         (nu-prompt-for-keymap nu-replace-map))))


;;
;; term
;;


(defun nu-tmp-prompt-for-term-line-c-c ()
  "Temporary map... to be improved."
  (interactive)
  (nu-define-prefix 'nu-term-line-c-c)

  (define-key nu-term-line-c-c (kbd "c") 'nu-copy-region-or-line)

  ; stolen from term mode map C-c part...
  (define-key nu-term-line-c-c (kbd "C-\\") 'term-quit-subjob)
  (define-key nu-term-line-c-c (kbd "C-a") 'term-bol)
  (define-key nu-term-line-c-c (kbd "C-c") 'term-interrupt-subjob)
  (define-key nu-term-line-c-c (kbd "C-d") 'term-send-eof)
  (define-key nu-term-line-c-c (kbd "C-e") 'term-show-maximum-output)
  (define-key nu-term-line-c-c (kbd "C-j") 'term-line-mode)
  (define-key nu-term-line-c-c (kbd "C-k") 'term-char-mode)
  (define-key nu-term-line-c-c (kbd "C-l") 'term-dynamic-list-input-ring)
  (define-key nu-term-line-c-c (kbd "C-n") 'term-next-prompt)
  (define-key nu-term-line-c-c (kbd "C-o") 'term-kill-output)
  (define-key nu-term-line-c-c (kbd "C-p") 'term-previous-prompt)
  (define-key nu-term-line-c-c (kbd "C-q") 'term-pager-toggle)
  (define-key nu-term-line-c-c (kbd "C-r") 'term-show-output)
  (define-key nu-term-line-c-c (kbd "C-u") 'term-kill-input)
  (define-key nu-term-line-c-c (kbd "C-w") 'backward-kill-word)
  (define-key nu-term-line-c-c (kbd "C-z") 'term-stop-subjob)
  (define-key nu-term-line-c-c (kbd "RET") 'term-copy-old-input)

  (nu-prompt-for-keymap nu-term-line-c-c))

(defvar nu-term-map "Map for term single point of entry.")

(defun nu-prompt-for-term ()
  "This is a specific prompt designed to make term usable.

& still, respect cua principles. The idea is to let the user
both navigate, access to essential prompts, and control the terminal."
  (interactive)
  (nu-define-prefix 'nu-term-map)

  ; paddle keys are dedicated to direct functions
  ; either to navigate or do something with term.
  (define-key nu-term-map (kbd "i") 'nu-buffers-list)
  (define-key nu-term-map (kbd "k") 'ibuffer)
  (define-key nu-term-map (kbd "l") 'term-line-mode)


  ; This includes internal windows
  ; or other functions using modified buffers keys.
  ; since control is necessary to run this prompt
  ; we use control as the unique modifier...
   (define-key nu-term-map (kbd "C-i") 'windmove-up)
   (define-key nu-term-map (kbd "C-j") 'windmove-left)
   (define-key nu-term-map (kbd "C-k") 'windmove-down)
   (define-key nu-term-map (kbd "C-l") 'windmove-right)


  ; Prompt keys will run prompt, but that means
  ; at least three keys to run a func :
  ; term prompt -> nu prompt -> func
  ; so whenever it might be avoided it should.
  (define-key nu-term-map (kbd "c") 'nu-copy-region-or-line)
  (define-key nu-term-map (kbd "d") 'nu-delete-prompt)
  (define-key nu-term-map (kbd "g") 'nu-goto-prompt)
  (define-key nu-term-map (kbd "h") 'nu-help-prompt)
  (define-key nu-term-map (kbd "n") 'nu-new-prompt)
  (define-key nu-term-map (kbd "o") 'nu-open-prompt)
  (define-key nu-term-map (kbd "p") 'nu-print-prompt)
  (define-key nu-term-map (kbd "q") 'nu-quit-prompt)
  (define-key nu-term-map (kbd "w") 'nu-window-prompt)


  ; some specific stuff that do not fit well
  ; elsewhere...
  (define-key nu-term-map (kbd "C-c") 'term-interrupt-subjob)
  (define-key nu-term-map (kbd "C-<SPC>") 'term-pager-toggle)

  (nu-prompt-for-keymap nu-term-map))


(defun nu-populate-tab ()
  (nu-define-prefix 'nu-tab-map)
  (define-key nu-tab-map (kbd "i") 'delete-other-windows)
  (define-key nu-tab-map (kbd "j") 'minimize-window)
  (define-key nu-tab-map (kbd "k") 'delete-window)
  (define-key nu-tab-map (kbd "l") 'split-window-right)
  (define-key nu-tab-map (kbd "n") 'scroll-other-window)
  (define-key nu-tab-map (kbd "p") 'scroll-other-window-down)

  (define-key nu-tab-map (kbd "g") 'ido-switch-buffer-other-window)
  (define-key nu-tab-map (kbd "t") 'ace-window)

  (define-key nu-tab-map (kbd "u") 'windmove-up)
  (define-key nu-tab-map (kbd "h") 'windmove-left)
  (define-key nu-tab-map (kbd "o") 'windmove-down)
  (define-key nu-tab-map (kbd "m") 'windmove-right))

(defun nu-tab-prompt ()
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-tab)
  (nu-prompt-for-keymap nu-tab-map))

(defhydra hydra-nu-meta-menu (:color pink
			      :hint nil
			      :pre (setq nu-major-mode major-mode))
"\n
  _h_ ibuffer        _k_ kill window
  _i_ open file      _l_ switch buffer
  _m_ aximize        _u_ new frame
  _x_ goto line      _a_ goto char
  _r_ goto symbol    _o_ goto word
  _g_ global prompt

  _n_ mode specific map
  _p_ M-x (execute command)
  _f_ Ctrl x maps
"
    ;; direct func : open
    ("i" nu-find-files :exit t)
    ("l" nu-buffers-list :exit t)
    ("j" nu-recentf :exit t)
    ("h" ibuffer :exit t)

    ;; direct func : goto
    ("x" avy-goto-line :exit t)
    ("a" avy-goto-char :exit t)
    ("r" avy-goto-symbol-1 :exit t)
    ("o" avy-goto-word-1 :exit t)

    ;; direct func : tab/frame/win
    ("k" kill-buffer-and-window :exit t)
    ("u" make-frame-command :exit t)
    ("m" delete-other-windows :exit t)

    ;; prompts / maps / commands
    ("n" nu-trigger-mode-specific-map :exit t)
    ("g" nu-global-prompt :exit t)
    ("p" (nu-M-x) :exit t)
    ("f" (nu-buffer-prompt-for-keymap ctl-x-map) :exit t))

(provide 'nu-menus)
