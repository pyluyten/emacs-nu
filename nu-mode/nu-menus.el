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

(defvar nu-populate-hook nil
  "Hook running *after* a menu is populated.

This hook normaly allows to add keys to some menus,
either regarding some conditions like a mode being active,
or in order to let the user customize

There is a convention. nu-mode uses letters for prompts.
q is forbidden in *any* prompt, as for '?' '-' '[0-9]'.

A major mode func added to the populate hook might rebind
an existing func (like i j k l),
or define a new capitalized letter (like I J K L).

A minor mode might add new func with Alt- as prefix
(like Alt-i Alt-j Alt-k Alt-l)
or Ctrl as prefix.

As in nu-keymap, the user owns the punctation.")




(defun nu-populate-switch ()
   (nu-define-prefix 'nu-switch-map)
   (define-key nu-switch-map "c" 'customize)
   (define-key nu-switch-map "s" 'subword-mode))

(defun nu-switch-prompt ()
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-switch)
  (run-hooks 'nu-populate-hook)
  (nu-prompt-for-keymap nu-switch-map))


(defun nu-populate-display ()
   (nu-define-prefix 'nu-display-map)

   (define-key nu-display-map "d" 'scroll-other-window-down)
   (define-key nu-display-map "u" 'scroll-left)
   (define-key nu-display-map "s" 'scroll-other-window)
   (define-key nu-display-map "o" 'scroll-right)
   (define-key nu-display-map "r" 'recenter-top-bottom)

   (define-key nu-display-map "i" 'enlarge-window)
   (define-key nu-display-map "l" 'enlarge-window-horizontally)

   (define-key nu-display-map "k" 'shrink-window)
   (define-key nu-display-map "j" 'shrink-window-horizontally))


(defun nu-display-prompt ()
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-display)
  (run-hooks 'nu-populate-hook)
  (nu-prompt-for-keymap nu-display-map))


(defun nu-populate-print ()
  (nu-define-prefix 'nu-print-map)

  (define-key nu-print-map "b" 'print-buffer)
  (define-key nu-print-map "d" 'pwd)
  (define-key nu-print-map "f" 'find-grep)
  (define-key nu-print-map "g" 'grep)
  (define-key nu-print-map "i" 'ediff)
  (define-key nu-print-map "m" 'compile)
  (define-key nu-print-map "n" 'negative-argument)
  (define-key nu-print-map "p" 'kmacro-end-or-call-macro)
  (define-key nu-print-map "P" 'kmacro-call-macro)
  (define-key nu-print-map "r" 're-builder)
  (define-key nu-print-map "u" 'universal-argument)
  (define-key nu-print-map ":" 'eval-expression)
  (define-key nu-print-map "z" 'async-shell-command))


(defun nu-print-prompt ()
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-print)
  (run-hooks 'nu-populate-hook)
  (nu-prompt-for-keymap nu-print-map))


(defun nu-populate-quit ()
  "Populate quit map."
 (nu-define-prefix 'nu-quit-map)
 (define-key nu-quit-map "d" 'delete-frame)
 (define-key nu-quit-map "f" 'suspend-frame)
 (define-key nu-quit-map "g" 'keyboard-escape-quit)
 (define-key nu-quit-map "m" 'minimize-window)
 (define-key nu-quit-map "s" 'save-buffers-kill-emacs)
 (define-key nu-quit-map "w" 'quit-window)
 (define-key nu-quit-map "y" 'kill-emacs)
 (define-key nu-quit-map "z" 'suspend-emacs))


(defun nu-quit-prompt ()
  "Prompt to quit."
  (interactive)
  (setq nu-major-mode major-mode)
  (if mark-active
      (cua-set-mark)
      (progn
        (nu-populate-quit)
	(run-hooks 'nu-populate-hook)
        (nu-prompt-for-keymap nu-quit-map))))

(defun nu-populate-kill ()
  "Populate nu-kill-map."
  (nu-define-prefix 'nu-kill-map)

  (define-key nu-kill-map "a" 'bookmark-delete)
  (define-key nu-kill-map "b" 'kill-buffer)
  (define-key nu-kill-map "h" 'delete-horizontal-space)
  (define-key nu-kill-map "i" 'fixup-whitespace)
  (define-key nu-kill-map "j" 'delete-duplicate-lines)
  (define-key nu-kill-map "l" 'delete-marching-line)
  (define-key nu-kill-map "k" 'delete-blank-lines)
  (define-key nu-kill-map "t" 'delete-trailing-whitespace)
  (define-key nu-kill-map "u" 'flush-lines)
  (define-key nu-kill-map "x" 'kill-sexp)
  (define-key nu-kill-map "y" 'nu-delete-all)

  (if mark-active
    (define-key nu-kill-map (kbd "<RET>") 'kill-region)))

(defun nu-kill-prompt ()
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-kill)
  (run-hooks 'nu-populate-hook)

  (define-key nu-kill-map "f" 'delete-file)
  (define-key nu-kill-map "o" 'delete-other-windows)
  (define-key nu-kill-map "w" 'delete-window)
  (nu-prompt-for-keymap nu-kill-map))


(defun nu-populate-change-map ()
 "Populate change map"
  (nu-define-prefix 'nu-change-map)
  (define-key nu-change-map "a" 'align)
  (define-key nu-change-map "c" 'capitalize-word)
  (define-key nu-change-map "d" 'downcase-word)
  (define-key nu-change-map "e" 'facemenu-face-menu)
  (define-key nu-change-map "f" 'fill-paragraph)
  (define-key nu-change-map "i" 'indent)
  (define-key nu-change-map "m" 'transpose-frame)
  (define-key nu-change-map "n" 'set-fill-column)
  (define-key nu-change-map "o" 'indent-for-comment)
  (define-key nu-change-map "r" 'set-fill-prefix)
  (define-key nu-change-map "s" 'indent-sexp)
  (define-key nu-change-map "u" 'upcase-word))


(defun nu-change-prompt ()
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-change-map)
  (run-hooks 'nu-populate-hook)
  (nu-prompt-for-keymap nu-change-map))


(defun nu-populate-insert-map ()
 "Populate insert map	.	"
  (nu-define-prefix 'nu-insert-map)

  (define-key nu-insert-map "b" 'insert-buffer)
  (define-key nu-insert-map "c" 'quoted-insert)
  (define-key nu-insert-map "f" 'insert-file)
  (define-key nu-insert-map "g" 'define-global-abbrev)
  (define-key nu-insert-map "h" 'nu-browse-kill-ring)
  (define-key nu-insert-map "i" 'nu-insert-line-above)
  (define-key nu-insert-map "k" 'nu-insert-line-below)
  (define-key nu-insert-map "l" 'open-rectangle)
  (define-key nu-insert-map "m" 'dabbrev-expand)
  (define-key nu-insert-map "p" 'yank)
  (define-key nu-insert-map "r" 'insert-register)
  (define-key nu-insert-map "v" 'expand-abbrev)
  (define-key nu-insert-map "y" 'yank-rectangle)
  (define-key nu-insert-map "z" 'async-shell-command)
  (define-key nu-insert-map "Z" 'shell-command))


(defun nu-insert-prompt ()
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-insert-map)
  (run-hooks 'nu-populate-hook)
  (nu-prompt-for-keymap nu-insert-map))


(defun nu-populate-save-map ()
 "Populate save map	.	"
  (nu-define-prefix 'nu-save-map)
  (define-key nu-save-map "a" 'bookmark-set)
  (define-key nu-save-map "b" 'save-buffer)
  (define-key nu-save-map "c" 'inverse-add-global-abbrev)
  (define-key nu-save-map "d" 'add-global-abbrev)
  (define-key nu-save-map "e" 'frame-configuration-to-register)
  (define-key nu-save-map "f" 'write-file)
  (define-key nu-save-map "h" 'nu-toggle-goal-column)
  (define-key nu-save-map "i" 'recover-session)
  (define-key nu-save-map "j" 'add-mode-abbrev)
  (define-key nu-save-map "l" 'org-store-link)

  (if (not (eq nu-major-mode 'magit-status-mode))
      (define-key nu-save-map "m" 'magit-status)
      (define-key nu-save-map "m" 'kmacro-start-macro-or-insert-counter))

  (define-key nu-save-map "M" 'inverse-add-mode-abbrev)
  (define-key nu-save-map "n" 'rename-buffer)
  (define-key nu-save-map "o" 'save-some-buffers)
  (define-key nu-save-map "r" 'point-to-register)
  (define-key nu-save-map "t" 'nu-create-tags)
  (define-key nu-save-map "u" 'revert-buffer)
  (define-key nu-save-map "w" 'window-configuration-to-register))


(defun nu-save-prompt ()
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-save-map)
  (run-hooks 'nu-populate-hook)
  (nu-prompt-for-keymap nu-save-map))


(defun nu-populate-new-map ()
  (nu-define-prefix 'nu-new-map)

  (define-key nu-new-map "*" 'org-capture)
  (define-key nu-new-map "c" 'calc)
  (define-key nu-new-map "f" 'make-frame-command)
  (define-key nu-new-map "m" 'compose-mail)
  (define-key nu-new-map "n" 'nu-new-empty-buffer)
  (define-key nu-new-map "o" 'ibuffer-other-window)
  (define-key nu-new-map "P" 'kmacro-end-macro)
  (define-key nu-new-map "p" 'kmacro-start-macro)
  (define-key nu-new-map "r" 'split-window-right)
  (define-key nu-new-map "s" 'eshell)
  (define-key nu-new-map "u" 'name-last-kbd-macro)
  (define-key nu-new-map "w" 'split-window-below)
  (define-key nu-new-map "y" 'insert-kbd-macro)
  (define-key nu-new-map "z" 'term))

(defun nu-new-prompt ()
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-new-map)
  (run-hooks 'nu-populate-hook)
  (nu-prompt-for-keymap nu-new-map))

;; visual map is only used in nu-mode
;; it is not used in evil backend
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

; nu a map is used in nu-mode
; it is not used in evil backend
(defun nu-populate-mark-map ()
  (nu-define-prefix 'nu-mark-map)

  (define-key nu-mark-map (kbd "y") 'nu-mark-whole-buffer)
  (define-key nu-mark-map (kbd "s") 'nu-mark-sexp)
  (define-key nu-mark-map (kbd "g") 'nu-mark-paragraph)
  (define-key nu-mark-map (kbd "p") 'nu-mark-page)
  (define-key nu-mark-map (kbd "d") 'nu-mark-defun)
  (define-key nu-mark-map (kbd "i") 'nu-set-mark)
  (define-key nu-mark-map (kbd "r") 'nu-set-rectangle-mark))

(defun nu-mark-prompt ()
  "Triggers nu-mark-map"
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-mark-map)
  (run-hooks 'nu-populate-hook)
  (define-key nu-mark-map (kbd "f") 'cd)
  (nu-prompt-for-keymap nu-mark-map))


(defun nu-populate-open-map ()
"Populate open map."
  (setq nu-open-map nil)
  (nu-define-prefix 'nu-open-map)

  ;; common case
  (define-key nu-open-map "a" 'nu-bookmarks)
  (define-key nu-open-map "b" 'nu-view-buffer-other-window)
  (define-key nu-open-map "c" 'recentf-open-files)
  (define-key nu-open-map "d" 'dired)
  (define-key nu-open-map "e" 'find-file-read-only)
  (define-key nu-open-map "f" 'nu-find-files)
  (define-key nu-open-map "g" 'org-agenda)
  (define-key nu-open-map "h" 'ido-switch-buffer)
  (define-key nu-open-map "i" 'ibuffer)
  (define-key nu-open-map "j" 'nu-next-buffer)
  (define-key nu-open-map "k" 'nu-previous-buffer)
  (define-key nu-open-map "l" 'nu-buffers-list)
  (define-key nu-open-map "m" 'menu-bar-read-mail)
  (define-key nu-open-map "n" 'find-file-read-only-other-window)
  (define-key nu-open-map "o" 'find-file-other-window)
  (define-key nu-open-map "p" 'package-list-packages)
  (define-key nu-open-map "r" 'nu-recentf)
  (define-key nu-open-map "s" 'org-iswitchb)
  (define-key nu-open-map "t" 'visit-tags-table)
  (define-key nu-open-map "u" 'browse-url)
  (define-key nu-open-map "v" 'dired-other-window)
  (define-key nu-open-map "w" 'ibuffer-other-window)
  (define-key nu-open-map "x" 'list-registers)
  (define-key nu-open-map "z" 'customize))


(defun nu-open-prompt ()
"Maybe temporary prompt."
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-open-map)
  (run-hooks 'nu-populate-hook)
  (nu-prompt-for-keymap nu-open-map))


(defun nu-populate-goto-map ()
"Populate goto map."
  (setq nu-goto-map nil)
  (nu-define-prefix 'nu-goto-map)

  (define-key nu-open-map "a" 'bookmark-jump)
  (define-key nu-goto-map "l" 'forward-list)
  (define-key nu-goto-map "i" 'backward-list))

(defun nu-goto-prompt ()
"Offer to goto wherever wished."
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-goto-map)
  (run-hooks 'nu-populate-hook)

  ;
  ; common keys
  ;

  (define-key nu-goto-map "E" 'previous-error)
  (define-key nu-goto-map "M" 'org-mark-ring-goto)
  (define-key nu-goto-map "a" 'forward-page)
  (define-key nu-goto-map "c" 'backward-page)
  (define-key nu-goto-map "e" 'next-error)
  (define-key nu-goto-map "g" 'goto-line)
  (define-key nu-goto-map "k" 'avy-goto-line)
  (define-key nu-goto-map "m" 'nu-find-previous-mark)
  (define-key nu-goto-map "o" 'end-of-buffer)
  (define-key nu-goto-map "r" 'jump-to-register)
  (define-key nu-goto-map "t" 'goto-char)
  (define-key nu-goto-map "u" 'beginning-of-buffer)
  (define-key nu-goto-map "v" 'view-register)
  (define-key nu-goto-map "w" 'ace-window)
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
      " Enter any key to quit this prompt or "(propertize "SPC" 'face 'nu-face-shortcut)
      " to obtain the cheat sheet."
      "\n To disable this screen, put this in your init file\n\n"
        (propertize " (require 'nu-mode)\n" 'face 'italic)
	(propertize " (setq nu-show-welcome-screen nil)\n" 'face 'error)
	(propertize " (nu-mode)" 'face 'italic)
      "\n\n To obtain Help, use "
      (propertize "Control+h" 'face 'nu-face-shortcut)
      "\n For example, to obtain a Cheat Sheet, use "
      (propertize (substitute-command-keys "\\[nu-cheat-sheet]") 'face 'nu-face-shortcut)
      "\nor press ² at any time.\nTo enter a command (M-x in vanilla Emacs), use "
      (propertize (substitute-command-keys "\\[nu-M-x]") 'face 'nu-face-shortcut)
      ".\n To quit a command, use "
      (propertize (substitute-command-keys "\\[keyboard-escape-quit]") 'face 'nu-face-shortcut)))
  (setq answer (read-key ""))
  (lv-delete-window)
  (if (eq answer 32)
      (nu-cheat-sheet)))

(defun nu-populate-find-map ()
  (nu-define-prefix 'nu-find-map)

  ; fundamental mode
  (define-key nu-find-map "f" 'ace-jump-char-mode)
  (define-key nu-find-map "l" 'ace-jump-line-mode)
  (define-key nu-find-map "w" 'ace-jump-word-mode)

  ; common keys
  (define-key nu-find-map "d" 'find-name-dired)
  (define-key nu-find-map "e" 'find-grep-dired)
  (define-key nu-find-map "h" 'highlight-regexp)
  (define-key nu-find-map "k" 'list-matching-lines)
  (define-key nu-find-map "x" 'regexp-builder)
  (define-key nu-find-map "r" 'rgrep)
  (define-key nu-find-map "g" 'grep)
  (define-key nu-find-map "c" 'occur)
  (define-key nu-find-map "t" 'find-tag)
  (define-key nu-find-map "o" 'find-tag-other-window)
  (define-key nu-find-map "s" 'tags-search))

(defun nu-find-prompt ()
  (interactive)
  (setq nu-major-mode major-mode)
  (nu-populate-find-map)
  (run-hooks 'nu-populate-hook)
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

  (define-key nu-copy-map (kbd "k") 'avy-kill-ring-save-whole-line)
  (define-key nu-copy-map (kbd "i") 'avy-copy-region)
  (define-key nu-copy-map (kbd "l") 'avy-copy-line)
  (define-key nu-copy-map (kbd "r") 'copy-rectangle-as-kill))


(defun nu-copy-prompt ()
 (interactive)
  (setq nu-major-mode major-mode)
 (nu-populate-copy-map)
    (nu-prompt-for-keymap nu-copy-map))


(defun nu-populate-replace ()
  "Create replace-keymap."
  (setq nu-replace-map nil)
  (nu-define-prefix 'nu-replace-map)

  (define-key nu-replace-map "a" 'clear-rectangle)
  (define-key nu-replace-map "b" 'revert-buffer)
  (define-key nu-replace-map "c" 'transpose-chars)
  (define-key nu-replace-map "d" 'delete-whitespace-rectangle)
  (define-key nu-replace-map "e" 'keep-lines)
  (define-key nu-replace-map "f" 'sort-fields)
  (define-key nu-replace-map "g" 'split-line)
  (define-key nu-replace-map "h" 'sort-regexp-fields)
  (define-key nu-replace-map "i" 'transpose-words)
  (define-key nu-replace-map "j" 'nu-join-with-following-line)
  (define-key nu-replace-map "k" 'overwrite-mode)
  (define-key nu-replace-map "l" 'transpose-sexps)
  (define-key nu-replace-map "m" 'nu-toggle-read-only)
  (define-key nu-replace-map "n" 'sort-numeric-fields)
  (define-key nu-replace-map "o" 'transpose-sentences)
  (define-key nu-replace-map "p" 'join-line)
  (define-key nu-replace-map "r" 'query-replace)
  (define-key nu-replace-map "s" 'replace-string)
  (define-key nu-replace-map "t" 'transpose-lines)
  (define-key nu-replace-map "u" 'tags-query-replace)
  (define-key nu-replace-map "v" 'string-rectangle)
  (define-key nu-replace-map "w" 'query-replace-regexp)
  (define-key nu-replace-map "x" 'nu-rot-reg-or-toggle-rot)
  (define-key nu-replace-map "x" 'replace-regexp)
  (define-key nu-replace-map "y" 'flyspell-mode)
  (define-key nu-replace-map "z" 'sort-lines))


(defun nu-replace-prompt ()
  (interactive)
  (setq nu-major-mode major-mode)
      (cond
        ((or (eq overwrite-mode 'overwrite-mode-textual)
             (eq overwrite-mode 'overwrite-mode-binary))
         (overwrite-mode -1))
        ((eq buffer-read-only t)
         (nu-toggle-read-only))
        (t
         (nu-populate-replace)
         (run-hooks 'nu-populate-hook)
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

  ; Prompt keys will run prompt, but that means
  ; at least three keys to run a func :
  ; term prompt -> nu prompt -> func
  ; so whenever it might be avoided it should.
  (define-key nu-term-map (kbd "c") 'nu-copy-region-or-line)
  (define-key nu-term-map (kbd "d") 'nu-kill-prompt)
  (define-key nu-term-map (kbd "g") 'nu-goto-prompt)
  (define-key nu-term-map (kbd "h") 'nu-help-prompt)
  (define-key nu-term-map (kbd "n") 'nu-new-prompt)
  (define-key nu-term-map (kbd "o") 'nu-open-prompt)
  (define-key nu-term-map (kbd "p") 'nu-print-prompt)
  (define-key nu-term-map (kbd "q") 'nu-quit-prompt)
  (define-key nu-term-map (kbd "w") 'nu-display-prompt)
  (define-key nu-term-map (kbd "C-c") 'term-interrupt-subjob)
  (define-key nu-term-map (kbd "C-<SPC>") 'term-pager-toggle)

  (nu-prompt-for-keymap nu-term-map))


;;
;; nu do map is only populated @ startup
;;

(defvar nu-do-map (make-sparse-keymap))
(nu-define-prefix 'nu-do-map)
(define-key nu-do-map "a" 'avy-goto-char)
(define-key nu-do-map "g" 'nu-global-prompt)
(define-key nu-do-map "h" 'nu-find-files)
(define-key nu-do-map "i" 'ibuffer)
(define-key nu-do-map "k" 'kill-buffer-and-window)
(define-key nu-do-map "l" 'nu-buffers-list)
(define-key nu-do-map "m" 'delete-other-windows)
(define-key nu-do-map "n" 'nu-trigger-mode-specific-map)
(define-key nu-do-map "o" 'avy-goto-word-1)
(define-key nu-do-map "p" 'nu-M-x)
(define-key nu-do-map "r" 'nu-recentf)
(define-key nu-do-map "s" 'avy-goto-symbol-1)
(define-key nu-do-map "x" 'avy-goto-line)

(defun nu-do-prompt ()
  (interactive)
  (nu-prompt-for-keymap nu-do-map))

(provide 'nu-menus)
