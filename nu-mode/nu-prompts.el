
;;
;;   Pierre-Yves Luyten
;;   2014
;;
;;
;;   This file is part of Nu.
;;
;;   Nu is free software: you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation, either version 3 of the License, or
;;   (at your option) any later version.
;;
;;   Nu is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with Nu.  If not, see <http://www.gnu.org/licenses/>.
;;
;;
;;


;;
;; prompts : first define keys from "common case"
;; that might be shallowed by other modes
;;


(require 'windmove)
(require 'nu-help)

(defvar nu-print-map)
(defvar nu-delete-map)
(defvar nu-insert-map)
(defvar nu-save-map)
(defvar nu-open-map)
(defvar nu-goto-map)
(defvar nu-replace-map)
(defvar nu-window-map)
(defvar nu-new-map)
(defvar nu-a-map)
(defvar nu-find-map)
(defvar nu-copy-map)


 (autoload 'zap-up-to-char "misc"
"Kill up to, but not including ARGth occurrence of CHAR." t)

(nu-define-prefix 'nu-window-map)
(define-key nu-window-map (kbd "x") 'nu-close-document)
(define-key nu-window-map (kbd "M-k") 'kill-buffer)
(define-key nu-window-map (kbd "M-w") 'delete-window)
(define-key nu-window-map (kbd "w") 'delete-other-windows)
(define-key nu-window-map (kbd "M-<SPC>") 'scroll-other-window)
(define-key nu-window-map (kbd "M-<backspace>") 'scroll-other-window-down)
(define-key nu-window-map (kbd "f") 'transpose-frame)
(define-key nu-window-map (kbd "n")   'nu-next-window)
(define-key nu-window-map (kbd "p")   'nu-previous-window)
(define-key nu-window-map (kbd "Q") 'save-buffers-kill-emacs)

(defun nu-window-prompt ()
  (interactive)
  (nu-prompt-for-keymap nu-window-map))



(defun nu-populate-print ()
  (nu-define-prefix 'nu-print-map)

  (define-key nu-print-map (kbd "C-p") 'print-buffer)
  (define-key nu-print-map (kbd "p") 'async-shell-command)
  (define-key nu-print-map (kbd "d") 'ediff)

  (if (or (eq major-mode 'emacs-lisp-mode)
          (eq major-mode 'lisp-interaction-mode))
     (progn
       (define-key nu-print-map (kbd "s") 'eval-last-sexp)
       (define-key nu-print-map (kbd "b") 'eval-current-buffer)
       (define-key nu-print-map (kbd "r") 'eval-region)))


  (if (eq major-mode 'magit-status-mode)
      (progn
         (define-key nu-print-map (kbd "p") 'magit-shell-command)
         (define-key nu-print-map (kbd ":") 'magit-git-command)))

  (if (eq major-mode 'dired-mode)
        (progn
        (define-key nu-print-map (kbd "C-p") 'dired-do-print)
        (define-key nu-print-map (kbd "C-b") 'dired-do-byte-compile)
        (define-key nu-print-map (kbd "p")   'dired-do-async-shell-command)
        (define-key nu-print-map (kbd "P")   'dired-do-shell-command)
        (define-key nu-print-map (kbd "d")   'dired-diff)))

  (if (eq major-mode 'texinfo-mode)
     (progn
       (define-key nu-print-map (kbd "i") 'makeinfo-buffer)
       (define-key nu-print-map (kbd "P") 'nu-texi2pdf)))

  (define-key nu-print-map (kbd "f") 'find-grep)
  (define-key nu-print-map (kbd "g") 'grep)
  (define-key nu-print-map (kbd "w") 'pwd)
  (define-key nu-print-map (kbd "n") 'negative-argument)
  (define-key nu-print-map (kbd "u") 'universal-argument)
  (define-key nu-print-map (kbd "m") 'compile)
  (define-key nu-print-map (kbd "k") 'kmacro-end-or-call-macro)
  (define-key nu-print-map (kbd "c") 'subword-mode))


(defun nu-print-prompt ()
  (interactive)
  (nu-populate-print)
  (nu-prompt-for-keymap nu-print-map))


(defun nu-populate-delete ()
  "Populate nu-delete-map."
  (nu-define-prefix 'nu-delete-map)

  (if (eq major-mode 'magit-status-mode)
      (progn
        (define-key nu-delete-map (kbd "b") 'magit-delete-branch)
        (define-key nu-delete-map (kbd "h") 'magit-discard-item))
      ; else
      (if (eq major-mode 'dired-mode)
         (progn
           (define-key nu-delete-map (kbd "d") 'dired-flag-file-deletion)
           (define-key nu-delete-map (kbd "k") 'dired-do-flagged-delete)
           (define-key nu-delete-map (kbd "o") 'dired-do-delete))
           ; else
            (define-key nu-delete-map (kbd "i") 'nu-delete-above-line)
            (define-key nu-delete-map (kbd "b") 'kill-buffer) ; redundant.
            (define-key nu-delete-map (kbd "j") 'backward-delete-char)
            (define-key nu-delete-map (kbd "M-j") 'nu-backward-kill-line)
            (define-key nu-delete-map (kbd "x") 'kill-whole-line)
            (define-key nu-delete-map (kbd "k") 'nu-delete-below-line)
            (define-key nu-delete-map (kbd "l") 'delete-forward-char)
            (define-key nu-delete-map (kbd "$") 'kill-line)
            (define-key nu-delete-map (kbd "M-l") 'kill-line)
            (define-key nu-delete-map (kbd "u") 'backward-kill-word)
            (define-key nu-delete-map (kbd "o") 'kill-word)
            (define-key nu-delete-map (kbd "M-o") 'nu-kill-block)
            (define-key nu-delete-map (kbd "M-u") 'nu-backward-kill-block)
            (define-key nu-delete-map (kbd "h") 'delete-horizontal-space)
            (define-key nu-delete-map (kbd "t") 'delete-trailing-whitespace)
            (define-key nu-delete-map (kbd "b") 'delete-blank-lines)
            (define-key nu-delete-map (kbd "s") 'kill-sexp)
            (define-key nu-delete-map (kbd "e") 'kill-sentence)
            (define-key nu-delete-map (kbd "f") 'nu-delete-defun)
            (define-key nu-delete-map (kbd "z")  'zap-up-to-char)
            (define-key nu-delete-map (kbd "a") 'nu-delete-all)))

  ; common cases
  (define-key nu-delete-map (kbd "M-f") 'delete-file)

  ; these ones are additional...
  (if (equal major-mode 'org-mode)
      (progn
        (define-key nu-delete-map (kbd "!") 'org-table-delete-column)
        (define-key nu-delete-map (kbd "r") 'org-table-kill-row)
        (define-key nu-delete-map (kbd "*") 'org-cut-special)
        (define-key nu-delete-map (kbd "M-k") 'org-cut-subtree))))

(defun nu-delete-prompt-internal ()
  (interactive)
  (nu-populate-delete)
  (nu-prompt-for-keymap nu-delete-map))



(defun nu-delete-prompt ()
  (interactive)
  (if mark-active
    (call-interactively 'kill-region)
    (progn
      (nu-delete-prompt-internal)
      (help-make-xrefs (help-buffer)))))


(defvar nu-bold-map)

(defun nu-populate-bold-map ()
 "Populate bold map."
  (nu-define-prefix 'nu-bold-map)
  (define-key nu-bold-map (kbd "f") 'fill-paragraph)
  (define-key nu-bold-map (kbd "i") 'indent)
  (define-key nu-bold-map (kbd "a") 'align)
  (if (or (eq major-mode 'c-mode)
          (eq major-mode 'lisp-interaction-mode)
          (eq major-mode 'emacs-lisp-mode))
      (progn
          (define-key nu-bold-map (kbd "c") 'comment-or-uncomment-region)
          (define-key nu-bold-map (kbd "l") 'comment-indent))))

(defun nu-bold-prompt ()
  (interactive)
  (nu-populate-bold-map)
  (nu-prompt-for-keymap nu-bold-map))


(defun nu-populate-insert-map ()
 "Populate insert map."
  (nu-define-prefix 'nu-insert-map)

  (if (eq major-mode 'dired-mode)
        (progn
           (define-key nu-insert-map (kbd "v") 'dired-maybe-insert-subdir)
           (define-key nu-insert-map (kbd "M-v") 'dired-create-directory))
         ; else
        (define-key nu-insert-map (kbd "V") 'nu-yank-pop-or-yank) ; absurd.
        (define-key nu-insert-map (kbd "v") 'yank)
        (define-key nu-insert-map (kbd "b") 'insert-buffer)
        (define-key nu-insert-map (kbd "f") 'insert-file)
        (define-key nu-insert-map (kbd "c") 'quoted-insert)
        (define-key nu-insert-map (kbd "k") 'nu-insert-line-below)
        (define-key nu-insert-map (kbd "i") 'nu-insert-line-above)

        ; addon
        (if (eq major-mode 'org-mode)
          (progn
            (define-key nu-insert-map (kbd "L") 'org-insert-link)
            (define-key nu-insert-map (kbd "o") 'org-table-insert-column)
            (define-key nu-insert-map (kbd "O") 'org-table-insert-row)
            (define-key nu-insert-map (kbd "M-s") 'org-paste-subtree)
            (define-key nu-insert-map (kbd "M-o") 'org-paste-special)
            (define-key nu-insert-map (kbd "m") 'org-time-stamp)
            (define-key nu-insert-map (kbd "t") 'org-insert-todo-heading))))

  ; anycase
  (define-key nu-insert-map (kbd "s") 'async-shell-command)
  (define-key nu-insert-map (kbd "S") 'shell-command)
  (define-key nu-insert-map (kbd "h")  'helm-show-kill-ring))


(defun nu-insert-prompt ()
  (interactive)
  (nu-populate-insert-map)
  (nu-prompt-for-keymap nu-insert-map))



(defun nu-populate-save-map ()
 "Populate save map."
  (nu-define-prefix 'nu-save-map)
  (define-key nu-save-map (kbd "s") 'save-buffer)
  (define-key nu-save-map (kbd "g") 'nu-toggle-goal-column)
  (define-key nu-save-map (kbd "b") 'bookmark-set)
  (define-key nu-save-map (kbd "w") 'ido-write-file)
  (define-key nu-save-map (kbd "r") 'rename-buffer)
  (define-key nu-save-map (kbd "L") 'org-store-link)
  (if (eq major-mode 'org-mode)
      (define-key nu-save-map (kbd "o") 'org-refile))
  (define-key nu-save-map (kbd "k") 'kmacro-start-macro-or-insert-counter)
  (define-key nu-save-map (kbd "f") 'nu-create-tags)

  (if (eq major-mode 'magit-status-mode)
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
  (nu-populate-save-map)
  (nu-prompt-for-keymap nu-save-map))


(defun nu-populate-new-map ()
  (nu-define-prefix 'nu-new-map)

  (if (eq major-mode 'dired-mode)
      ; switch what does d according to mode
      (define-key nu-new-map (kbd "d") 'dired-create-directory)
      (define-key nu-new-map (kbd "d") 'make-directory))

  (if (eq major-mode 'magit-status-mode)
      (define-key nu-new-map (kbd "b") 'magit-create-branch)
      (define-key nu-new-map (kbd "a") 'magit-annotated-tag))

  (define-key nu-new-map (kbd "n") 'nu-new-empty-buffer)
  (define-key nu-new-map (kbd "C-n") 'helm-run-external-command)
  (define-key nu-new-map (kbd "m") 'compose-mail)
  (define-key nu-new-map (kbd "w") 'make-frame-command)
  (define-key nu-new-map (kbd "v") 'split-window-below)
  (define-key nu-new-map (kbd "h") 'split-window-right)
  (define-key nu-new-map (kbd "t") 'term)
  (define-key nu-new-map (kbd "s") 'eshell)
  (define-key nu-new-map (kbd "i") 'ibuffer-other-window)

  (define-key nu-new-map (kbd "o")   'org-capture))


(defun nu-new-prompt ()
  (interactive)
  (nu-populate-new-map)
  (nu-prompt-for-keymap nu-new-map))


(defun nu-populate-a-map ()
  (nu-define-prefix 'nu-a-map)
  (define-key nu-a-map (kbd "C-f") 'cd)

  (if (eq major-mode 'proced)
      (progn
	    (define-key nu-a-map (kbd "a") 'proced-mark-all)
	    (define-key nu-a-map (kbd "C-a") 'proced-unmark-all)
	    (define-key nu-a-map (kbd "c") 'proced-mark-children)
	    (define-key nu-a-map (kbd "p") 'proced-mark-parents)
	    (define-key nu-a-map (kbd "l") 'proced-mark)
	    (define-key nu-a-map (kbd "t") 'proced-toggle-marks)
	    (define-key nu-a-map (kbd "u") 'proced-unmark))
  ; else if...
  (if (eq major-mode 'dired-mode)
      (progn
        (define-key nu-a-map (kbd "d") 'dired-flag-file-deletion)
        (define-key nu-a-map (kbd "r") 'dired-flag-files-regexp)
        (define-key nu-a-map (kbd "m") 'dired-mark)
        (define-key nu-a-map (kbd "u") 'dired-unmark)
        (define-key nu-a-map (kbd "s") 'nu-mark-subdirs-files)
        (define-key nu-a-map (kbd "x") 'dired-toggle-marks)
        (define-key nu-a-map (kbd "C-u") 'dired-unmark-all-marks)
        (define-key nu-a-map (kbd "r") 'dired-mark-files-regexp)
        (define-key nu-a-map (kbd "C-r") 'dired-mark-files-containing-regexp))
   ; else...
      (define-key nu-a-map (kbd "a") 'nu-mark-whole-buffer)
      (define-key nu-a-map (kbd "f") 'nu-mark-defun)
      (define-key nu-a-map (kbd "s") 'nu-mark-sentence)
      (define-key nu-a-map (kbd "w") '_nu-mark-a-word)
      (define-key nu-a-map (kbd "C-w") '_nu-select-a-block)
      (define-key nu-a-map (kbd "p") 'nu-mark-paragraph)
      (define-key nu-a-map (kbd "j") 'nu-mark-to-bol)
      (define-key nu-a-map (kbd "l") 'nu-mark-to-eol)
      (define-key nu-a-map (kbd "k") '_nu-mark-current-line)
      (define-key nu-a-map (kbd "C-<SPC>") 'nu-set-mark)
      (define-key nu-a-map (kbd "r") 'nu-set-rectangle-mark))))



(defun nu-a-prompt ()
  "Triggers nu-a-map.

But if mark is active, exchange point and mark."
  (interactive)
     (if mark-active
      (exchange-point-and-mark)
      (nu-populate-a-map)
      (nu-prompt-for-keymap nu-a-map)))




(defun nu-populate-open-map ()
"Populate open map."
  (setq nu-open-map nil)
  (nu-define-prefix 'nu-open-map)

  (if (eq major-mode 'magit-status-mode)
      (progn
        (define-key nu-open-map (kbd "g") 'magit-log-long)
        (define-key nu-open-map (kbd "C-b") 'magit-branch-manager)))

  (if (eq major-mode 'dired-mode)
      (progn
        (define-key nu-open-map (kbd "d") 'dired-find-file)
        (define-key nu-open-map (kbd "C-d") 'dired-find-file-other-window)))

  (if (eq major-mode 'org-mode)
      (progn
         (define-key nu-open-map (kbd "L") 'org-open-at-point)))

  (define-key nu-open-map (kbd "f")  'helm-find-files)
  (define-key nu-open-map (kbd "o")  'helm-mini)
  (define-key nu-open-map (kbd "C-f")  'find-file-other-window) ; useless now that helm fixes this stuff =)
  (define-key nu-open-map (kbd "r")  'helm-recentf)
  (define-key nu-open-map (kbd "b")  'helm-bookmarks)
  (define-key nu-open-map (kbd "B")  'bookmark-jump)
  (define-key nu-open-map (kbd "x")  'list-registers)
  (define-key nu-open-map (kbd "l")  'nu-next-buffer)
  (define-key nu-open-map (kbd "j")   'nu-previous-buffer)
  (define-key nu-open-map (kbd "a")   'org-agenda)
  (define-key nu-open-map (kbd "m")   'menu-bar-read-mail)
  (define-key nu-open-map (kbd "i")   'helm-buffers-list)
  (define-key nu-open-map (kbd "C-i")   'ibuffer) ; is better at _reorganizing_ buffers...
  (define-key nu-open-map (kbd "I") 'org-iswitchb)
  (define-key nu-open-map (kbd "C-o") 'ido-switch-buffer))



(defun nu-open-prompt ()
"Maybe temporary prompt."
  (interactive)
  (nu-populate-open-map)
  (nu-prompt-for-keymap nu-open-map))




(defun nu-populate-goto-map ()
"Populate goto map."
  (setq nu-goto-map nil)
  (nu-define-prefix 'nu-goto-map)

  ;; actually this case is : all read only modes...
  (if (eq major-mode 'dired-mode)
      (progn
        ; i should go parent dir. k should try to persistent-action subdir.
        (define-key nu-goto-map (kbd "u") 'dired-prev-marked-file)
        (define-key nu-goto-map (kbd "o") 'dired-next-marked-file))
      ; else
      (define-key nu-goto-map (kbd "l") 'forward-sentence)
      (define-key nu-goto-map (kbd "j") 'backward-sentence)
      (define-key nu-goto-map (kbd "u") 'backward-paragraph)
      (define-key nu-goto-map (kbd "o") 'forward-paragraph)
      (define-key nu-goto-map (kbd "f") 'end-of-defun)
      (define-key nu-goto-map (kbd "M-f") 'beginning-of-defun)
      (define-key nu-goto-map (kbd "*") 'forward-list)
      (define-key nu-goto-map (kbd "M-*") 'backward-list))


      (define-key nu-goto-map (kbd "M-i") 'windmove-up)
      (define-key nu-goto-map (kbd "M-j") 'windmove-left)
      (define-key nu-goto-map (kbd "M-k") 'windmove-down)
      (define-key nu-goto-map (kbd "M-l") 'windmove-right)

  ; common keys
  (define-key nu-goto-map (kbd "i") 'beginning-of-buffer)
  (define-key nu-goto-map (kbd "k") 'forward-line)
  (define-key nu-goto-map (kbd "k") 'end-of-buffer)
  (define-key nu-goto-map (kbd "e") 'next-error)
  (define-key nu-goto-map (kbd "M-e") 'previous-error)
  (define-key nu-goto-map (kbd "g") 'goto-line)
  (define-key nu-goto-map (kbd "M-g") 'nu-goto-line-previousbuffer)
  (define-key nu-goto-map (kbd "s") 'nu-find-previous-mark)
  (define-key nu-goto-map (kbd "M-s") 'org-mark-ring-goto))


(defun nu-goto-prompt ()
"Offer to goto wherever wished."
  (interactive)
  (nu-populate-goto-map)
  (nu-prompt-for-keymap nu-goto-map))


(defun nu-global-prompt ()
"Offer x keymap. Temporary function."
  (interactive)
  (nu-prompt-for-keymap ctl-x-map))



; we do use native help-map.
; this is an exception : we do not want
; to print the _entire_ help map.
; still, we use this.
;
; this is why a specific prompt is there.
;(make-help-screen nu-help-prompt
;(purecopy "Help")
;"Press q to quit or :
; 
;h: emacs-nu help page
;r: emacs manual
;i: info
;f: describe-function         d: search in documentation
;k: describe-key              m: describe-mode
;v: describe-variable"
;help-map)
(defun nu-help-prompt ()
 (interactive)
 (define-key help-map (kbd "*") 'nu-help)
 ;(define-key help-map (kbd "h") 'nu-help)
 (nu-prompt-for-keymap help-map))


(defun nu-populate-find-map ()
  (nu-define-prefix 'nu-find-map)

  (if (eq major-mode 'dired-mode)
      (progn
        (define-key nu-find-map (kbd "f") 'dired-mark-files-containing-regexp))
      (define-key nu-find-map (kbd "f") 'ace-jump-char-mode))

  ; common keys
  (define-key nu-find-map (kbd "F") 'nu-isearch-forward)
  (define-key nu-find-map (kbd "R") 'nu-isearch-backward)
  (define-key nu-find-map (kbd "M-f") 'nu-isearch-forward-regexp)
  (define-key nu-find-map (kbd "M-F") 'search-forward-regexp)
  (define-key nu-find-map (kbd "r") 'nu-isearch-backward-regexp)
  (define-key nu-find-map (kbd "M-R") 'search-backward-regexp)
  (define-key nu-find-map (kbd "b") 'regexp-builder)
  (define-key nu-find-map (kbd "o") 'occur)
  (define-key nu-find-map (kbd "g") 'rgrep)
  (define-key nu-find-map (kbd "m") 'helm-imenu)
  (define-key nu-find-map (kbd "l") 'ace-jump-line-mode)
  (define-key nu-find-map (kbd "w") 'ace-jump-word-mode)
  (define-key nu-find-map (kbd "z") 'nu-find-char)
  (define-key nu-find-map (kbd "t") 'find-tag)
  (define-key nu-find-map (kbd "M-z") 'nu-find-char-backward))

(defun nu-find-prompt ()
  (interactive)
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
 (if (eq major-mode 'dired-mode)
     (progn
       (define-key nu-copy-map (kbd "c") 'dired-do-copy)
       (define-key nu-copy-map (kbd "C-c") 'dired-copy-filename-as-kill)
       (define-key nu-copy-map (kbd "h") 'dired-do-hardlink)
       (define-key nu-copy-map (kbd "s") 'dired-do-symlink))))

(defun nu-copy-prompt ()
 (interactive)
 (nu-populate-copy-map)
 (nu-prompt-for-keymap nu-copy-map))


(defalias 'toggle-sorting-by-date-or-name 'dired-sort-toggle-or-edit)

(defun nu-populate-replace-dired ()
  (setq nu-replace-map nil)
  (nu-define-prefix 'nu-replace-map)
  (define-key nu-replace-map (kbd "r") 'dired-do-rename)
  (define-key nu-replace-map (kbd "s") 'toggle-sorting-by-date-or-name)
  (define-key nu-replace-map (kbd "z") 'dired-do-compress)
  (define-key nu-replace-map (kbd "u") 'dired-upcase)
  (define-key nu-replace-map (kbd "d") 'dired-downcase)
  (define-key nu-replace-map (kbd "w") 'wdired-change-to-wdired-mode))


(defalias 'git-checkout-item 'magit-discard-item)
(defalias 'git-pull-rebase 'magit-rebase-step)
(defalias 'git-toggle-amend-next-commit 'magit-log-edit-toggle-amending)


(defun nu-populate-replace-magit ()
  (setq nu-replace-map nil)
  (nu-define-prefix 'nu-replace-map)
  (define-key nu-replace-map (kbd "k") 'git-checkout-item)
  (define-key nu-replace-map (kbd "a") 'git-toggle-amend-next-commit)
  (define-key nu-replace-map (kbd "b") 'magit-move-branch)
  (define-key nu-replace-map (kbd "r") 'git-pull-rebase))


(defun nu-populate-replace ()
  "Create replace-keymap."
  (setq nu-replace-map nil)
  (nu-define-prefix 'nu-replace-map)

  (define-key nu-replace-map (kbd "m") 'nu-toggle-read-only)
  (define-key nu-replace-map (kbd "M-r")  'query-replace-regexp)
  (define-key nu-replace-map (kbd "a")  'revert-buffer)
  (define-key nu-replace-map (kbd "R")  'query-replace)
  (define-key nu-replace-map (kbd "k")  'overwrite-mode)
  (define-key nu-replace-map (kbd "s")  'replace-string)
  (define-key nu-replace-map (kbd "r")  'replace-regexp)
  (define-key nu-replace-map (kbd "j")  'nu-join-with-following-line)
  (define-key nu-replace-map (kbd "J")  'join-line)
  (define-key nu-replace-map (kbd "t")  'transpose-lines)
  (define-key nu-replace-map (kbd "u") 'upcase-word)
  (define-key nu-replace-map (kbd "d") 'downcase-word)
  (define-key nu-replace-map (kbd "c") 'capitalize-word)
  (define-key nu-replace-map (kbd "x") 'nu-rot-reg-or-toggle-rot)
  (define-key nu-replace-map (kbd "h") 'delete-horizontal-space)

  (if (not (fboundp 'ethan-wspace-untabify))
    (defun ethan-wspace-untabify ()
      (interactive)
      (message "Please install ethan-wspace mode!")))
  (define-key nu-replace-map (kbd "t") 'ethan-wspace-untabify)

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


(defun nu-replace-prompt ()
  (interactive)
      (cond
        ((eq major-mode 'dired-mode)
         (nu-populate-replace-dired)
         (nu-prompt-for-keymap nu-replace-map))
        ((eq major-mode 'magit-status-mode)
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


(provide 'nu-prompts)
