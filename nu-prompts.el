;;
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

;; Get the macro make-help-screen when this is compiled,
;; or run interpreted, but not when the compiled code is loaded.
(eval-when-compile (require 'help-macro))


(defun nu-prompt (&optional title message)
 (interactive)
 (setq curb (current-buffer))
 (unless title (setq title "Enter:"))
 (setq buf (generate-new-buffer title))
 (view-buffer-other-window buf)
 (read-only-mode t)
 (org-mode)
 (funcall (and initial-major-mode))
 (setq message
   (concat "\n    ~~~ ☸ ~~~\n" ; U+2638
            message))
 (insert message)
 (setq x (read-event))
 (quit-window)
 (kill-buffer buf)
 (switch-to-buffer curb)
 (setq x x))


(define-prefix-command 'nu-print-map)
(define-key nu-print-map (kbd "p") 'print-buffer)
(define-key nu-print-map (kbd "s") 'eval-last-sexp)
(define-key nu-print-map (kbd "b") 'eval-buffer)
(define-key nu-print-map (kbd "w") 'pwd)
(define-key nu-print-map (kbd "-") 'negative-argument)
(define-key nu-print-map (kbd "\C-p") 'universal-argument)
(define-key nu-print-map (kbd "c") 'compile)
(make-help-screen nu-print-prompt ; wow notice this sound! =)
(purecopy "Print")
"Control+p : universal argument
 p: Really Print this (hardware)
 s: eval last sexp
 b: eval buffer
 w: pwd
 -: negative argument
 m: make
 c: compile"
nu-print-map)


(define-prefix-command 'nu-delete-map)
(define-key nu-delete-map (kbd "i") 'nu-delete-above-line)
(define-key nu-delete-map (kbd "j") 'backward-delete-char)
(define-key nu-delete-map (kbd "$") 'kill-line)
(define-key nu-delete-map (kbd "x") 'kill-whole-line)
(define-key nu-delete-map (kbd "k") 'nu-delete-below-line)
(define-key nu-delete-map (kbd "l") 'delete-forward-char)
(define-key nu-delete-map (kbd "u") 'backward-kill-word)
(define-key nu-delete-map (kbd "o") 'kill-word)
(define-key nu-delete-map (kbd "h") 'delete-horizontal-space)
(define-key nu-delete-map (kbd "t") 'delete-trailing-whitespace)
(define-key nu-delete-map (kbd "b") 'delete-blank-lines)
(define-key nu-delete-map (kbd "s") 'kill-sexp)
(define-key nu-delete-map (kbd "e") 'kill-sentence)
(define-key nu-delete-map (kbd "f") 'nu-delete-defun)
(define-key nu-delete-map (kbd "a") 'nu-delete-all)
(make-help-screen nu-delete-prompt-internal
(purecopy "Delete")
"=i= above line
 =j= backward-delete-char (C-j)
 =k= delete below line
 =$= kill-line
 =x= kill whole line
 =l= next char (C-l)
 =u= backward kill word (C-u)
 =o= kill word
 =e= kill sentence

 =h= horizontal space
 =t= trailing space
 =w= whole line (C-x)
 =b= blank lines
 =s= function `kill-sexp'
 =f= delete function
 =a= delete whole buffer"
nu-delete-map)
(defun nu-delete-prompt ()
  (interactive)
  (nu-delete-prompt-internal)
  (help-make-xrefs (help-buffer)))


(define-prefix-command 'nu-insert-map)
(define-key nu-insert-map (kbd "v") 'nu-yank-pop-or-yank)
(define-key nu-insert-map (kbd "k") 'yank)
(define-key nu-insert-map (kbd "i") 'browse-kill-ring)
(define-key nu-insert-map (kbd "b") 'insert-buffer)
(define-key nu-insert-map (kbd "f") 'insert-file)
(define-key nu-insert-map (kbd "c") 'quoted-insert)
(define-key nu-insert-map (kbd "o") 'open-line)
(define-key nu-insert-map (kbd "s") 'async-shell-command)
(define-key nu-insert-map (kbd "S") 'shell-command)
(make-help-screen nu-insert-prompt
(purecopy "Insert")
"
 =v= Yank / *pop*   =i= browsekillring
 =k= Yank but *do not*

 =b= Insert buffer       =f= Insert file

 =c= Insert litterally (~quoted insert~)
 =o= open line below

 =s= async-shell-command (=S= for sync)

 Use _alt v_ to yank pop"
nu-insert-map)


(define-prefix-command 'nu-save-map)
(define-key nu-save-map (kbd "s") 'save-buffer)
(define-key nu-save-map (kbd "w") 'ido-write-file)
(define-key nu-save-map (kbd "r") 'rename-buffer)
(define-key nu-save-map (kbd "m") 'magit-status)
(make-help-screen nu-save-prompt
(purecopy "Save")
"(Use Alt+s to directly save a buffer.)
Press q to quit or :

s: save
w: save-as
r: rename buffer
m: magit-status"
nu-save-map)


(defun nu-all-prompt ()
  "Prompt to select several chars (func, word, buffer...)

" ; if region is selected, toggle....
   (interactive)
; below does not work
;   (if (and (transient-mark-mode mark-active))
   (setq c (nu-prompt "All"
     "
 Once mark is set, C-a to exchange point & mark.


_space_ set mark         _return_ set rectangle

a: select all            f : mark-function
p : mark-paragraph       l : mark to end of line
s : mark sentence        j : mark to beginning of l
w : mark-word            k : mark current line
"))
   (cond
   ;; curiously, if we mark-whole-buffer right now, this fails
   ;; using a timer works. ?uh?
   ((eq c ?a)
    (run-with-timer 0.001 nil  'mark-whole-buffer))
   ((eq c ?f)
     (run-with-timer 0.001 nil 'mark-defun))
   ((eq c ?s)
     (run-with-timer 0.001 nil 'nu-mark-sentence))
   ((eq c ?w)
     (run-with-timer 0.001 nil 'mark-word))
   ((eq c ?p)
     (run-with-timer 0.001 nil 'mark-paragraph))
   ((eq c ?j)
     (run-with-timer 0.001 nil 'nu-mark-to-beginning-of-line))
   ((eq c ?l)
     (run-with-timer 0.001 nil 'nu-mark-to-end-of-line))
   ((eq c ?k)
     (run-with-timer 0.001 nil 'nu-mark-current-line))
   ; <SPC>
   ((eq c ?\s)
     (run-with-timer 0.001 nil 'cua-set-mark))
   ; <RET> <C-m>
   ((eq c ?\r)
     (call-interactively 'cua-set-rectangle-mark))
   (t
    (keyboard-quit)))
;  ; There is a region. Toggle
;   ((progn (exchange-point-and-mark) (message "toto")
)


(defun nu-open-prompt ()
   "Open"
  (interactive)
  (setq c (nu-prompt "Open..."
    "
  =f= open file/dir         =l= next-buffer
  =F= file other window     =j= previous-buffer
  =r= recent files          =space= ido-switch-buffer
  =o= other-window (next)
  =O= other-window (prev.)  =i= ibuffer
                            =I= ibuffer-other-window
  =x= registers

  =m= bookmarks menu, =M= jump to bookmark
  =b= bookmark set"))
  (cond
    ((eq c ?f)
     (call-interactively 'find-file))
    ((eq c ?F)
      (call-interactively 'find-file-other-window))
    ((eq c ?r)
     (call-interactively 'recentf-open-files))
    ((eq c ?m)
     (call-interactively 'bookmark-bmenu-list))
    ((eq c ?M)
     (call-interactively 'bookmark-jump))
    ((eq c ?b)
     (call-interactively 'bookmark-set))
    ((eq c ?x)
     (list-registers))
    ((eq c ?l)
      (next-buffer))
    ((eq c ?j)
      (previous-buffer))
    ((eq c ?o)
     (other-window 1))
    ((eq c ?O)
     (other-window -l1))
    ((eq c ?i)
     (ibuffer))
    ((eq c ?I)
     (ibuffer-other-window))
    ((eq c ?\s)
     (ido-switch-buffer))
    (t
     (keyboard-quit))))

(define-prefix-command 'nu-global-map)
(defun nu-no-goal-column () (interactive) (setq goal-column nil) (message "No goal column"))
(defun nu-set-x-4-map () (interactive) (set-temporary-overlay-map ctl-x-4-map))
(defun nu-set-x-5-map () (interactive) (set-temporary-overlay-map ctl-x-5-map))
(define-key nu-global-map (kbd "a") 'async-shell-command)
(define-key nu-global-map (kbd "à") 'delete-other-windows)
(define-key nu-global-map (kbd "1") 'delete-other-windows)
(define-key nu-global-map (kbd "&") 'delete-other-windows)
(define-key nu-global-map (kbd "2") 'split-window-below)
(define-key nu-global-map (kbd "é") 'split-window-below)
(define-key nu-global-map (kbd "3") 'split-window-right)
(define-key nu-global-map (kbd "\"") 'split-window-right)
(define-key nu-global-map (kbd "4")  'nu-set-x-4-map)
(define-key nu-global-map (kbd "'") 'nu-set-x-4-map)
(define-key nu-global-map (kbd "5") 'nu-set-x-5-map)
(define-key nu-global-map (kbd "(") 'nu-set-x-5-map)
(define-key nu-global-map (kbd "g") 'set-goal-column)
(define-key nu-global-map (kbd "G") 'nu-no-goal-column)
(define-key nu-global-map (kbd "\C-q") 'save-buffers-kill-emacs)
(define-key nu-global-map (kbd "t") 'transpose-frame)
(define-key nu-global-map (kbd "x") 'Control-X-prefix)
(make-help-screen nu-global-prompt
(purecopy "GLOBAL")
"
 a: async-shell-command     g:    goal column
 t: transpose-frame         G: rm goal column

 0 or à Close             50 Close frame
 1 or & This window only
 2 or é Hsplit	          52 New frame
 3 or \" VSplit
 4 or ' xxx

 x: Emacs standard Control-X keymap
 Control-q: quit emacs

<!> if you wanted C-g to keyboard-quit, use C-q <!>"
nu-global-map)


(define-key help-map (kbd "h") 'nu-help)

(make-help-screen nu-help-prompt
(purecopy "Help")
"Press q to quit or :

h: emacs-nu help page
r: emacs manual
i: info
f: describe-function         d: search in documentation
k: describe-key              m: describe-mode
v: describe-variable"
help-map)

(define-prefix-command 'nu-find-map)
(defun nu-isearch-forward ()
  (interactive)
  (if mark-active
      (progn
	(call-interactively 'isearch-forward)
	(isearch-yank-string (buffer-substring-no-properties (region-beginning) (region-end))))
      (isearch-forward)))
(defun nu-isearch-backward ()
  (interactive)
    (if mark-active
      (progn
	(call-interactively 'isearch-backward)
	(isearch-yank-string (buffer-substring-no-properties (region-beginning) (region-end))))
    (isearch-backward)))
(defun nu-isearch-forward-regexp ()
  (interactive)
    (if mark-active
      (progn
	(call-interactively 'isearch-forward-regexp)
	(isearch-yank-string (buffer-substring-no-properties (region-beginning) (region-end))))
    (isearch-forward-regexp)))
(defun nu-isearch-backward-regexp ()
  (interactive)
    (if mark-active
      (progn
	(call-interactively 'isearch-backward-regexp)
	(isearch-yank-string (buffer-substring-no-properties (region-beginning) (region-end))))
    (isearch-backward-regexp)))
(defun nu-set-mark-1 () (interactive) (cua-set-mark 1))
(defun nu-goto-line-previousbuffer () (interactive) (goto-line (previous-buffer)))
(define-key nu-find-map (kbd "F") 'nu-isearch-forward)
(define-key nu-find-map (kbd "R") 'nu-isearch-backward)
(define-key nu-find-map (kbd "f") 'nu-isearch-forward-regexp)
(define-key nu-find-map (kbd "r") 'nu-isearch-backward-regexp)
(define-key nu-find-map (kbd "i") 'beginning-of-buffer)
(define-key nu-find-map (kbd "k") 'end-of-buffer)
(define-key nu-find-map (kbd "b") 'regexp-builder)
(define-key nu-find-map (kbd "s") 'nu-set-mark-1)
(define-key nu-find-map (kbd "l") 'ace-jump-line-mode)
(define-key nu-find-map (kbd "c") 'ace-jump-char-mode)
(define-key nu-find-map (kbd "w") 'ace-jump-word-mode)
(define-key nu-find-map (kbd "z") 'nu-find-char)
(define-key nu-find-map (kbd "g") 'goto-line)
(define-key nu-find-map (kbd "G") 'nu-goto-line-previousbuffer)
(make-help-screen nu-find-prompt
(purecopy "Find")
"<!> if you wanted to forward char, use M-l <!>

f: isearch-forward-regexp    r: isearch-backward-regexp
F: isearch-forward	     R: isearch-backward
			     b: regexp-builder
l: ace-jump-line-mode
c: ace-jump-char-mode        z: nu-find-char (zap...)
w: ace-jump-word-mode
                             s: goto previous selection
i: beginning-of-buffer       g: goto line
k: end-of-buffer             G: goto line (previous-buffer)
"
nu-find-map)


(defun nu-replace-do-prompt ()
  (interactive)
  (setq c (nu-prompt "Search"
   "
    r: query-replace-regexp        j: join-line (following)
    R: query-replace               J: join-line (previous)
    I: replace-string
    i: replace-regexp              t: transpose-lines

    k: overwrite-mode              u: UPCASE-WORD
                                   d: downcase-word
    z: zap-to-char                 c: Capitalize-Word
    h: delete-horizontal-space

    a: revert buffer               x: rot13-region (if region)"))
  (cond
   ((eq c ?r)
    (call-interactively 'query-replace-regexp))
   ((eq c ?a)
    (revert-buffer))
   ((eq c ?R)
    (call-interactively 'query-replace))
   ((eq c ?k)
    (call-interactively 'overwrite-mode))
   ((eq c ?I)
    (call-interactively 'replace-string))
   ((eq c ?i)
    (call-interactively 'replace-regexp))
   ((eq c ?j)
    (join-line 1))
   ((eq c ?J)
    (join-line))
   ((eq c ?t)
    (call-interactively 'transpose-lines))
   ((eq c ?z)
    (call-interactively 'zap-to-char))
   ((eq c ?u)
    (call-interactively 'upcase-word))
   ((eq c ?d)
    (call-interactively 'downcase-word))
   ((eq c ?c)
    (call-interactively 'capitalize-word))
   ((eq c ?x)
    (when mark-active
    (call-interactively 'rot13-region)))
   ((eq c ?h)
    (delete-horizontal-space))
   (t
    (keyboard-quit))))


(defun nu-replace-prompt ()
  (interactive)
  (if (or (eq overwrite-mode 'overwrite-mode-textual)
	  (eq overwrite-mode 'overwrite-mode-binary))
    (overwrite-mode -1)
    (nu-replace-do-prompt)))


(provide 'nu-prompts)
