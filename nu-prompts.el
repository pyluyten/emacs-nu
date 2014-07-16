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




(defun nu-prompt-for-keymap (keymap prompt)
 (setq curb (current-buffer))
 (with-help-window (help-buffer)
   (prin1 prompt)
   (set-temporary-overlay-map keymap)))


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


(defun nu-delete-prompt ()
  "Delete some <movement>"
  (interactive)
  (setq c (nu-prompt "Delete..."
"=i= above line
 =j= previous char (C-j)
 =k= current line (C-k)
 =l= next char (C-l)
 =u= backward kill word (C-u)
 =o= kill word

 =h= horizontal space
 =t= trailing space
 =w= whole line (C-x)
 =b= blank lines
 =s= kill sexp
 =f= delete function
 =a= delete whole buffer"))
  (cond
  ((eq c ?i) (call-interactively 'nu-delete-above-line))
  ((eq c ?j) (call-interactively 'backward-delete-char))
  ((eq c ?k) (call-interactively 'kill-line))
  ((eq c ?l) (call-interactively 'delete-forward-char))
  ((eq c ?u) (call-interactively 'backward-kill-word))
  ((eq c ?o) (call-interactively 'kill-word))
  ((eq c ?h) (call-interactively 'delete-horizontal-space))
  ((eq c ?t) (call-interactively 'delete-trailing-whitespace))
  ((eq c ?b) (call-interactively 'delete-blank-lines))
  ((eq c ?s) (call-interactively 'kill-sexp))
  ((eq c ?f) (call-interactively 'nu-delete-defun))
  ((eq c ?a) (call-interactively 'nu-delete-all))
  (t (keyboard-quit))))


(defun nu-insert-prompt ()
  "Paste / Insert stuff"
  (interactive)
  (setq c (nu-prompt "Insert"
"
 =v= Yank / *pop*   =i= browsekillring
 =k= Yank but *do not*

 =b= Insert buffer       =f= Insert file

 =c= Insert litterally (~quoted insert~)
 =o= open line below

 =s= async-shell-command (=S= for sync)

 Use _alt v_ to yank pop"))
  (cond
  ((eq c ?v) (nu-yank-pop-or-yank))
  ((eq c ?k) (yank))
  ((eq c ?i) (call-interactively 'browse-kill-ring))
  ((eq c ?b) (call-interactively 'insert-buffer))
  ((eq c ?f) (call-interactively 'insert-file))
  ((eq c ?c) (call-interactively 'quoted-insert))
  ((eq c ?o) (call-interactively 'open-line))
  ((eq c ?s) (call-interactively 'async-shell-command))
  ((eq c ?S) (call-interactively 'shell-command))
  (t (keyboard-quit))))


(defun nu-save-prompt ()
  "Save / Rename stuff"
  (interactive)
  (setq c (nu-prompt "Save"
"
 =s= Save        =r= Rename buffer
 =w= Save as     =m= Magit status



 Use _Alt-s_ to save without prompt"))
  (cond
  ((eq c ?s) (save-buffer))
  ((eq c ?w) (ido-write-file))
  ((eq c ?r) (call-interactively 'rename-buffer))
  ((eq c ?m) (call-interactively 'magit-status))
  (t (keyboard-quit))))


(defun nu-all-prompt ()
  "Use functions on _all_."
   (interactive)
   (setq c (nu-prompt "All"
     "
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
   ((eq c ?space)
     (run-with-timer 0.001 nil 'cua-set-mark))
   ((eq c ?ret)
     (run-with-timer 0.001 nil 'cua-set-mark))
   (t
    (keyboard-quit))))


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


(defun nu-global-prompt ()
  "Access global functions."
  (interactive)
  (setq c (nu-prompt "Global"
     "
 a: async-shell-command     g:    goal column
 t: transpose-frame         G: rm goal column

 0 or à Close             50 Close frame
 1 or & This window only
 2 or é Hsplit	          52 New frame
 3 or \" VSplit
 4 or ' xxx

 x: Emacs standard Control-X keymap
 Q: quit emacs

<!> if you wanted C-g to keyboard-quit, use C-q <!>"))
  (cond
   ((eq c ?a)
     (call-interactively 'async-shell-command))
   ((eq c ?à)
    (delete-other-windows))
   ((eq c ?1)
    (delete-other-windows))
   ((eq c ?&)
    (delete-other-windows))
   ((eq c ?2)
     (split-window-below))
   ((eq c ?é)
     (split-window-below))
   ((eq c ?3)
     (split-window-right))
   ((eq c 34) ; "
     (split-window-right))
   ((eq c ?4)
     (set-temporary-overlay-map ctl-x-4-map))
   ((eq c ?')
    (set-temporary-overlay-map ctl-x-4-map))
   ((eq c ?5)
     (set-temporary-overlay-map ctl-x-5-map))
   ((eq c 40) ; (
     (set-temporary-overlay-map ctl-x-5-map))
   ((eq c ?g)
    (call-interactively 'set-goal-column))
   ((eq c ?G)
     (progn (setq goal-column nil) (message "No goal column.")))
    ((eq c ?Q)
    (save-buffers-kill-emacs))
   ((eq c ?t)
    (transpose-frame))
   ((eq c ?x)
    (set-temporary-overlay-map ctl-x-map))
   (t
    (keyboard-quit))))


(defun nu-help-prompt ()
  "Find some documenation"
  (interactive)
  (nu-prompt-for-keymap help-map
   "
   h: emacs-nu help page
   r: emacs manual
   i: info

   f: describe-function         d: search in documentation
   k: describe-key              m: describe-mode
   v: describe-variable"))

(defun nu-find-prompt ()
  (interactive)
  (setq c (nu-prompt "Search"
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
"))
  (cond
   ((eq c ?F)
    (if mark-active
      (progn
	(call-interactively 'isearch-forward)
	(isearch-yank-string (buffer-substring-no-properties (region-beginning) (region-end))))
      (isearch-forward)))
   ((eq c ?R)
    (isearch-backward))
   ((eq c ?f)
    (if mark-active
      (progn
	(call-interactively 'isearch-forward-regexp)
	(isearch-yank-string (buffer-substring-no-properties (region-beginning) (region-end))))
    (isearch-forward-regexp)))
   ((eq c ?r)
    (if mark-active
      (progn
	(call-interactively 'isearch-backward-regexp)
	(isearch-yank-string (buffer-substring-no-properties (region-beginning) (region-end))))
    (isearch-backward-regexp)))
   ((eq c ?i)
    (beginning-of-buffer))
   ((eq c ?k)
    (end-of-buffer))
   ((eq c ?b)
    (regexp-builder))
   ((eq c ?s)
    (cua-set-mark 1))
   ((eq c ?l)
    (ace-jump-line-mode))
   ((eq c ?c)
    (call-interactively 'ace-jump-char-mode))
   ((eq c ?w)
    (call-interactively 'ace-jump-word-mode))
   ((eq c ?z)
    (call-interactively 'nu-find-char))
;  goto-map variable
;  move-to-column, previous/next error, goto char (?)
   ((eq c ?g)
    (call-interactively 'goto-line))
   ((eq c ?G)
    (call-interactively 'goto-line (previous-buffer)))
   (t
    (keyboard-quit))))



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
