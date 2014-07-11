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

(defun nu-prompt-for-keymap (keym)
 (setq curb (current-buffer))
 (with-help-window (help-buffer)
   (prin1 (symbol-name keym))
   (princ "\n
Now i should lookup for available keys!!!")))



(defun nu-light-prompt-for-keymap (keym)
  ; enter minibuffer
)

(defun nu-prompt (&optional title message)
 (interactive)
 (setq curb (current-buffer))
 (unless title (setq title "Enter:"))
 (setq buf (generate-new-buffer title))
 (view-buffer-other-window buf)
 (read-only-mode t)
 (funcall (and initial-major-mode))
 (setq message
   (concat "\n    ~~~ ☸ ~~~\n" ; U+2638 
            message))
 (insert message)
 (setq x (read-char-exclusive))
 (quit-window)
 (kill-buffer buf)
 (switch-to-buffer curb)
 (setq x x))


(defun nu-all-prompt ()
  "Use functions on _all_."
   (interactive)
   (setq c (nu-prompt "All"
     "
a: select all            f : mark-function
                         s : mark sentence
                         w : mark-word
                         p : mark-paragraph"))
   (cond
   ;; curiously, if we mark-whole-buffer right now, this fails
   ;; using a timer works. ?uh?
   ((eq c ?a)
    (run-with-timer 0.001 nil  'mark-whole-buffer))
   ((eq c ?f)
     (run-with-timer 0.001 nil 'mark-defun))
   ((eq c ?s)
     (call-interactively 'mark-end-of-sentence)) ; ko
   ((eq c ?w)
     (run-with-timer 0.001 nil 'mark-word))
   ((eq c ?p)
     (run-with-timer 0.001 nil 'mark-paragraph))
   (t
    (keyboard-quit))))


(defun nu-open-prompt ()
   "Open"
  (interactive)
  (setq c (nu-prompt "Open..."
    "
  o : open file
  r : recent files
  m : bookmarks menu, M : jump to bookmark
  x : registers"))
  (cond
    ((eq c ?o)
     (call-interactively 'find-file))
    ((eq c ?r)
     (call-interactively 'recentf-open-files))
    ((eq c ?m)
     (call-interactively 'bookmark-bmenu-list))
    ((eq c ?M)
     (call-interactively 'bookmark-jump))
    ((eq c ?x)
     (list-registers))
    (t
     (keyboard-quit))))


(defun nu-global-prompt ()
  "Access global functions."
  (interactive)
  (setq c (nu-prompt "Global"
     "<!> if you wanted C-g to keyboard-quit, use C-q <!> 
==== GOTO ==========     == GLOBAL ===
i: beginning of buffer	 a: async-shell-command
g: goto line (M-g)	 t: transpose-frame
k: end of buffer	      
$: next buffer	         b: bookmark-set
o: other window	   

0 (C-w) = close, 1 or & (Alt-w) = this window only
2  or é = h split,	3  or \ = v split.
50 or à = close this window, 52 or é = new window
x: Emacs standard Control-X keymap
Q: quit emacs				"))
  (cond
   ((eq c ?a)
     (call-interactively 'async-shell-command))
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
   ((eq c ?i)
    (beginning-of-buffer))
   ((eq c ?k)
    (end-of-buffer))
   ((eq c ?g)
    (set-temporary-overlay-map goto-map))
   ((eq c ?$)
    (next-buffer))
   ((eq c ?o)
    (other-window 1))
   ((eq c ?r)
    (revert-buffer))
   ((eq c ?Q)
    (save-buffers-kill-emacs))
   ((eq c ?b)
    (call-interactively 'bookmark-set))
   ((eq c ?j)
    (call-interactively 'bookmark-jump))
   ((eq c ?t)
    (transpose-frame))
   ((eq c ?x)
    (set-temporary-overlay-map ctl-x-map))
   ((eq c ?z)
    (set-temporary-overlay-map goto-map))
   (t
    (keyboard-quit))))


(defun nu-help-prompt ()
  "Find some documenation"
  (interactive)
  (setq c (nu-prompt "Help"
   "
   h: emacs-nu help page
   r: emacs manual

   f: describe-function         d: search in documentation
   k: describe-key              m: describe-mode
   v: describe-variable

   x: toggle help prefix keymap (ie emacs vanilla ctrl h
      You might use Alt-H too, directly,
      rather than Control-h x"))
  (cond
  ((eq c ?h)
    (nu-help))
  ((eq c ?f)
    (call-interactively 'describe-function))
  ((eq c ?d)
   (call-interactively 'apropos-documentation))
  ((eq c ?k)
    (call-interactively 'describe-key))
  ((eq c ?m)
    (describe-mode))
  ((eq c ?r)
    (info-emacs-manual))
  ((eq c ?v)
    (call-interactively 'describe-variable))
  ((eq c ?x)
    (set-temporary-overlay-map help-map))
  (t
   (keyboard-quit))))


(defun nu-find-prompt ()
  (interactive)
  (setq c (nu-prompt "Search"
   "<!> if you wanted to forward char, use M-l <!>

    F: isearch-forward                    R: isearch-backward                   
    f: isearch-forward-regexp             r: isearch-backward-regexp
                                          b: regexp-builder
    l: ace-jump-line-mode
    k: ace-jump-char-mode                 z: nu-find-char (zap...)
    w: ace-jump-word-mode"))
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
   ((eq c ?b)
    (regexp-builder))
   ((eq c ?l)
    (ace-jump-line-mode))
   ((eq c ?k)
    (call-interactively 'ace-jump-char-mode))
   ((eq c ?w)
    (call-interactively 'ace-jump-word-mode))
   ((eq c ?z)
    (call-interactively 'nu-find-char))
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
    ('revert-buffer))
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
