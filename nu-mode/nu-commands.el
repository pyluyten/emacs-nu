;;; nu-commands.el --- Modern Emacs Keybinding
;;; Emacs-Nu is an emacs mode which wants to makes Emacs easier.kk
;;; Copyright (C) 2018 Pierre-Yves LUYTEN
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


;; FIXME
;; this files contains a lot of commands
;; this should be reviewed


(require 'nu-vars)

(defun nu-check-candidates-for-menu ()
;; this func does analyse if nu-menu implementation is complete
;; open a file
;; row 1 : contain the PATH to nu file (eg nu-markdown.el nu-org.el ...)
;;         this file is the reference detailing which func are already
;;         included in nu menu
;; further rows :
;; - [ ] markdown-do
;; - [ ] markdown-promote
;; 
;; nu-check-for-candidates-for-menu will toggle row when func is found
;; in nu file provided in row 1.
;; if func is not found, it will let checkbox not toggled.
;; then sort-lines is used to emphasize which func remain to integrate in
;; nu menu.
;; 
 (interactive)
 (let ((not-last-line t)
       (nu-limit 0)
       (nu-file nil)
       (nu-func nil)
       (found-func nil))

   ;; loop start
   (goto-char 1)
   (while (and not-last-line (< nu-limit 366))
     
     (setq nu-limit (+ nu-limit 1)
	   nu-func nil
	   found-func nil)
  
     ;; if first line, note nu file
     (if (eq nu-file nil)
         (setq nu-file (buffer-substring-no-properties 
                          (line-beginning-position)
                          (line-end-position)))
          
         ;; else, check the func
         ;; 1st we grab the func name
         (progn
  	  (end-of-line)
  	  (ignore-errors (search-backward " "))
          (setq nu-func (buffer-substring-no-properties
                        (+ 1 (point))
  	                (line-end-position)))
 
          ;; so 2nd we look for this func in nu file
          (with-temp-buffer
            (insert-file-contents nu-file nil nil nil t)
	    (goto-char 1)
	    (setq found-func (search-forward nu-func nil t)))

          ;; 3rd we amend the current row
	  (unless (eq nu-func nil)
            (progn
	       (beginning-of-line)
	       (delete-region (point) (line-end-position))
	       (if (not (eq found-func nil))
	         (insert "- [X] ")
                 (insert "- [ ] "))
	       (insert nu-func)))))

     ;; loop : next line
     (setq not-last-line (= 0 (forward-line 1))))

     ;; loop is over. sort lines.
     (beginning-of-buffer)
     (forward-line 1)
     (sort-lines nil (point) (buffer-end 1))))




;; Commands for menu
;; i sent a request to include this func in bookmark.el to the maintainer ;; ( 2018.10.09)

(defun bookmark-jump-other-frame (bookmark)
  "Jump to BOOKMARK in another frame.  See `bookmark-jump' for more."
  (interactive
   (list (bookmark-completing-read "Jump to bookmark (in another frame)"
                                   bookmark-current-bookmark)))
  (bookmark-jump bookmark 'view-buffer-other-frame))





(defun nu-no-goal-column () (interactive) (setq goal-column nil) (message "No goal column"))
(defun nu-join-with-following-line () (interactive) (join-line 1))
(defun nu-rot-reg-or-toggle-rot () (interactive) (if mark-active (rot13-region) (toggle-rot13-mode)))


; async shell commands...;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nu-git-push ()
 (interactive)
 (async-shell-command "git push"))


(defun nu-texi2pdf ()
 (interactive)
 (async-shell-command (format "texi2pdf %s" buffer-file-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'nu-cheat-sheet 'which-key-show-top-level)

(defun nu-prompt-for-major-mode ()
  "Promt for major mode keymap.

Raise a standard prompt, using major mode as keymap.
Well, at least current-local-map : this should be what
we are talking about..."
  (interactive)
  (nu-prompt-for-keymap (current-local-map)))


; the function that defines nu-bold-function
; it is made to be adviced by major mode
; so major mode can override defalias
(defun nu-set-bold-f ()
  (defalias 'nu-bold-function 'fill-paragraph))


;; this function that calls nu-bold-function
(defun nu-bold ()
  (interactive)
  (nu-set-bold-f)
  (nu-bold-function))


(defun nu-zap-up-to-char (arg char)
  "Kill up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (interactive "p\ncZap up to char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (kill-region (point)
                 (progn
                   (forward-char direction)
                   (unwind-protect
                       (search-forward (char-to-string char) nil nil arg)
                     (backward-char direction))
                   (point)))))


(defun nu-next-buffer (&optional previous)
  "next-buffer, skip some.

Messages, Backtrace, Completions, Help."
  (interactive)
  (let* ((currentbuffer (current-buffer))
	 (needstobreak nil))
   (if (eq previous nil)
       (next-buffer)
       (previous-buffer))
   (while (and (string-match "\*.*\*" (buffer-name))
	       (eq needstobreak nil))
     (if (not (eq currentbuffer (current-buffer)))
         (if (eq previous nil)
             (next-buffer)
             (previous-buffer))
         (setq needstobreak t)))))


(defun nu-previous-buffer ()
  "Calls nu nu-next-buffer with arg."
  (interactive)
  (nu-next-buffer '-))



(defun nu-create-tags (dir-name)
 "Create exhuberant ctags.

Interactively specify a directory.
Output will be standard TAGS file."
 (interactive "DDirectory: ")
 (shell-command
   (format "ctags -e -R %s" (directory-file-name dir-name))))


(defun nu-toggle-goal-column ()
 "Toggle goal column."
 (interactive)
 (if (eq goal-column nil)
     (call-interactively 'set-goal-column)
     (nu-no-goal-column)))

(defun nu-isearch-forward ()
"Search forward interactively.

If region is selected, use this as a search string."
  (interactive)
  (if mark-active
      (progn
        (call-interactively 'isearch-forward)
        (isearch-yank-string (buffer-substring-no-properties (region-beginning) (region-end))))
      (isearch-forward)))

(defun nu-isearch-backward ()
"Search backward interactively.

If region is selected, use this as a search string."
  (interactive)
    (if mark-active
      (progn
        (call-interactively 'isearch-backward)
        (isearch-yank-string (buffer-substring-no-properties (region-beginning) (region-end))))
    (isearch-backward)))

(defun nu-isearch-forward-regexp ()
"Search forward interactively (regexp).

If region is selected, use this as a search string."
  (interactive)
    (if mark-active
      (progn
        (call-interactively 'isearch-forward-regexp)
        (isearch-yank-string (buffer-substring-no-properties (region-beginning) (region-end))))
    (call-interactively 'isearch-forward-regexp)))

(defun nu-isearch-backward-regexp ()
"Search backward interactively (regexp).

If region is selected, use this as a search string."
  (interactive)
    (if mark-active
      (progn
        (call-interactively 'isearch-backward-regexp)
        (isearch-yank-string (buffer-substring-no-properties (region-beginning) (region-end))))
    (isearch-backward-regexp)))

(defun nu-goto-line-previousbuffer ()
 (interactive)
 (previous-buffer)
 (call-interactively 'goto-line))

(defun nu-new-tab ()
"Open a new tab. Show ibuffer."
  (interactive)
  (ibuffer t "Blank Tab"))



(defun nu-find-char (&optional backward)
  "Move forward up to char, up to end of buffer."
  (interactive)
  (let ((c (read-char-exclusive "Enter char to move point to:"))
        (b nil))
    (while (eq b nil)
    (if (eq
         (if backward (char-before) (char-after)) c)
      (setq b t)
      (if backward
        (backward-char)
        (forward-char))))))


(defun nu-find-char-backward ()
  (interactive)
  (nu-find-char t))


(defun nu-end-of-line ()
  (interactive)
  (if (= (point) (progn (end-of-line) (point)))
     (forward-line)))



(defun nu-back-to-indentation ()
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
    (if (= (point) (progn (beginning-of-line) (point)))
        (forward-line -1))))


(defun nu-back-to-bol ()
  (interactive)
  (if (= (point) (progn (beginning-of-line) (point)))
      (forward-line -1)))

 (defun nu-copy-line (arg)
    "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
    (interactive "p")
    (let ((beg (line-beginning-position))
          (end (line-end-position arg)))
      (when mark-active
        (if (> (point) (mark))
            (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
          (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
      (if (eq last-command 'copy-line)
          (kill-append (buffer-substring beg end) (< end beg))
        (kill-ring-save beg end)))
    (kill-append "\n" nil)
    (beginning-of-line (or (and arg (1+ arg)) 2))
    (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))


(defun nu-copy-region-or-line ()
  (interactive)
  (if (and (transient-mark-mode) (eq mark-active t))
    (call-interactively 'kill-ring-save)
    (call-interactively 'nu-copy-line))
  (message "Copy done! [Use control-space for C-c mode prefix.]"))


(defun nu-trigger-mode-specific-map ()
  (interactive)
  (which-key-mode 1)
  (run-with-timer 5 nil 'define-key evil-insert-state-map (kbd "C-c") (key-binding (kbd "C-c")))
  (define-key evil-insert-state-map (kbd "C-c") nil)
  (setq unread-command-events
	(listify-key-sequence "\C-c")))


(defun nu-cut-region-or-line (&optional arg)
  "If region is selected, cut region.
   If not, cut line."
  (interactive "p")
  (if (and (transient-mark-mode) (eq mark-active t))
   (call-interactively 'kill-region)
   (kill-whole-line arg))
  (message "Cut! If you wanted to x keymap, Undo with M-z or C-z then, C-g"))

(defun nu-delete-all ()
 "Deletes the current buffer text."
 (interactive)
 (mark-whole-buffer)
 (call-interactively 'kill-region))


(defun nu-delete-defun ()
 "Deletes a function."
 (interactive)
 (backward-sexp)
 (kill-sexp))

(defun nu-backward-kill-line ()
  "Kill ARG lines backward."
  (interactive)
  (kill-line (- 0)))

(defun nu-yank-end-of-line ()
  "Copy up to end of line."
  (interactive)
  (kill-ring-save (point) (line-beginning-position 2)))



(defun nu-quit-document ()
  "Closes current document.
If tab is the only one, closes window.
If window is the only one, kill buffer."
  (interactive)
  (let ((win (next-window (next-window))))
   (if win
     (if (eq win (next-window))
         (bury-buffer)
         (delete-window)))))




(defun nu-new-empty-buffer ()
  "Opens a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

(defalias 'nu-insert-line-below 'open-line)

(defalias 'nu-clear-mark 'cua-set-mark)

(defun nu-insert-line-above (&optional num)
"Insert an empty row above."
 (interactive "p")
 (open-line num)
 (next-line num))


(provide 'nu-commands)
