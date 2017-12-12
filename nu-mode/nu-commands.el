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
					;

; sometimes even simple interactive command need to be defined here
; this is because describe-keymap need some definition
; lambda function appear as ?

(require 'nu-vars)

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

(defun nu-cheat-sheet ()
"Displays cheat sheet for nu-mode.

This command uses which-key."
  (interactive)
  (nu-which-key-prompt-for-keymap nu-keymap))


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
(defun nu-find-previous-mark () (interactive) (cua-set-mark 1))

(defun nu-goto-line-previousbuffer ()
 (interactive)
 (previous-buffer)
 (call-interactively 'goto-line))

(defun nu-mark-whole-buffer ()
  (interactive)
  (run-with-timer 0.01 nil 'mark-whole-buffer))

(defun nu-mark-defun ()
  (interactive)
  (run-with-timer 0.01 nil 'mark-defun))

(defun nu-mark-sexp ()
  (interactive)
  (run-with-timer 0.01 nil 'mark-sexp))

(defun nu-mark-paragraph ()
  (interactive)
  (run-with-timer 0.01 nil 'mark-paragraph))

(defun nu-mark-page ()
  (interactive)
  (run-with-timer 0.01 nil 'mark-page))

(defun nu-set-mark ()
  (interactive)
  (push-mark-command nil))

(defun nu-set-rectangle-mark ()
  (interactive)
  (run-with-timer 0.01 nil 'rectangle-mark-mode))


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


; see misc.el;
; except they do by default copy the whole line!
; what we want is to copy char after char.
(defun nu-copy-from-above (&optional arg)
  "Copy characters from previous nonblank line, starting just above point.
Copy ARG characters, but not past the end of that line.
If no argument given, copy 1 char."
  (interactive "P")
  (let ((cc (current-column))
        n
        (string ""))
    (save-excursion
      (beginning-of-line)
      (backward-char 1)
      (skip-chars-backward "\ \t\n")
      (move-to-column cc)
       (setq n (if arg (prefix-numeric-value arg) 1))
      ;; If current column winds up in middle of a tab,
      ;; copy appropriate number of "virtual" space chars.
      (if (< cc (current-column))
          (if (= (preceding-char) ?\t)
              (progn
                (setq string (make-string (min n (- (current-column) cc)) ?\s))
                (setq n (- n (min n (- (current-column) cc)))))
            ;; In middle of ctl char => copy that whole char.
            (backward-char 1)))
      (setq string (concat string
                           (buffer-substring
                            (point)
                            (min (line-end-position)
                                 (+ n (point)))))))
    (insert string)))


(defun nu-copy-from-below (&optional arg)
  "Copy characters from next nonblank line, starting just above point.
Copy ARG characters, but not past the end of that line.
If no argument given, copy 1 char."
  (interactive "P")
  (let ((cc (current-column))
        n
        (string ""))
    (save-excursion
      (end-of-line)
      (forward-char 1)
      (skip-chars-forward "\ \t\n")
      (move-to-column cc)
       (setq n (if arg (prefix-numeric-value arg) 1))
      ;; If current column winds up in middle of a tab,
      ;; copy appropriate number of "virtual" space chars.
      (if (< cc (current-column))
          (if (= (preceding-char) ?\t)
              (progn
                (setq string (make-string (min n (- (current-column) cc)) ?\s))
                (setq n (- n (min n (- (current-column) cc)))))
            ;; In middle of ctl char => copy that whole char.
            (backward-char 1)))
      (setq string (concat string
                           (buffer-substring
                            (point)
                            (min (line-end-position)
                                 (+ n (point)))))))
    (insert string)))


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
  (define-key evil-insert-state-map (kbd "C-c") nil)
  (run-with-timer 5 nil 'define-key evil-insert-state-map (kbd "C-c") 'nu-copy-prompt)
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
