; sometimes even simple interactive command need to be defined here
; this is because describe-keymap need some definition
; lambda function appear as ?

(defun nu-next-window () (interactive) (other-window 1))
(defun nu-previous-window () (interactive) (other-window -1))
(defun nu-no-goal-column () (interactive) (setq goal-column nil) (message "No goal column"))
(defun nu-find-char-backward () (interactive) (nu-find-char t))
(defun nu-join-with-following-line () (interactive) (join-line 1))
(defun nu-rot-reg-or-toggle-rot () (interative) (if mark-active (rot13-region) (toggle-rot13-mode)))


(defun nu-bold ()
 (interactive)
 (message (format "%s" major-mode))
 (cond
   ((eq major-mode 'fundamental-mode)
    (call-interactively 'fill-paragraph))
   ((eq major-mode 'org-mode)
    (call-interactively 'org-emphasize))
   ((eq major-mode 'lisp-interaction-mode)
    (call-interactively 'comment-region))
   ((eq major-mode 'emacs-lisp-mode)
    (call-interactively 'comment-region))
   ((eq major-mode 'c-mode)
    (call-interactively 'comment-region))
   (t
    (message "nu-bold : no action"))))


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
  (if (eq previous nil)
      (next-buffer)
      (previous-buffer))
  (while (string-match "\*.*\*" (buffer-name))
      (if (eq previous nil)
          (next-buffer)
          (previous-buffer))))


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
(defun nu-goto-line-previousbuffer () (interactive) (goto-line (previous-buffer)))


(define-prefix-command 'nu-help-map)
(define-key nu-help-map (kbd "<RET>") '(lambda ()
  (interactive) (describe-keymap 'nu-keymap)))
(define-key nu-help-map (kbd "m") '(lambda ()
  (interactive) (describe-keymap 'nu-menu-map)))
(defun nu-describe-help () (interactive) (describe-keymap 'help-map))
(define-key nu-help-map (kbd "h") 'nu-describe-help)
(defun nu-describe-a () (interactive) (describe-keymap 'nu-a-map))
(define-key nu-help-map (kbd "a") 'nu-describe-a)
(defun nu-describe-print () (interactive) (describe-keymap 'nu-print-map))
(define-key nu-help-map (kbd "p") 'nu-describe-print)
(defun nu-describe-replace () (interactive) (describe-keymap 'nu-replace-map))
(define-key nu-help-map (kbd "r") 'nu-describe-replace)
(defun nu-describe-open () (interactive) (describe-keymap 'nu-open-map))
(define-key nu-help-map (kbd "o") 'nu-describe-open)
(defun nu-describe-find () (interactive) (describe-keymap 'nu-find-map))
(define-key nu-help-map (kbd "f") 'nu-describe-find)
(defun nu-describe-delete () (interactive) (describe-keymap 'nu-delete-map))
(define-key nu-help-map (kbd "d") 'nu-describe-delete)
(defun nu-describe-insert () (interactive) (describe-keymap 'nu-insert-map))
(define-key nu-help-map (kbd "v") 'nu-describe-insert)
(defun nu-describe-save () (interactive) (describe-keymap 'nu-save-map))
(define-key nu-help-map (kbd "s") 'nu-describe-save)
(make-help-screen nu-help
(purecopy "emacs-NU help")
"See there for manual: URL `https://github.com/pyluyten/emacs-nu/blob/master/doc/nu.pdf?raw=true'
 Space / Backspace to scroll this help screen.
 (Like any other NU prompt)

 Enter a key to describe NU shorcuts.

 Enter <Return> to see _the full_ keymap.
 m: describe the menu-map, ie, all the prompts.

 // Control Key Prompts
 h: nu-help-prompt     a: nu-a-prompt
 p: nu-print-prompt    r: nu-replace-prompt
 f: nu-find-prompt     o: nu-open-prompt

 // Alt Key Prompts
 s: nu-save-prompt     d: nu-delete-prompt (alt)
 v: nu-insert-prompt

 q: quit this help. Works for any NU prompt."
nu-help-map)


(defun nu-yank (&optional n)
"Yank as many time as numeric-prefix-argument.

Thus, always paste at leat one time.
Negative argument is not handled."
  (interactive "p")
  (if (eq n nil)
   (yank)
   (if (< n 0)
    (message "cannot yank negative arg yet.")
     (while (> n 0)
      (yank)
      (setq n (1- n))))))


(defun nu-yank-pop-or-yank (&optional n)
 "Try to yank pop. Otherwise, yank.
If called with a prefix argument, yank several times.
In such case, this function will not yank pop.

To go backward several times in history, use
a browse-kill-ring function."
  (interactive "p")
   (if (not (or (eq last-command 'yank)
              (eq last-command 'nu-yank)
              (eq last-command 'cua-paste-pop)))
    (nu-yank n)
    ; otherwise, if n yank n otherwise yank pop.
    (if (eq n 1)
     (yank-pop)
     (nu-yank n))))



(defun nu-mark-whole-buffer ()
  (interactive)
  (run-with-timer 0.01 nil 'mark-whole-buffer))
(defun nu-mark-defun ()
  (interactive)
  (run-with-timer 0.01 nil 'mark-defun))
(defun nu-mark-sentence ()
  (interactive)
  (run-with-timer 0.01 nil 'mark-sentence))
(defun _nu-mark-a-word ()
  (interactive)
  (run-with-timer 0.01 nil 'nu-mark-a-word))
(defun _nu-select-a-block ()
  (interactive)
  (run-with-timer 0.01 nil 'nu-select-a-block))
(defun nu-mark-paragraph ()
  (interactive)
  (run-with-timer 0.01 nil 'mark-paragraph))
(defun nu-mark-to-bol ()
  (interactive)
  (run-with-timer 0.01 nil 'nu-mark-to-beginning-of-line))
(defun nu-mark-to-eol ()
  (interactive)
  (run-with-timer 0.01 nil 'nu-mark-to-end-of-line))
(defun _nu-mark-current-line ()
  (interactive)
  (run-with-timer 0.01 nil 'nu-mark-current-line))
(defun nu-set-mark ()
  (interactive)
  (run-with-timer 0.01 nil 'cua-set-mark))
(defun nu-set-rectangle-mark ()
  (interactive)
  (run-with-timer 0.01 nil 'cua-set-rectangle-mark))



(defun nu-select-a-block ()
  "Select a block
\(What vim calls a 'word')

A contigent amount of chars different than <space>."
   (interactive)
   (while (not (eq (char-before) ?\s))
          (backward-char))
   (cua-set-mark)
   (while (not (or
                   (eq (char-after) ?\s)
                   (eq (char-after) ?\n)))
          (forward-char)))

(defun nu-mark-a-word ()
  "Mark a word."
  (interactive)
  (backward-word)
  (mark-word))

(defun nu-mark-to-end-of-line ()
  "Select up to eol."
  (interactive)
  (cua-set-mark)
  (end-of-line))


(defun nu-mark-to-beginning-of-line ()
  "Select up to bol."
  (interactive)
  (cua-set-mark)
  (beginning-of-line))


(defun nu-mark-current-line ()
  "Select current line."
  (interactive)
  (beginning-of-line)
  (cua-set-mark)
  (end-of-line))


(defun nu-mark-sentence ()
  "Mark whole sentence.

Sentence uses sentence-end delimiter."
  (interactive)
  (backward-sentence)
  (call-interactively 'mark-end-of-sentence))


(defun nu-new-tab ()
"Open a new tab. Show ibuffer."
  (interactive)
  (ibuffer t "Blank Tab"))



(defun nu-find-char (&optional backward)
  "Move forward up to char, up to end of buffer."
  (interactive)
  (setq c (read-char-exclusive "Enter char to move point to:"))
  (setq b nil)
  (while (eq b nil)
  (if (eq 
         (if backward (char-before) (char-after)) c)
    (setq b t)
    (if backward
      (backward-char)
      (forward-char)))))


(defun nu-end-of-line ()
  (interactive)
  (if (= (point) (progn (end-of-line) (point)))
     (next-line)))



(defun nu-back-to-indentation ()
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
    (if (= (point) (progn (beginning-of-line) (point)))
	(previous-line))))


(defun nu-back-to-bol ()
  (interactive)
  (if (= (point) (progn (beginning-of-line) (point)))
      (previous-line)))


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
       (setq n (if arg (prefix-numeric-value-arg) 1))
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
       (setq n (if arg (prefix-numeric-value-arg) 1))
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
  "Set temporary overlay map mode-specific-map"
  (interactive)
  (message "C-c active.")
  (setq prefix-arg current-prefix-arg)

  ; Avoid infinite loop : we deactivate C-c as a key,
  (define-key nu-keymap (kbd "C-c") nil)

  ; special case : look at C-c C-c
  ; if possible, use Control Space to toggle this.
  (setq defn-target
       (local-key-binding (kbd "\C-c \C-c")))

  ; to do this C-c C-c trick, we need this to be bound,
  ; and also C-c C-<space> not to be...
  (unless (or (eq nil defn-target)
          (not (symbolp defn-target))
          (not (commandp defn-target)))
    (message (format "C-c active. Assigning %s" (symbol-name defn-target)))
    (setq defn-obstruct
       (local-key-binding (kbd "\C-c C-SPC")))
    (if (and (eq nil defn-obstruct)
             (commandp defn-target))
        (define-key nu-keymap (kbd "\C-c C-SPC") defn-target)))

    ; then run C-c in order to make it a prefix...
    (setq unread-command-events
      (listify-key-sequence "\C-c"))

  ; Now add back function but after some delay
  ; or this would intercept C-c!
  (run-with-timer 0.3 nil 'define-key nu-keymap (kbd "C-c") 'nu-copy-region-or-line))


(defun nu-cut-region-or-line (&optional arg)
  "If region is selected, cut region.
   If not, cut line."
  (interactive "p")
  (if (and (transient-mark-mode) (eq mark-active t))
   (call-interactively 'kill-region)
   (kill-whole-line arg))
  (message "Cut! If you wanted to x keymap, Undo with M-z or C-z then, C-g"))


(defun nu-delete-a-block ()
  "Select a block
\(What vim calls a 'word')

A contigent amount of chars different than <space>."
   (interactive)
   (while (not (or
                   (eq (char-before) ?\s)
                   (eq (char-before) ?\n)))
          (backward-char))
   (while (not (or
                   (eq (char-after) ?\s)
                   (eq (char-after) ?\n)))
          (delete-char 1)))


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

(defun nu-delete-above-line ()
  ""
  (interactive)
  (previous-line)
  (kill-whole-line))

(defun nu-delete-below-line ()
  ""
  (interactive)
  (next-line)
  (kill-whole-line)
  (previous-line))

(defun nu-backward-kill-line ()
  "Kill ARG lines backward."
  (interactive)
  (kill-line (- 0)))

(defun nu-yank-end-of-line ()
  "Copy up to end of line."
  (interactive)
  (kill-ring-save (point) (line-beginning-position 2)))



(defun nu-new-empty-buffer ()
  "Opens a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))



(defun nu-close-document ()
  "Closes current document.
If tab is the only one, closes window.
If window is the only one, kill buffer."
  (interactive)
  (setq win (next-window (next-window)))
  (if win
    (if (eq win (next-window))
         (bury-buffer)
         (delete-window))))




(provide 'nu-commands)
