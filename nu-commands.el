
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
(defun nu-describe-global () (interactive) (describe-keymap 'nu-global-map))
(define-key nu-help-map (kbd "g") 'nu-describe-global)
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
 f: nu-find-prompt     g: nu-global-prompt
 o: nu-open-prompt

 // Alt Key Prompts
 s: nu-save-prompt     d: nu-delete-prompt (alt)
 v: nu-insert-prompt

 q: quit this help. Works for any NU prompt."
nu-help-map)


(defun nu-yank-pop-or-yank ()
 "Try to yank pop. Otherwise, yank."
  (interactive)
  (if
    (not 
        (or
           (eq last-command 'yank)
           (eq last-command 'cua-paste-pop)))
    (yank))
    (yank-pop)) 
         


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
  (interactive)
  (ibuffer t "Blank Tab"))



(defun nu-find-char (&optional backward)
  "Move forward up to char, up to end of buffer."
  (interactive)
  (setq c (read-char-exclusive))
  (setq b nil)
  (while (eq b nil)
  (if (eq (char-after) c)
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
  (setq prefix-arg current-prefix-arg)
  ; Avoid infinite loop : we deactivate C-c as a key,
  ; then run C-c in order to make it a prefix...
  (define-key nu-keymap (kbd "C-c") nil)
  (setq unread-command-events
     (listify-key-sequence "\C-c"))
  ; Now add back function but after some delay
  ; or this would intercept C-c!
  (run-with-timer 0.3 nil 'define-key nu-keymap (kbd "C-c") 'nu-copy-region-or-line))


(defun nu-cut-region-or-line ()
  "If region is selected, cut region.
   If not, cut line."
  (interactive)
  (if (and (transient-mark-mode) (eq mark-active t))
   (call-interactively 'kill-region)
   (kill-whole-line))
  (message "Cut! If you wanted to x keymap, Undo with M-z or C-z then, C-g"))

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



(defun nu-close-tab ()
  "Closes Current tab.
If tab is the only one, closes window.
If window is the only one, kill buffer."
  (interactive)
  (setq win (next-window (next-window)))
  (if win
    (if (eq win (next-window))
         (kill-buffer)
         (delete-window))))




(provide 'nu-commands)
