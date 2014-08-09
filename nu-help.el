
;
; note : one cannot "advice" a 'map'



 ; map-keymap has no way to receive
 ; more than two args
 ; we cannot easily communicate
 ; to this the keymap we are parsing (!)
 ; thus, use a global var
(defvar nu-current-keymap nil)


; repeat does not work
; as we would like with prompts
; below fixes this.
;
; (you can see repeat advice and nu-prompt-for-keymap but
; you already got the idea)
(defvar nu-last-command nil)



(defun nu-help-about-prompts ()
 (interactive)
 (nu-prompt-for-keymap nu-current-keymap t))


(defun nu-define-prefix (arg)
"Define a prefix command, assign ? key."
 (define-prefix-command arg)
 (define-key arg (kbd "?") 'nu-help-about-prompts))



; for a given binding, display stuff...
(defun nu-insert-binding-row (ev bind)
 "insert some link, the binding, the global binding, CR."
 (if (symbolp bind)
       (progn
  ; insert the button
        (insert-button (symbol-name bind))

  ; insert shortcuts from the prompt
        (setq help-string (where-is-internal bind (list nu-current-keymap)))
        (if (not (eq nil help-string))
             (progn
               (insert
                 (format " %s"
                   (mapconcat 'key-description help-string ", ")))))

  ;; print the direct keys
   (setq all
      (replace-regexp-in-string "<menu>.*@" ""
        (format "%s@"
          (mapconcat 'key-description (where-is-internal bind) "@"))))
   (if (> (string-width all) 1)
   (progn
     (setq all (replace-regexp-in-string "@" " " all))
     (insert " - or " all)))
   (insert "\n"))))




(defun nu-prompt-for-keymap (keymap &optional describe)
 "Help to choose a key from a keymap

If describe arg is t, only describe-function."

 ; map-keymap has no way to receive
 ; more than two args
 ; we cannot easily communicate
 ; to this the keymap we are parsing (!)
 ; thus, use a global var
 (setq nu-current-keymap keymap)

 (setq prev-frame (selected-frame))
 (setq config (current-window-configuration))
 (setq local-map (make-sparse-keymap))
 (setcdr local-map keymap)
 (define-key local-map [t] 'undefined)

 (with-help-window (help-buffer)
  (with-current-buffer "*Help*"
   (if describe
       (insert
"In a standard prompt, press the associated key to run the function.
Use space or del to scroll down or up.
Press ? to obtain this screen.

From this prompt, press the associated key
to describe the function.\n")
       (insert
"Press ? for help or to describe function\n"))
   (map-keymap 'nu-insert-binding-row keymap)
   (insert "\n\n\n")))


 (switch-to-buffer-other-window "*Help*")
 (setq new-frame (window-frame (selected-window)))

 (setq cursor-in-echo-area t)
 (setq input nil)
 (setq defn nil)
   (while (not input)
      (setq key (read-key-sequence "Enter a key or ? :"))
      (if (eq (aref key 0) ?\d)
               (scroll-down)
          (if (eq (aref key 0) ?\s)
               (scroll-up)
             (progn
                (setq defn (lookup-key local-map key))
                (message "")
                (set-window-configuration config)
                (setq input t))))
      (if defn
          (progn
              (if describe
                    (describe-function defn)
                    (progn
                          (setq nu-last-command defn)
                          (call-interactively defn)))))))


(defadvice repeat (before nu-repeat-last-prompt ())
  (if
   (or
    (eq last-repeatable-command 'nu-delete-prompt)
    (eq last-repeatable-command 'nu-replace-prompt)
    (eq last-repeatable-command 'nu-window-prompt)
    (eq last-repeatable-command 'nu-open-prompt)
    (eq last-repeatable-command 'nu-a-prompt)
    (eq last-repeatable-command 'nu-find-prompt)
    (eq last-repeatable-command 'nu-help-prompt)
    (eq last-repeatable-command 'nu-new-prompt)
    (eq last-repeatable-command 'nu-save-prompt)
    (eq last-repeatable-command 'nu-insert-prompt)
    (eq last-repeatable-command 'nu-print-prompt))
   (setq last-repeatable-command nu-last-command)))

(ad-activate 'repeat)



; keep the code here temp.
; just for inspiration.
; i'm still missing scroll & maybe %THIS-KEY%
(defmacro make-help-screen2 (fname help-line help-text helped-map)
  "Construct help-menu function name FNAME.
When invoked, FNAME shows HELP-LINE and reads a command using HELPED-MAP.
If the command is the help character, FNAME displays HELP-TEXT
and continues trying to read a command using HELPED-MAP.
If HELP-TEXT contains the sequence `%THIS-KEY%', that is replaced
with the key sequence that invoked FNAME.
When FNAME finally does get a command, it executes that command
and then returns."
  (let ((doc-fn (intern (concat (symbol-name fname) "-doc"))))
    `(progn
       (defun ,doc-fn () ,help-text nil)
       (defun ,fname ()
	 "Help command."
	 (interactive)
	 (let ((line-prompt
		(substitute-command-keys ,help-line)))
	   (when three-step-help
	     (message "%s" line-prompt))
	   (let* ((help-screen (documentation (quote ,doc-fn)))
		  ;; We bind overriding-local-map for very small
		  ;; sections, *excluding* where we switch buffers
		  ;; and where we execute the chosen help command.
		  (local-map (make-sparse-keymap))
		  (new-minor-mode-map-alist minor-mode-map-alist)
		  (prev-frame (selected-frame))
		  config new-frame key char)
	     (when (string-match "%THIS-KEY%" help-screen)
	       (setq help-screen
		     (replace-match (key-description
				     (substring (this-command-keys) 0 -1))
				    t t help-screen)))
	     (unwind-protect
		 (let ((minor-mode-map-alist nil))
		   (setcdr local-map ,helped-map)
		   (define-key local-map [t] 'undefined)
		   ;; Make the scroll bar keep working normally.
		   (define-key local-map [vertical-scroll-bar]
		     (lookup-key global-map [vertical-scroll-bar]))
		   (if three-step-help
		       (progn
			 (setq key (let ((overriding-local-map local-map))
				     (read-key-sequence nil)))
			 ;; Make the HELP key translate to C-h.
			 (if (lookup-key function-key-map key)
			     (setq key (lookup-key function-key-map key)))
			 (setq char (aref key 0)))
		     (setq char ??))
		   (when (or (eq char ??) (eq char help-char)
			     (memq char help-event-list))
		     (setq config (current-window-configuration))
		     (switch-to-buffer-other-window "*Help*")
		     (and (fboundp 'make-frame)
			  (not (eq (window-frame (selected-window))
				   prev-frame))
			  (setq new-frame (window-frame (selected-window))
				config nil))
		     (setq buffer-read-only nil)
		     (let ((inhibit-read-only t))
		       (erase-buffer)
		       (insert help-screen))
		     (let ((minor-mode-map-alist new-minor-mode-map-alist))
		       (help-mode)
		       (setq new-minor-mode-map-alist minor-mode-map-alist))
		     (goto-char (point-min))
		     (while (or (memq char (append help-event-list
						   (cons help-char '(?? ?\C-v ?\s ?\177 delete backspace vertical-scroll-bar ?\M-v))))
				(eq (car-safe char) 'switch-frame)
				(equal key "\M-v"))
		       (condition-case nil
			   (cond
			    ((eq (car-safe char) 'switch-frame)
			     (handle-switch-frame char))
			    ((memq char '(?\C-v ?\s))
			     (scroll-up))
			    ((or (memq char '(?\177 ?\M-v delete backspace))
				 (equal key "\M-v"))
			     (scroll-down)))
			 (error nil))
		       (let ((cursor-in-echo-area t)
			     (overriding-local-map local-map))
			 (setq key (read-key-sequence
				    (format "Type one of the options listed%s: "
					    (if (pos-visible-in-window-p
						 (point-max))
						"" ", or SPACE or DEL to scroll")))
			       char (aref key 0)))

		       ;; If this is a scroll bar command, just run it.
		       (when (eq char 'vertical-scroll-bar)
			 (command-execute (lookup-key local-map key) nil key))))
		   ;; We don't need the prompt any more.
		   (message "")
		   ;; Mouse clicks are not part of the help feature,
		   ;; so reexecute them in the standard environment.
		   (if (listp char)
		       (setq unread-command-events
			     (cons char unread-command-events)
			     config nil)
		     (let ((defn (lookup-key local-map key)))
		       (if defn
			   (progn
			     (when config
			       (set-window-configuration config)
			       (setq config nil))
			     ;; Temporarily rebind `minor-mode-map-alist'
			     ;; to `new-minor-mode-map-alist' (Bug#10454).
			     (let ((minor-mode-map-alist new-minor-mode-map-alist))
			       ;; `defn' must make sure that its frame is
			       ;; selected, so we won't iconify it below.
			       (call-interactively defn))
			     (when new-frame
			       ;; Do not iconify the selected frame.
			       (unless (eq new-frame (selected-frame))
				 (iconify-frame new-frame))
			       (setq new-frame nil)))
			 (ding)))))
	       (when config
		 (set-window-configuration config))
	       (when new-frame
		 (iconify-frame new-frame))
	       (setq minor-mode-map-alist new-minor-mode-map-alist))))))))


(provide 'nu-help)
