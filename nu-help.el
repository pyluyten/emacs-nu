;
; les pistes par rapport au "define func"
; 1. créer un help bouton à l'aide d'une fonction déjà existante
;    ou encore un clickable text.
; 2. créer une référence `ma-fonc' et demander au help buffer de faire
;    le travail
; 3. proposer une autre manière de consulter les fonctions...
;    par exemple la touche . est réservée à l'actuel ?
;    et le ? sert à describe-func sur la keymap en cours
;    
;
; TODO
; (fix bugs)
; clickable functions
; 
; 
;  "27" is ^[ is escape but is actually meta. Uh?
;
;
; on ne peut pas "advice" une 'map'. Seulement une fonction stricto sensu.

;(require 'button)



(defun nu-help-about-prompts ()
"Provides some information about prompts."
 (interactive)
 (with-help-window (help-buffer)
  (with-current-buffer "*Help*"
    (insert
"A prompt appears when you press a `prefix' command.

You can enter a key to trigger a function. The prompts
informs you about which keys are available.

Use <space> / <backspace> to scroll the prompt.

Use control+h then f to trigger describe-function,
then enter the function you want to describe."))))


 ; map-keymap has no way to receive
 ; more than two args
 ; we cannot easily communicate
 ; to this the keymap we are parsing (!)
 ; thus, use a global var
(setq nu-current-keymap nil)



(defun nu-define-prefix (arg)
"Define a prefix command, assign ? key."
 (define-prefix-command arg)
 (define-key arg (kbd "?") 'nu-help-about-prompts))

;(defun nu-insert-button (nu-function some-keymap)
;  "insert a button describing function!"
;  (insert-button nu-function 'action
;    (lambda (x) 
;       (describe-function (intern x)))))



; for a given binding, display stuff...
(defun nu-insert-binding-row (ev bind)
 "insert some link, the binding, the global binding, CR."

 (if (symbolp bind)
 ; if this is directly a binding, just print it.
      (progn
    ;    (insert ev "  : ") ; for now we insert only
    ;    (insert-button (symbol-name bind)
    ;           'action 'nu-describe-function)
;        (insert (symbol-name bind))
; make-text-button ((point) nil 
        ;(insert-button (symbol-name bind) 'help-function (symbol-function bind))
                                     ;match-number 1 , type 'help-button, arg "def" ie symbol-function
;        (insert-button (symbol-name bind) 'help-function (symbol-function bind))
        (insert "`" (symbol-name bind) "'")
;        (if (not (eq nil (where-is-internal bind nu-keymap)))
        (setq help-string (where-is-internal bind (list nu-current-keymap)))
        (if (not (eq nil help-string))
           (progn
             (insert
               (format ", %s"
                 (mapconcat 'key-description help-string ", ")))
              (insert "\n"))))))
  ; if bind is itself a nested keymap,
  ; first print modifier, then run self.
  ;    (progn
  ;        (if (eq ev 27)
  ;            (insert "Alt+"))
  ;        (map-keymap 'nu-insert-binding-row bind))))


(defun nu-insert-binding-all (ev bind)
 (if (symbolp bind)
  (progn
   (insert (symbol-name bind) " : ")
   (insert (format ", %s" (mapconcat 'key-description
   (where-is-internal bind) ", ")))
   (insert "\n"))))

(defun nu-prompt-for-keymap-old (nukeymap)
 "display a prompter with buttons."
 (with-help-window (help-buffer)
  (with-current-buffer "*Help*"
   (insert "Press one of the below key, or ?\n" )
   (map-keymap 'nu-insert-binding-row nukeymap)))
 (set-temporary-overlay-map nukeymap))




(defun nu-prompt-for-keymap (keymap)
 "Help to choose a key from a keymap."

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
   (insert "Press one of the below key, or ?\n" )
   (map-keymap 'nu-insert-binding-row keymap)
   (insert "\n\n\n")
   (map-keymap 'nu-insert-binding-all keymap)))

 (switch-to-buffer-other-window "*Help*")
 (setq new-frame (window-frame (selected-window)))

 (setq cursor-in-echo-area t)
 (setq key (read-key-sequence "Enter a key or ? :"))
 (setq defn (lookup-key local-map key))
 (message "")
  (if defn
    (progn
      (set-window-configuration config)
      ;(delete-window (get-buffer-window (help-buffer)))
      (call-interactively defn))
    (delete-window (get-buffer-window (help-buffer)))))


(defun nu-insert-binding-desc (ev bind)
 "insert some link, the binding, the global binding, CR."
 (if (symbolp bind)
    (progn
     (insert "`" (symbol-name bind) "'")
      (if (not (eq nil (where-is-internal bind nu-keymap)))
        (insert
         (format ", %s"
            (mapconcat 'key-description
              (where-is-internal bind nu-keymap) ", "))))
        (insert "\n"))))


;; not used
(defun nu-describe-keymap (keym)
 "Creates a description of keymap."
  (generate-new-buffer "*Nu*")
  (with-current-buffer "*Nu*"
  (insert "Press one of the below key, or ?\n")
  (map-keymap 'nu-insert-binding-desc keym)
  (buffer-string)))




; keep the code here temp.
; just for inspiration.
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
			       char (aref key 0)))k

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
