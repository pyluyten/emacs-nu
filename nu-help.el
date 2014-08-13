
;
; note : one cannot "advice" a 'map'



 ; map-keymap has no way to receive
 ; more than two args
 ; we cannot easily communicate
 ; to this function which keymap it is parsing (!)
 ; thus, use a global var
(defvar nu-current-keymap nil)
(defvar nu-current-major-mode nil)

; repeat does not work
; as we would like with prompts
; below fixes this.
;
; (you can see repeat advice and nu-prompt-for-keymap but
; you already got the idea)
(defvar nu-last-command nil)



(defun nu-help-about-prompts ()
"Displays a prompt to choose a function,
but rather than executing the function,
describes it."
 (interactive)
 (nu-prompt-for-keymap nu-current-keymap t))


(defun nu-define-prefix (arg)
"Define a prefix command, assign ? key.

This is a common key to _all_ prompts."
 (define-prefix-command arg)
 (define-key arg (kbd "?") 'nu-help-about-prompts))


(defun nu-prompt-describe (arg)
 "describe the nu prompt function point is at."
 (describe-function (function-called-at-point)))



(defun nu-insert-binding-row (ev bind)
 "insert some link, the binding, the global binding, CR.

  Do not document digit-argument."
 (if (and (symbolp bind) (not (eq bind 'digit-argument))
                         (not (eq bind 'nu-help-about-prompts)))
       (progn
  ; insert the button
        (insert-button (symbol-name bind) 'action 'nu-prompt-describe)

  ; insert shortcuts _from the prompt_
        (setq help-string (where-is-internal bind (list nu-current-keymap)))
        (if (not (eq nil help-string))
             (progn
               (insert
                 (format " %s"
                   (mapconcat 'key-description help-string ", ")))))

   ;; print the _direct keys_
   ;; remove menu, menu-bar, f1, help, ..
   ;; use non-greedy "*?"
  (setq major-keymap (eval (intern-soft 
          (concat (symbol-name nu-current-major-mode) "-map"))))


   ;; TODO : make the regexp replace one or two C-c at beginning only
   ;; (since where-is-internal does not know our sorcery)

  (if (not (keymapp major-keymap))
      (setq majorkeys "")
      (setq majorkeys
           (replace-regexp-in-string "\\(C-c\\)" "C-<SPC>"
               (mapconcat 'key-description (where-is-internal
                     bind (list major-keymap)) "@"))))

   (setq all
      (replace-regexp-in-string "\\(<menu>\\|<menu-bar>\\|<f.>\\|<help>\\).*?@" ""
        (format "%s@"
          (concat
           (mapconcat 'key-description (where-is-internal bind nu-keymap) "@")
            "@"
            majorkeys))))
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
 ;
 ; also, include major mode keys.
 (setq nu-current-keymap keymap)
 (setq nu-current-major-mode major-mode)

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
      (message (format "key is %s" (key-description key)))

; check if the user needs to scroll the help. Do not break loop.
      (if (eq (aref key 0) ?\d)
               (scroll-down)
          (if (eq (aref key 0) ?\s)
               (scroll-up)

; check for digit-argument. Do not use native function since
; last-command-key or something is broken.
; (string-to-number) would return 0, so don't look for this (infinite loop)
; string match seems correct.

               (if ;(memq (string-to-number (key-description key)) (list 1 2 3 4 5 6 7 8 9))
                   (and (stringp (key-description key))
                        (string-match (key-description key) "[0123456789]"))
                   (if (eq current-prefix-arg nil)
                       (setq current-prefix-arg (string-to-number (key-description key)))
                       (setq current-prefix-arg (+ (string-to-number (key-description key))
                                                   (* current-prefix-arg 10))))

; now, break the loop, no matter a func has been found or not.
; eg the user can type not-mapped key to quit. "q" is never boundp.
                    (progn
                       (setq defn (lookup-key local-map key))
                       (message "")
                       (set-window-configuration config)
                       (setq input t))))))

; run the func.
      (if defn
          (progn
              (if describe
                    (describe-function defn)
                    (progn
                          (setq nu-last-command defn)
                          (call-interactively defn))))))


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

(provide 'nu-help)
