(require 'hydra)

(defun nu-lv-prompt-for-keymap (keymap &optional describe)
 "Describe a keymap, ask for a key, run the func.

If describe arg is t, only describe-function."
 (setq nu-current-keymap keymap
       nu-describe-bind-mode "lv"
       nu-lv-message nil)
 
 (let* ((key)
        (defn)
        (prefixhelp)
        (local-map (make-sparse-keymap)))
 (setcdr local-map keymap)
 (define-key local-map [t] 'undefined)

 (setq nu-lv-message nil)
 (if (eq nil current-prefix-arg)
     (setq prefixhelp "X")
     (setq prefixhelp current-prefix-arg))
   (if describe
       (setq nu-lv-message
"In a standard prompt, press the associated key to run the function.
Use space or del to scroll down or up.
Press ? to obtain this screen.

From this prompt, press the associated key
to describe the function.\n")
       (setq nu-lv-message
	     (concat
               (propertize (format "Prefix = %s" prefixhelp) 'face 'shadow)
               (propertize "\nPress ? for help or to describe function\n" 'face 'italic))))
   (map-keymap 'nu-insert-binding-row keymap)
   (setq nu-lv-message (concat nu-lv-message "\n"))

 (lv-message nu-lv-message)
 (setq input nil)
 (setq defn nil)
   (while (not input)
	;(setq key (read-key-sequence-vector (propertize "Enter a key or ? :" 'face 'italic) t))
      (setq key (read-key-sequence-vector nil t))
      (cond

       ; check if the user needs to scroll the help. Do not break loop.
       ; ((~nu-check-vector key ?\d nil)
       ; (ignore-errors
       ;   (scroll-down nil)))
       ; ((~nu-check-vector key ?\s nil)
       ; (ignore-errors
       ;   (scroll-up nil)))

       ; allow to repeat prompt
       ((~nu-check-vector key "+" t)
          (setq nu-repeat-prompt t))

       ; check for negative / digit-argument.
       ((~nu-check-vector key "-" t)
        (cond ((integerp current-prefix-arg)
               (setq current-prefix-arg (- current-prefix-arg)))
              ((eq current-prefix-arg '-)
               (setq current-prefix-arg nil))
              (t
               (setq current-prefix-arg '-)))
         
         ;(while (re-search-forward "\\`Prefix = .*?\n" nil t)
	;(replace-match (propertize (format "Prefix = %s\n" current-prefix-arg) 'face 'underline)))
	)

       ((~nu-check-vector key "[0123456789]" t t)
          (cond
            ((eq current-prefix-arg '-)
             (setq current-prefix-arg (- (string-to-number (key-description key)))))
            ((integerp current-prefix-arg)
             (setq current-prefix-arg (+ (string-to-number (key-description key))
                                         (* current-prefix-arg 10))))
             (t
              (setq current-prefix-arg (string-to-number (key-description key)))))
            ; attention il faut modifier cela.
	   (while (re-search-forward "\\`Prefix = .*?\n" nil t)
	   (replace-match (propertize (format "Prefix = %s\n" current-prefix-arg) 'face 'underline))))

        ; now, break the loop, no matter a func has been found or not.
        ; eg the user can type not-mapped key to quit. "q" is never boundp.

       (t
	 (lv-delete-window)
         (progn
         (setq defn (lookup-key local-map key))

         ; run the func. Repeat if asked.
         (if (or (not nu-repeat-prompt)
                 (eq defn nil))
           (setq input t))
         (if defn
            (if describe
                (describe-function defn)
                (setq nu-last-command defn)
                (ignore-errors
                   (call-interactively defn)))
             ; if no func, make sure not to repeat.
	   (setq nu-repeat-prompt nil))))))))
