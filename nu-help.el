
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

(provide 'nu-help)
