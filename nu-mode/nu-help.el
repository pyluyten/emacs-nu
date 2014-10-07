
;
; note : one cannot "advice" a 'map'

(defalias 'nu-prompt-for-keymap 'nu-light-prompt-for-keymap)

; defconst?
(defvar nu-state t "Used by overriding maps alist.")

(defun nu-make-overriding-map (keymap unbind-keys-list &rest bindings)
 (make-local-variable 'minor-mode-overriding-map-alist)
 (push `(nu-state . ,keymap) minor-mode-overriding-map-alist)
 (while (not (eq unbind-keys-list nil))
    (define-key keymap (kbd (car unbind-keys-list)) nil)
    (pop unbind-keys-list))
 (while (not (eq bindings nil))
    (define-key keymap (kbd (car bindings)) (car (cdr bindings)))
    (pop bindings) (pop bindings)))


 ; map-keymap has no way to receive
 ; more than two args
 ; we cannot easily communicate
 ; to this function which keymap it is parsing (!)
 ; thus, use a global var
(defvar nu-current-keymap nil)
(defvar nu-current-major-mode nil)

; for helm
(defvar nu-keymap-list)

; helm style or buffer style
(defvar nu-describe-bind-mode)


(defvar nu-repeat-prompt nil)


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
 (nu-buffer-prompt-for-keymap nu-current-keymap t))


(defun nu-define-prefix (arg)
"Define a prefix command, assign ? key.

This is a common key to _all_ prompts."
 (define-prefix-command arg)
 (define-key arg (kbd "?") 'nu-help-about-prompts))


(defun nu-prompt-describe (arg)
 "describe the nu prompt function point is at."
 (describe-function (function-called-at-point)))



(defun nu-describe-bind (bind)
"Insert into *Help* Buffer a prompt row for bind.
Or create a helm candidate, depending on nu-describe-bind-mode.

This includes symbol name, key(s) from prompt,
and drect keys from both nu-keymap / major-mode."

  ;; we start with a mass let*
  ;; then a an if / progn that goes on all along...

  (let* ((candidate)      ;; if helm mode, a list element...
         (keyvect)        ;; keys from the prompt
         (majorkeys)      ;; keys from major mode
         (major-keymap)   ;; major-mode keymap
         (all-shortcuts)) ;; shortcuts from anywhere.

  (if (and (symbolp bind) (not (eq bind 'digit-argument))
                          (not (eq bind 'nu-help-about-prompts)))
     (progn

        ; insert the button
        (if (string= nu-describe-bind-mode "buffer")
            (insert-button (symbol-name bind) 'action 'nu-prompt-describe)
            (setq candidate (symbol-name bind)))

        ; insert shortcuts _from the prompt_
        (setq keyvect (where-is-internal bind (list nu-current-keymap)))
        (if (not (eq nil keyvect))
           (progn
              (if (string= nu-describe-bind-mode "buffer")
                 (insert
                    (propertize
                       (format " %s"
                          (mapconcat 'key-description keyvect ", "))
                              'face 'bold))
                 (setq candidate (concat candidate " "
                    (propertize
                         (format " %s"
                          (mapconcat 'key-description keyvect ", "))
                              'face 'bold))))))


    ;; hack : is there a major-mode map?. FIXME.
    (setq major-keymap
        (eval (intern-soft
                  (concat (symbol-name nu-current-major-mode) "-map"))))

   ;; TODO : make the regexp replace one or two C-c at beginning only
   ;; (since where-is-internal does not know our sorcery)

  (if (not (keymapp major-keymap))
      (setq majorkeys "")
      (setq majorkeys
           (replace-regexp-in-string "\\(C-c\\)" "C-<SPC>"
               (mapconcat 'key-description (where-is-internal
                     bind (list major-keymap)) "@"))))

   ;; print the _direct keys_  (remove menu, menu-bar, f1, help, ..)
   ;; use non-greedy "*?"

   (setq all-shortcuts
      (replace-regexp-in-string "\\(<menu>\\|<menu-bar>\\|<f.>\\|<help>\\).*?@" ""
        (format "%s@"
          (concat
           (mapconcat 'key-description (where-is-internal bind nu-keymap) "@")
            "@"
            majorkeys))))
   (if (> (string-width all-shortcuts) 1)
   (progn
     (setq all-shortcuts (replace-regexp-in-string "@" " " all-shortcuts))
     (if (string= nu-describe-bind-mode "buffer")
         (insert " - or " all-shortcuts)
         (setq candidate (concat candidate " - or " all-shortcuts)))))

   ; now it's over. Just append a \n...
   (if (string= nu-describe-bind-mode "buffer")
       (insert "\n")
           (setq nu-keymap-list (cons candidate nu-keymap-list)))))))


(defun nu-insert-binding-row (ev bind)
 "If bind is a function, insert into *Help* its doc.
If bind is a Meta char list bound keys,
call insert description for each bind."

  ; keymap has different format.
  ; immediate below code should implement this
  ; sanitize bind whenever it makes sense

  ; for now, handle M- shortcuts...
  (if (symbolp bind)
      (nu-describe-bind bind)
      (progn
        (setq bind (cdr bind)) ; bind is now a list
        (while (not (eq nil (car bind)))
             (nu-describe-bind (cdr (car bind)))
             (setq bind (cdr bind))))))





; buffer-prompt is a heavy description
; of a prompt keymap.
; it uses *Help* buffer
; and was previously
; the standard nu-mode prompter
(defun nu-buffer-prompt-for-keymap (keymap &optional describe)
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
 (setq nu-describe-bind-mode "buffer")

 (let* ((key)
        (defn)
        (prefixhelp)
        (new-frame)
        (prev-frame (selected-frame))
        (config (current-window-configuration))
        (local-map (make-sparse-keymap)))
 (setcdr local-map keymap)
 (define-key local-map [t] 'undefined)

 (with-help-window (help-buffer)
  (with-current-buffer "*Help*"
      (if (eq nil current-prefix-arg)
          (setq prefixhelp "X")
          (setq prefixhelp current-prefix-arg))
   (if describe
       (insert
"In a standard prompt, press the associated key to run the function.
Use space or del to scroll down or up.
Press ? to obtain this screen.

From this prompt, press the associated key
to describe the function.\n")
       (insert
         (concat
               (propertize (format "Prefix = %s" prefixhelp) 'face 'shadow)
               (propertize "\nPress ? for help or to describe function\n" 'face 'italic))))
   (map-keymap 'nu-insert-binding-row keymap)
   (insert "\n\n\n"))


 (switch-to-buffer-other-window "*Help*")
 (setq new-frame (window-frame (selected-window))))

 (setq cursor-in-echo-area t)
 (setq input nil)
 (setq defn nil)
   (while (not input)
      (setq key (read-key-sequence (propertize "Enter a key or ? :" 'face 'italic) t))
      (cond

       ; check if the user needs to scroll the help. Do not break loop.
       ((eq (aref key 0) ?\d)
        (ignore-errors
          (scroll-down nil)))
       ((eq (aref key 0) ?\s)
        (ignore-errors
          (scroll-up nil)))

       ; allow to repeat prompt
       ((string= key "+")
        (progn (message "plus")
               (setq nu-repeat-prompt t)))

       ; check for negative / digit-argument.
       ((string= (key-description key) "-")
        (cond ((integerp current-prefix-arg)
               (setq current-prefix-arg (- current-prefix-arg)))
              ((eq current-prefix-arg '-)
               (setq current-prefix-arg nil))
              (t
               (setq current-prefix-arg '-)))

       (with-current-buffer "*Help*"
         (goto-char (point-min))
         (read-only-mode -1)
         (while (re-search-forward "\\`Prefix = .*?\n" nil t)
         (replace-match (propertize (format "Prefix = %s\n" current-prefix-arg) 'face 'underline)))))

       ((and (stringp (key-description key))
             (string-match (key-description key) "[0123456789]"))
          (cond
            ((eq current-prefix-arg '-)
             (setq current-prefix-arg (- (string-to-number (key-description key)))))
            ((integerp current-prefix-arg)
             (setq current-prefix-arg (+ (string-to-number (key-description key))
                                         (* current-prefix-arg 10))))
             (t
              (setq current-prefix-arg (string-to-number (key-description key)))))
       (with-current-buffer "*Help*"
         (goto-char (point-min))
         (read-only-mode -1)
         (while (re-search-forward "\\`Prefix = .*?\n" nil t)
         (replace-match (propertize (format "Prefix = %s\n" current-prefix-arg) 'face 'underline)))))

        ; now, break the loop, no matter a func has been found or not.
        ; eg the user can type not-mapped key to quit. "q" is never boundp.

       (t
         (progn
         (setq defn (lookup-key local-map key))
         (message "")
         (set-window-configuration config)

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


(defun nu-helm-prompt-for-keymap (keymap)
  "Use helm mode to prompt for a keymap.

This one is a bit different..."
 (interactive)
   (setq nu-current-keymap keymap
         nu-current-major-mode major-mode
         nu-keymap-list nil
         nu-describe-bind-mode "helm")
   (map-keymap 'nu-insert-binding-row keymap)
   (setq nu-last-command
      (intern-soft
          (replace-regexp-in-string "\\(\\w\\) .*" "\\1"
             (helm-comp-read "Execute :" nu-keymap-list
                             :must-match t))))
   (ignore-errors (call-interactively nu-last-command))
   (setq nu-repeat-prompt nil))



(defun ~nu-check-vector (vect value &optional is-string str-match)
"check if vector is the same as value.

Normally use equal, but if is-string is true, use string=.
If str-match is true, use string-match.

This function ensures no error can occur inside the process..."
  (if (> (length vect) 1)
      (> (length vect) 1) ; #false, too long vect
      (if is-string
          (if (not (integerp (elt vect 0)))
             (integerp (elt vect 0)) ; #false, not an integer
             (if (not (char-valid-p (elt vect 0)))
                 (char-valid-p (elt vect 0)) ; #false invalid byte
                 (if str-match
          ; string + str match
                     (string-match (byte-to-string (elt vect 0)) value)
          ; string but not str match
                     (string= (byte-to-string (elt vect 0)) value))))
          ; not a string. check equality.
          (equal (elt vect 0) value))))


(defun nu-light-prompt-for-keymap  (keymap &optional describe)
"Light prompt for a keymap. Toggle buffer-prompt with ?"
  (interactive)
  (message "light prompt")
  (setq nu-current-keymap keymap)
  (setq nu-current-major-mode major-mode)
  (let* ((input nil)
         (defn nil)
         (key)
         (local-map (make-sparse-keymap)))
    (setcdr local-map keymap)
    (define-key local-map [t] 'undefined)
  (catch 'outide
    (while (not input)
      (setq key (read-key-sequence-vector (propertize "Enter a key or SPC or TAB :" 'face 'italic) t))
      (cond
        ; allow to repeat prompt
        ((~nu-check-vector key "+" t)
         (setq nu-repeat-prompt t))

        ((~nu-check-vector key 'tab)
                (nu-buffer-prompt-for-keymap keymap))

        ((~nu-check-vector key " " t)
               (nu-helm-prompt-for-keymap keymap)
               (throw 'outside "another prompt is used."))

        ; check for negative / digit-argument.
        ((~nu-check-vector key "-" t)
           (cond ((integerp current-prefix-arg)
               (setq current-prefix-arg (- current-prefix-arg)))
              ((eq current-prefix-arg '-)
               (setq current-prefix-arg nil))
              (t
               (setq current-prefix-arg '-))))

        ; digits
         ((~nu-check-vector key "[0123456789]" t t)
          (cond
            ((eq current-prefix-arg '-)
             (setq current-prefix-arg (- (string-to-number (byte-to-string (elt key 0))))))
            ((integerp current-prefix-arg)
             (setq current-prefix-arg (+ (string-to-number (byte-to-string (elt key 0)))
                                         (* current-prefix-arg 10))))
             (t
              (setq current-prefix-arg (string-to-number (byte-to-string (elt key 0)))))))
       (t
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
            (setq nu-repeat-prompt nil)))))) "normal exit value")))

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
