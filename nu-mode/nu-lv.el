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


(defhydra hydra-nu-meta-menu (:color pink
                              :hint nil)
"
_q_ quit any prompt

PADDLE
----------------------------------------------------------------
_i_: open file  _j_: recent     _k_: delete window   _l_: next buffer
_u_: bookmarks  _m_: maximize   _<SPC>_ : ibuffers

PROMPTS
----------------------------------------------------------------
_a_ select       _r_ replace    _o_ open     _g_ goto
_h_ help         _f_ find       _p_ print    _s_ save
_n_ new
_d_ delete

OTHER
--------------------------------------------------------------
_e_ execute command   or use M-x
_z_ undo tree         _t_ tab (what emacs calls window)
"
    ;; paddle direct functions.
    ("i" nu-find-files :exit t)
    ("l" nu-next-buffer :exit t)
    ("k" kill-region :exit t)
    ("j" nu-recentf :exit t)
    ("u" nu-bookmarks :exit t)
    ("m" delete-other-windows :exit t)
    ("<SPC>" ibuffer :exit t)

    ;; nu prompts
    ("a" (nu-buffer-prompt-for-keymap nu-a-map) :exit t)
    ("r" (nu-buffer-prompt-for-keymap nu-replace-map) :exit t)
    ("o" (nu-buffer-prompt-for-keymap nu-open-map) :exit t)
    ("g" (nu-buffer-prompt-for-keymap nu-goto-map) :exit t)
    ("h" (nu-buffer-prompt-for-keymap nu-help-map) :exit t)
    ("f" (nu-buffer-prompt-for-keymap nu-find-map) :exit t)
    ("p" (nu-buffer-prompt-for-keymap nu-print-map) :exit t)
    ("s" (nu-buffer-prompt-for-keymap nu-save-map) :exit t)
    ("d" (nu-buffer-prompt-for-keymap nu-delete-map) :exit t)
    ("n" (nu-buffer-prompt-for-keymap nu-new-map) :exit t)
    ("t" (nu-buffer-prompt-for-keymap nu-tab-map) :exit t)

    ;; other
    ("z" undo-tree-visualize :exit t)
    ("e" nu-M-x :exit t)
    ("y" nil :exit t)
    ("w" nil :exit t)
    ("x" nil :exit t)
    ("c" nil :exit t)
    ("v" nil :exit t)
    ("b" nil :exit t)
    ("q" nil :exit t))

(provide 'nu-lv)
