;
; TODO
; (fix bugs)
; clickable functions
; 
; 
;  "27" is ^[ is escape but is actually meta. Uh?


;(require 'button)
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
        (insert (symbol-name bind))
        (if (not (eq nil (where-is-internal bind nu-keymap)))
          (insert
          (format ", %s"
            (mapconcat 'key-description
              (where-is-internal bind nu-keymap) ", "))))
        (insert "\n"))))
  ; if bind is itself a nested keymap,
  ; first print modifier, then run self.
  ;    (progn
  ;        (if (eq ev 27)
  ;            (insert "Alt+"))
  ;        (map-keymap 'nu-insert-binding-row bind))))



(defun nu-prompt-for-keymap (nukeymap)
 "display a prompter with buttons."
 (with-help-window (help-buffer)
  (with-current-buffer "*Help*"
   (insert "Press one of the below key, or ?\n" )
   (map-keymap 'nu-insert-binding-row nukeymap)))
 (set-temporary-overlay-map nukeymap))



(defun argile ()
  (interactive)
  (nu-prompt-for-keymap nu-find-map))

(nu-prompt-for-keymap nu-delete-map)

