; état des lieux
; je peux balayer une keymap
; attentions les modifiers ne marchent pas !!
;
; ensuite je peux en faire un bouton,
; mais je ne sais pas le rendre cliquable
; (pas très grave!)

(require 'button)


; a button is no good,
; its not that well documented how to retrieve its text (!)
(defun rototo (nu-function some-keymap)
  "insert a button describing function!"
  (insert-button nu-function 'action
    (lambda (x) 
       (describe-function (intern x)))))

; for a given binding, display stuff...
(defun nu-insert-binding-row (ev bind)
 "insert some link, the binding, the global binding, CR."
 (if (symbolp bind)
  (progn 
    (insert ev " : ")
;    (insert-button (symbol-name bind)
;           'action 'nu-describe-function)
    (insert (symbol-name bind))
    (if (not (eq nil (where-is-internal bind nu-keymap)))
      (insert
      (format ", %s"
        (mapconcat 'key-description
          (where-is-internal bind nu-keymap) ", "))))
    (insert "\n"))))


(defun nu-prompt-for-keymap (nukeymap)
 "display a prompter with buttons. Cool ain't it."
 (with-help-window (help-buffer)
  (with-current-buffer "*Help*"
   (insert "Press one of the below key, or ?\n" )
   (map-keymap 'nu-insert-binding-row nukeymap))))


(defun argile ()
  (interactive)
  (nu-prompt-for-keymap nu-delete-map))





