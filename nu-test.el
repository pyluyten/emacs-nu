where-is-internal command &optional keymap firstonly noindirect no-remap


(require 'button)


; poc à 2 balles
(where-is-internal 'previous-line nu-keymap nil t t)




 
; basique de chez basique : voir ci dessous pourqoi nécessaire :(
(defun nu-describe-function (fname some-keymap)
  "describe-function."
  (describe-function (intern fname))
  (where-is-internal 'previous-line nu-keymap nil t t))

; nu-function, si passé à une lambda, ne sera pas évalué
; (dans intern ou intern-soft)
;
; il faut donc passer une chaine de caractère
; or, je ne sais pas récupérer la
; "description" du bouton
; (soit lip est nul soit pb de doc)
;
;
; on passe donc par nu-describe-function
(defun rototo (nu-function some-keymap)
  "insert a button describing function!"
  (insert-button nu-function 'action
    (nu-describe-function nu-function some-keymap)))


; POC - ça marche.
(rototo "forward-char" nu-keymap)



; for a given binding, display stuff...
(defun salto (ev bind)
 "do a triple axel loop."
 (if (symbolp bind)
   (insert (symbol-name bind) " : " ev "\n")))


(defvar nucurrentkeymap nil)

(defun yo (nukeymap)
 (interactive)
 ;below is tmp global variable because i cannot have enough args.
 ; this sucks.
 (setq nucurrentkeymap nukeymap)

 ; now do the work : prompt then for each bindign display stuff
 (with-help-window (help-buffer)
  (with-current-buffer "*Help*"
   (insert "Press one of the below key, q to quit, <space> to scroll down, <backspace> to scroll up, or ? to panic : \n" )
   (map-keymap 'salto nu-delete-map)))
)







