; état des lieux
; je peux balayer une keymap
; et indiquer fonctions & bindings
; attentions les modifiers ne marchent pas !!
;
; ensuite je peux en faire un bouton,
; mais je ne sais pas le rendre cliquable
; puisqu'il faut aller recherher le mot cliqué ^^
;
;
; encore, where-is-internal m'indique des bindings,
; mais pas leur description en chaine de caractère
; ce qui serait peut être l'essentiel en réalité...


(require 'button)


(defvar nucurrentkeymap nil)
(defvar nucurrentfunction nil)



(defun rototo (nu-function some-keymap)
  "insert a button describing function!"
  (insert-button nu-function 'action
    (lambda (x) 
       (describe-function (intern x)))))


; basique de chez basique : voir ci dessous pourqoi nécessaire :(
(defun nu-describe-function (but)
  "describe-function."
  (message "well...")
  (message but))


; for a given binding, display stuff...
(defun nu-insert-binding-row (ev bind)
 "insert some link, the binding, the global binding, CR."
 (if (symbolp bind)
  (progn 
    (insert-button (symbol-name bind)
           'action 'nu-describe-function)
    (insert " : " ev)
    (if (not (eq nil (where-is-internal bind nu-keymap)))
      (insert
      (format " or %s"
        (mapconcat 'key-description
          (where-is-internal bind nu-keymap) ", "))))
    (insert "\n"))))


; marche mais pas dans argile???
(if (not (eq nil (where-is-internal 'kill-word nu-keymap)))
   (format "%s"
    (mapconcat 'key-description
          (where-is-internal 'kill-word nu-keymap) ", ")))


; voir where-is pour la soluce. bouh.
(format "%s" 
  (mapconcat 'key-description
    (where-is-internal 'kill-line nu-keymap) ", "))


(defun nu-prompt-for-keymap (nukeymap)
 "display a prompter with buttons. Cool ain't it."
 (setq nucurrentkeymap nukeymap)
 (with-help-window (help-buffer)
  (with-current-buffer "*Help*"
   (insert "Press one of the below key, q to quit, <space> to scroll down, <backspace> to scroll up, or ? to panic : \n" )
   (map-keymap 'nu-insert-binding-row nukeymap))))



(defun argile ()
  (interactive)
  (nu-prompt-for-keymap nu-delete-map))





