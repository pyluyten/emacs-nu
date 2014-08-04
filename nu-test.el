(require 'button)


(defvar nucurrentkeymap nil)
(defvar nucurrentfunction nil)



(defun rototo (nu-function some-keymap)
  "insert a button describing function!"
  (insert-button nu-function 'action
    (lambda (x) 
       (describe-function (intern x)))))


; basique de chez basique : voir ci dessous pourqoi nécessaire :(
(defun nu-describe-function ()
  "describe-function."
)


; for a given binding, display stuff...
(defun nu-insert-binding-row (ev bind)
 "insert some link, the binding, the global binding"
 (if (symbolp bind)
  (progn 
    (insert-button (symbol-name bind)
           'action (nu-describe-function))
    (insert " : " ev "\n"))))


(defun nu-prompt-for-keymap (nukeymap)
 "display a prompter with buttons. Cool ain't it."
 (setq nucurrentkeymap nukeymap)
 (with-help-window (help-buffer)
  (with-current-buffer "*Help*"
   (insert "Press one of the below key, q to quit, <space> to scroll down, <backspace> to scroll up, or ? to panic : \n" )
   (map-keymap 'nu-insert-binding-row nukeymap)))
)



(defun argile ()
  (interactive)
  (nu-prompt-for-keymap nu-delete-map))





