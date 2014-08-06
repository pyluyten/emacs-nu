;
; les pistes par rapport au "define func"
; 1. créer un help bouton à l'aide d'une fonction déjà existante
;    ou encore un clickable text.
; 2. créer une référence `ma-fonc' et demander au help buffer de faire
;    le travail
; 3. proposer une autre manière de consulter les fonctions...
;    par exemple la touche . est réservée à l'actuel ?
;    et le ? sert à describe-func sur la keymap en cours
;    
;
; TODO
; (fix bugs)
; clickable functions
; 
; 
;  "27" is ^[ is escape but is actually meta. Uh?
;
;
; on ne peut pas "advice" une 'map'. Seulement une fonction stricto sensu.

;(require 'button)



(defun nu-help-about-prompts ()
"Provides some information about prompts."
 (interactive)
 (with-help-window (help-buffer)
  (with-current-buffer "*Help*"
    (insert
"A prompt appears when you press a `prefix' command.

You can enter a key to trigger a function. The prompts
informs you about which keys are available.

Use <space> / <backspace> to scroll the prompt.

Use control+h then f to trigger describe-function,
then enter the function you want to describe."))))



(defun nu-define-prefix (arg)
"Define a prefix command, assign ? key."
 (define-prefix-command arg)
 (define-key arg (kbd "?") 'nu-help-about-prompts))

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
;        (insert (symbol-name bind))
; make-text-button ((point) nil 
        ;(insert-button (symbol-name bind) 'help-function (symbol-function bind))
                                     ;match-number 1 , type 'help-button, arg "def" ie symbol-function
        (insert-button (symbol-name bind) 'help-function (symbol-function bind))
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



;(defun argile ()
;  (interactive)
;  (nu-prompt-for-keymap nu-find-map))

;(nu-prompt-for-keymap nu-delete-map)

(provide 'nu-help)
