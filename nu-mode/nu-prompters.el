
;;; nu-mode.el --- Modern Emacs Keybinding
;;; Emacs-Nu is an emacs mode which wants to makes Emacs easier.kk
;;; Copyright (C) 2017 Pierre-Yves LUYTEN
;;;  
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;  
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;  
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA



; defconst?
(defvar nu-state t "Used by overriding maps alist.")

(defun nu-make-overriding-map (keymap unbind-keys-list &rest bindings)
   "Make keymap a minor-mode-overriding-map.

Unbind any key as in unbind-keys-list.
Add a binding for any binding provide on the form
   C-l     'some-function."
 (make-local-variable 'minor-mode-overriding-map-alist)
 (push `(nu-state . ,keymap) minor-mode-overriding-map-alist)
 (while (not (eq unbind-keys-list nil))
    (define-key keymap (kbd (car unbind-keys-list)) nil)
    (pop unbind-keys-list))
 (while (not (eq bindings nil))
    (define-key keymap (kbd (car bindings)) (car (cdr bindings)))
    (pop bindings) (pop bindings)))

(defun nu-drop-overriding-map (keymap)
    "Remove keymap from minor-mode-overriding-map."
  (make-local-variable 'minor-mode-overriding-map-alist)
  (setq minor-mode-overriding-map-alist (delete `(nu-state . ,keymap) minor-mode-overriding-map-alist)))


(defun nu-add-keymap (keymap)
  "Adds keymap to the list of nu-mode keymaps.

Currently this is only used in order to use read-key-sequence."
  (make-local-variable 'minor-mode-map-alist)
  (push `(nu-mode . ,keymap) minor-mode-map-alist))

(defun nu-remove-keymap (keymap)
  "Removes keymap from nu-mode ones."
  (make-local-variable 'minor-mode-map-alist)
  (setq minor-mode-map-alist (delete `(nu-mode . ,keymap) minor-mode-map-alist)))

 ; map-keymap has no way to receive
 ; more than two args
 ; we cannot easily communicate
 ; to this function which keymap it is parsing (!)
 ; thus, use a global var
 ;
 ; (current-global-map) has to be 
 ; stocked for similar reasons.

 ; about nu-major mode
 ; currently only light-prompt/hydra 
 ; does set nu-major-mode
 ; this is fine as long as
 ; light prompt and/or hydra
 ; are the sole entry points to prompts
 ; if that changes, this variable
 ; might have to be reviewed

(defvar nu-current-keymap nil)
(defvar nu-current-local-map nil)
(defvar nu-major-mode nil)
(defvar nu-lv-message nil)

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
"This function is used with map-keymap.
For each element in keymap ('bind'),
it does describe the element.

This function acts differently according to
global variable : nu-describe-bind.
if nu-describe-bind='buffer', then *help*
         buffer is used to describe binding.

if nu-describe-bind = helm, then 
       a candidate list is built.

if nu-describe-bind= lv, then a lv
  message string is built.

This includes symbol name, key(s) from prompt,
and drect keys from both nu-keymap / major-mode."

  (let* ((candidate)      ;; if helm mode, a list element...
         (keyvect)        ;; keys from the prompt
         (majorkeys)      ;; keys from major mode
	 (nu-lv-row)         ;; if lv mode, a string...
         (all-shortcuts)) ;; shortcuts from anywhere.

  (if (and (symbolp bind) (not (eq bind 'digit-argument))
                          (not (eq bind 'nu-help-about-prompts)))
     (progn

	; insert the button
       (cond
           ((string= nu-describe-bind-mode "buffer")
	    (insert-button (symbol-name bind) 'action 'nu-prompt-describe))

	   ((string= nu-describe-bind-mode "lv")
	    (setq nu-lv-row (symbol-name bind)))
	   
	  ((string= nu-describe-bind-mode "completion")
	   (setq candidate (symbol-name bind))))

	; insert shortcuts _from the prompt_
        ; for HELM this is not useful since HELM uses completion       
        (setq keyvect (where-is-internal bind (list nu-current-keymap)))
        (if (not (eq nil keyvect))
            (cond
	       ((string= nu-describe-bind-mode "buffer")
                 (insert
                    (propertize
                       (format " %s"
                          (mapconcat 'key-description keyvect ", "))
		       'face 'nu-face-shortcut)))
	  
	      ((string= nu-describe-bind-mode "lv")
	       (setq nu-lv-row
		     (concat nu-lv-row
			(propertize
			  (format " %s"
                             (mapconcat 'key-description keyvect ", "))
                          'face 'nu-face-shortcut))))))

   ;; TODO : make the regexp replace one or two C-c at beginning only
   ;; (since where-is-internal does not know our sorcery)

  (if (not (keymapp nu-current-local-map))
      (setq majorkeys "")
      (setq majorkeys
           (replace-regexp-in-string "\\(C-c\\)" "C-<SPC>"
               (mapconcat 'key-description (where-is-internal
                     bind (list nu-current-local-map)) "@"))))

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
     (setq all-shortcuts
	 ;; replace % to avoid bug with format/propertize.
	 (replace-regexp-in-string "%" "<percent>"
	   (replace-regexp-in-string "@" " " all-shortcuts)))
     (cond
         ((string= nu-describe-bind-mode "buffer")
	  (insert " - or " all-shortcuts))

	 ((string= nu-describe-bind-mode "lv")
	  (setq nu-lv-row (concat nu-lv-row " - or " all-shortcuts)))

	 ((string= nu-describe-bind-mode "completion")
          (setq candidate (concat candidate " - or " all-shortcuts))))))
   
   ; now it's over. Just append a \n...
   (cond
    ((string= nu-describe-bind-mode "buffer")
     (insert "\n"))

    ((string= nu-describe-bind-mode "lv")
     (setq nu-lv-message (concat nu-lv-message nu-lv-row "\n")))
    ((string= nu-describe-bind-mode "completion")
     (setq nu-keymap-list (cons candidate nu-keymap-list))))))))


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

(provide 'nu-prompters)
