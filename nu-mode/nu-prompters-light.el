;;; nu-mode.el --- Modern Emacs Keybinding
;;; Emacs-Nu is an emacs mode which wants to makes Emacs easier.
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

(require 'nu-prompters)


(defun nu-light-prompt-for-keymap  (keymap &optional describe)
"Light prompt for a keymap. Toggle buffer-prompt with ?"
  (interactive)
  (setq nu-current-keymap keymap
	nu-major-mode major-mode
	nu-current-local-map (current-local-map))
  
  (let* ((input nil)
         (defn nil)
         (key)
         (local-map (make-sparse-keymap)))
    (setcdr local-map keymap)
    (define-key local-map [t] 'undefined)
  (catch 'outside
    (while (not input)
      ; read-key-sequence : in order to offer the user to enter
      ; the sequence he wants, we first need to activate the map
      ; this is why we make the keymap overriding.
      ; - as opposed to directly trigger the mapped func.
      ; after this, we're parsing the sequence to check
      ; if we should look for the mapped func
      ; or just do something else....

      (nu-add-keymap keymap)
      (setq key (read-key-sequence-vector
		  (concat
		      "Enter a key / "
                       (propertize "SPC" 'face 'nu-face-shortcut)
                       " for completion / "
                       (propertize "TAB" 'face 'nu-face-shortcut)
		       " for list / "
		       (propertize "q" 'face 'nu-face-shortcut)
		       " to quit "
		       (propertize ": " 'face 'nu-face-shortcut)) t))
      (nu-remove-keymap keymap)
      (cond
        ; allow to repeat prompt
        ((~nu-check-vector key "+" t)
         (setq nu-repeat-prompt t))

        ; check for tab ; with 9 i try to fix a bug on some platforms...
        ((or (~nu-check-vector key 'tab) (~nu-check-vector key 9))
                (nu-full-prompt-for-keymap keymap))

        ((~nu-check-vector key " " t)
	 (nu-completion-prompt-for-keymap keymap)
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

(provide 'nu-prompters-light)
