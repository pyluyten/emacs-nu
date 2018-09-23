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

	(lv-delete-window)
	(setq nu-lv-message (replace-regexp-in-string "Prefix = .*?\n" (format "Prefix = %s\n" current-prefix-arg) nu-lv-message))
	(lv-message nu-lv-message))

       ((~nu-check-vector key "[0123456789]" t t)
          (cond
            ((eq current-prefix-arg '-)
             (setq current-prefix-arg (- (string-to-number (key-description key)))))
            ((integerp current-prefix-arg)
             (setq current-prefix-arg (+ (string-to-number (key-description key))
                                         (* current-prefix-arg 10))))
             (t
              (setq current-prefix-arg (string-to-number (key-description key)))))

	  (lv-delete-window)
	  (setq nu-lv-message (replace-regexp-in-string "Prefix = .*?\n" (format "Prefix = %s\n" current-prefix-arg) nu-lv-message))
	   (lv-message nu-lv-message))

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

(provide 'nu-prompters-lv)
