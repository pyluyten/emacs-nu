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

; buffer-prompt is a heavy description
; of a prompt keymap.
; it uses *Help* buffer
; and was previously
; the standard nu-mode prompter
(defun nu-buffer-prompt-for-keymap (keymap &optional describe)
 "Help to choose a key from a keymap

If describe arg is t, only describe-function."

 ; map-keymap has no way to receive
 ; more than two args
 ; we cannot easily communicate
 ; to this the keymap we are parsing (!)
 ; thus, use a global var
 ;
 ; also, include major mode keys.
 (setq nu-current-keymap keymap
       nu-describe-bind-mode "buffer")
 
 (let* ((key)
        (defn)
        (prefixhelp)
        (new-frame)
        (prev-frame (selected-frame))
        (config (current-window-configuration))
        (local-map (make-sparse-keymap)))
 (setcdr local-map keymap)
 (define-key local-map [t] 'undefined)

 (with-help-window (help-buffer)
  (with-current-buffer "*Help*"
      (if (eq nil current-prefix-arg)
          (setq prefixhelp "X")
          (setq prefixhelp current-prefix-arg))
   (if describe
       (insert
"In a standard prompt, press the associated key to run the function.
Use space or del to scroll down or up.
Press ? to obtain this screen.

From this prompt, press the associated key
to describe the function.\n")
     (insert
	  (concat
               (propertize (format "Prefix = %s" prefixhelp) 'face 'shadow)
               (propertize "\nPress ? for help or to describe function\n" 'face 'italic))))
   (insert (concat "Major mode = " (symbol-name nu-major-mode) "\n"))
   (map-keymap 'nu-insert-binding-row keymap)
   (insert "\n\n\n"))


 (switch-to-buffer-other-window "*Help*")
 (setq new-frame (window-frame (selected-window))))

 (setq cursor-in-echo-area t)
 (setq input nil)
 (setq defn nil)
   (while (not input)
      (setq key (read-key-sequence-vector (propertize "Enter a key or ? :" 'face 'italic) t))
      (cond

       ; check if the user needs to scroll the help. Do not break loop.
       ((~nu-check-vector key ?\d nil)
        (ignore-errors
          (scroll-down nil)))
       ((~nu-check-vector key ?\s nil)
        (ignore-errors
          (scroll-up nil)))

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

       (with-current-buffer "*Help*"
         (goto-char (point-min))
         (read-only-mode -1)
         (while (re-search-forward "\\`Prefix = .*?\n" nil t)
         (replace-match (propertize (format "Prefix = %s\n" current-prefix-arg) 'face 'underline)))))

       ((~nu-check-vector key "[0123456789]" t t)
          (cond
            ((eq current-prefix-arg '-)
             (setq current-prefix-arg (- (string-to-number (key-description key)))))
            ((integerp current-prefix-arg)
             (tsetq current-prefix-arg (+ (string-to-number (key-description key))
                                         (* current-prefix-arg 10))))
             (t
              (setq current-prefix-arg (string-to-number (key-description key)))))
       (with-current-buffer "*Help*"
         (goto-char (point-min))
         (read-only-mode -1)
         (while (re-search-forward "\\`Prefix = .*?\n" nil t)
         (replace-match (propertize (format "Prefix = %s\n" current-prefix-arg) 'face 'underline)))))

        ; now, break the loop, no matter a func has been found or not.
        ; eg the user can type not-mapped key to quit. "q" is never boundp.

       (t
         (progn
         (setq defn (lookup-key local-map key))
         (message "")
         (set-window-configuration config)

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


(provide 'nu-prompters-buffer)
