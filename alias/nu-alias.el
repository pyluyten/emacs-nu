;
;    Pierre-Yves Luyten           2014.
;
;
;    This file is part of Nu.
; 
;    Nu is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
; 
;    Nu is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
; 
;    You should have received a copy of the GNU General Public License
;    along with Nu.  If not, see <http://www.gnu.org/licenses/>.
;
;
;
;    ~~~ Principle
;    This was a bare trial but it appears its quite simple to
;    define aliases from strings. Thus, from a file.
;    Thus, from a list of files.
;
;    ~~~ Wishlist
;    This is already nice because even a dot.org file is better
;    to maintain than defalias list.
;    Also, we can leverage this to memorize alias which we're creating
;    Next step is to display it and allow user to add
;    its own file more dynamically...
;
;    function completion while editing is also important.


; core

(require 'org)

(defvar nu-aliases (make-hash-table :test 'equal)
 "Hash table of nu aliases.

Each nu alias is a string of three elements,
the alias symbol, the function it's an alias for,
the optional docstr, & the file it was defined in.

(Hash keys are the alias.)

Call nu-alias-list-aliases to display existing aliases
Call nu-alias-add-file to add a file to this table.
Call nu-alias-parse-aliases to reload every file.")



(defvar nu-alias-files-list (make-hash-table :test 'equal)
"Hash table of files containing aliases.
See nu-aliases.")


(defun nu-alias-list-aliases ()
"List current nu aliases in a dedicated buffer.

Aliases are links to the files defining them
thus use org open link
normally bound to C-c C-
to visit the file."
  (interactive)
  (let ((buf (generate-new-buffer "Nu Aliases")))
    (switch-to-buffer buf)
    (maphash
      (lambda (key value)
	(insert "| ")
	(org-insert-link nil (nth 3 value) (symbol-name (nth 0 value)))
	(insert (concat
	            " | "  (symbol-name (nth 1 value))
	            " | "  (nth 2 value) "\n"))) nu-aliases)
    (org-mode)
    (previous-line)
    (org-table-align)
    (beginning-of-buffer)))



(defun nu-alias-defalias-from-strings (filename alname funame &optional docstr)
  "Make an alias from strings.

Do not call this directly, this is made to be called by others."
  (interactive)
  (setq el (list (intern alname) (intern-soft funame) docstr filename))
  (puthash (nth 0 el) el nu-aliases)
  (defalias (nth 0 el) (nth 1 el) (nth 2 el)))


(defun nu-alias-read-lines (fullname)
  "Return a list of lines of a file <fullname>."
  (interactive)
  (with-temp-buffer
    (insert-file-contents fullname)
    (split-string (buffer-string) "\n" t)))

(defun nu-alias-parse-aliases ()
"Use nu-alias-files-list to parse aliases.

The function starts undefining all current alias.
Then, it goes throught every file in the list
and for each row, try to create an alias from this row."
  (interactive)
  ;; undefine current aliases. Might be risky ;)
  (maphash
     (lambda (key value)
       (unintern key))
       nu-aliases)
  ;; empty the list
  (clrhash nu-aliases)
  ;; check every row of every file
  (maphash
    (lambda (key value)
      (setq parser (nu-alias-read-lines key))
      ;; define the alias & add it to the list
      (while parser
        (setq row (split-string (car parser) "|"))
        (nu-alias-defalias-from-strings
	  value
          (replace-regexp-in-string " " "" (nth 1 row))
          (replace-regexp-in-string " " "" (nth 2 row))
          (nth 3 row))
          (setq parser (cdr parser))))
    nu-alias-files-list))



(defun nu-alias-add-file (filenamestr)
  (interactive)
  (puthash filenamestr filenamestr nu-alias-files-list)
  (nu-alias-parse-aliases))


(defun nu-alias-remove-file (filenamestr)
  (interactive)
  (remhash filenamestr nu-alias-files-list)
  (nu-alias-parse-aliases))



(provide 'nu-alias)
