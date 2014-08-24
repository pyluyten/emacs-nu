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


; we have the real keymap for real ("nu-keymap").
; menu-map is only there for where-is sake.
; thus we define things twice :
; once a grand tragedy. Once as a farce.
(defvar nu-keymap (make-sparse-keymap) "Emacs nu keymap")
(defvar nu-menu-map (make-sparse-keymap) "Nu Menu Keymap")

(require 'nu-help)

; external parts
(require 'transpose-frame) ; play with frames
(require 'nu-tile) ; still...

; internal parts
(require 'nu-prompts) ; prompters (menu-like)
(require 'nu-commands) ; not emacs native commands
(require 'nu-hooks) ; how to use other modes

(provide 'nu-common)
