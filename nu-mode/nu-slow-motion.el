;;;
;;; Slow Motion : evil adaptation to fit emacs environment
;;; so slow motion is inverse of https://github.com/emacs-evil/evil-collection

;;; Changes vs Evil are
;;; - keys are consistent w/ emacs keys (eg y to paste or f forward-char)
;;; - use emacs native func rather than evil- when makes sense
;;; - insert state by default excepted fundamentals
;;; - a custom nu menu based on operator->motion logic
;;;   (this allows to avoid Modifiers even for advanced func like
;;;    the one usually found in C-x, despite C-x is still available)
;;; - 'alt' keys are an additional interface
;;;   (this proved to be pretty efficient, and alt keys are consistent w/
;;;    emacs meaning, and anyway original alt feature is no more needed
;;;    here, and it is still possible not to use alt keys obviously)

;;; see also nu-state.el
;;;


(defvar nu-no-emacs-breakage t
 "If true, do not customize modes, only handle nu-evil-menu.
  nu-mode or nu-state do not define this variable.")

(require 'nu-state)

(defun nu-slow-motion-help-prompt ()
  (interactive)
  (lv-message
    (concat
      (propertize "\n Welcome to nu-slow-motion\n\n" 'face 'bold)
      " This screen does provide some help to use slow motion.\n It is shown at startup.\n"
      " Enter any key to quit this prompt or "(propertize "SPC" 'face 'nu-face-shortcut)
      " to obtain the cheat sheet."
      "\n To disable this screen, put this in your init file\n\n"
        (propertize " (require 'nu-slow-motion)\n" 'face 'italic)
	(propertize " (setq nu-show-welcome-screen nil)\n" 'face 'error)
      "\n\n To obtain Help, use "
      (propertize "Control+h" 'face 'nu-face-shortcut)
      "\n For example, to obtain a Cheat Sheet, use "
      (propertize (substitute-command-keys "\\[nu-cheat-sheet]") 'face 'nu-face-shortcut)
      "\n Or press ² at any time.\n To enter a command, use "
      (propertize (substitute-command-keys "\\[nu-M-x]") 'face 'nu-face-shortcut)
      " like in vanilla Emacs)."))
  (setq answer (read-key ""))
  (lv-delete-window)
  (if (eq answer 32)
      (nu-cheat-sheet)))

(defun nu-slow-motion-set-keys-a-la-emacs ()
  "substitute to evil keys emacs like keybinding, but modal"

   (define-key evil-emacs-state-map (kbd "C-o") 'nu-open-prompt)

   (define-key evil-normal-state-map "a" nil) ; motion
   (define-key evil-normal-state-map "A" nil) ; motion
   (define-key evil-normal-state-map "b" nil) ; motion
   (define-key evil-normal-state-map "c" 'evil-change)
   (define-key evil-normal-state-map "C" 'evil-change-line)
   (define-key evil-normal-state-map "d" 'evil-delete-char)
   (define-key evil-normal-state-map "D" 'evil-delete-backward-char)
   (define-key evil-normal-state-map "g" nil)
   (define-key evil-normal-state-map "g&" 'evil-ex-repeat-global-substitute)
   (define-key evil-normal-state-map "g8" 'what-cursor-position)
   (define-key evil-normal-state-map "g?" 'evil-rot13)
   (define-key evil-normal-state-map "gF" 'evil-find-file-at-point-with-line)
   (define-key evil-normal-state-map "gJ" 'evil-join-whitespace)
   (define-key evil-normal-state-map "gU" 'evil-upcase)
   (define-key evil-normal-state-map "ga" 'what-cursor-position)
   (define-key evil-normal-state-map "gf" 'find-file-at-point)
   (define-key evil-normal-state-map "gi" 'evil-insert-resume)
   (define-key evil-normal-state-map "gq" 'evil-fill-and-move)
   (define-key evil-normal-state-map "gu" 'evil-downcase)
   (define-key evil-normal-state-map "gw" 'evil-fill)
   (define-key evil-normal-state-map "gx" 'browse-url-at-point)
   (define-key evil-normal-state-map "g~" 'evil-invert-case)
   (define-key evil-normal-state-map "h" nil) ; motion
   (define-key evil-normal-state-map "H" help-map)
   (define-key evil-normal-state-map "i" 'evil-insert)
   (define-key evil-normal-state-map "I" 'evil-insert-line)
   (define-key evil-normal-state-map "j" nil) ; motion
   (define-key evil-normal-state-map "J" 'evil-join)
   (define-key evil-normal-state-map "k" 'evil-delete)
   (define-key evil-normal-state-map "K" 'evil-delete-line)
   (define-key evil-normal-state-map "l" nil) ; motion
   (define-key evil-normal-state-map "L" nil) ; motion
   (define-key evil-normal-state-map "m" 'point-to-register)
   (define-key evil-normal-state-map "M" 'kmacro-start-macro)
   (define-key evil-normal-state-map "n" nil) ; TODO motion
   (define-key evil-normal-state-map "o" 'evil-open-below)
   (define-key evil-normal-state-map "O" 'evil-open-above)
   (define-key evil-normal-state-map "p" nil) ; motion
   (define-key evil-normal-state-map "P" nil) ; motion
   (define-key evil-normal-state-map "q" 'evil-append)
   (define-key evil-normal-state-map "Q" 'evil-append-line)
   (define-key evil-normal-state-map "r" 'evil-replace)
   (define-key evil-normal-state-map "R" 'evil-search-previous)
   (define-key evil-normal-state-map "s" 'nu-search)
   (define-key evil-normal-state-map "S" 'evil-search-next)
   (define-key evil-normal-state-map "t" nil) ; motion
   (define-key evil-normal-state-map "T" nil) ; motion
   (define-key evil-normal-state-map "u" 'undo)
   (define-key evil-normal-state-map "U" 'undo-tree-visualize)
   (define-key evil-normal-state-map "v" 'evil-visual-char)
   (define-key evil-normal-state-map "V" 'evil-visual-line)
   (define-key evil-normal-state-map "\C-v" 'evil-visual-block)
   (define-key evil-normal-state-map "w" 'evil-yank)
   (define-key evil-normal-state-map "W" 'evil-yank-line)
   (define-key evil-normal-state-map "x" nu-evil-map)
   (define-key evil-normal-state-map "X" 'nu-M-x)
   (define-key evil-normal-state-map "y" 'evil-paste-after)
   (define-key evil-normal-state-map "Y" 'evil-paste-before)
   (define-key evil-normal-state-map "z" 'evil-substitute)
   (define-key evil-normal-state-map "Z" 'evil-change-whole-line)
   (define-key evil-normal-state-map (kbd "<SPC>") 'evil-scroll-down)
   (define-key evil-normal-state-map (kbd "<DEL>") 'evil-scroll-up)
   (define-key evil-normal-state-map "~" 'evil-invert-char)
   (define-key evil-normal-state-map "\"" 'evil-use-register)
   (define-key evil-normal-state-map "\C-n" 'evil-paste-pop-next)
   (define-key evil-normal-state-map "\C-p" 'evil-paste-pop)
   (define-key evil-normal-state-map "\C-t" 'pop-tag-mark)
   (define-key evil-normal-state-map (kbd "C-.") 'evil-repeat-pop)
   (define-key evil-normal-state-map (kbd "M-.") 'evil-repeat-pop-next)
   (define-key evil-normal-state-map "&" 'evil-ex-repeat-substitute)
   (define-key evil-normal-state-map "." 'evil-repeat)
   (define-key evil-normal-state-map "<" 'evil-shift-left)
   (define-key evil-normal-state-map "=" 'evil-indent)
   (define-key evil-normal-state-map ">" 'evil-shift-right)
   (define-key evil-normal-state-map "@" 'evil-execute-macro)
   (define-key evil-normal-state-map "$" 'kmacro-end-or-call-macro)
   (define-key evil-normal-state-map "/" 'evil-replace-state)
   ;;(define-key evil-normal-state-map "z=" 'ispell-word)
   ;;(define-key evil-normal-state-map "zO" 'evil-open-fold-rec)
   ;;(define-key evil-normal-state-map "za" 'evil-toggle-fold)
   ;;(define-key evil-normal-state-map "zc" 'evil-close-fold)
   ;;(define-key evil-normal-state-map "zm" 'evil-close-folds)
   ;;(define-key evil-normal-state-map "zo" 'evil-open-fold)
   ;;(define-key evil-normal-state-map "zr" 'evil-open-folds)

   ;; MOTION STATE MAP ================================================
   ;; "0" is a special command when called first
   (define-key evil-motion-state-map "a" 'evil-beginning-of-line)
   (define-key evil-motion-state-map "A" 'evil-backward-sentence-begin)
   (define-key evil-motion-state-map "b" 'evil-backward-char)
   (define-key evil-motion-state-map "B" 'evil-backward-word-begin)
   (define-key evil-motion-state-map "e" 'evil-end-of-line)
   (define-key evil-motion-state-map "E" 'evil-forward-sentence-begin)
   (define-key evil-motion-state-map "f" 'evil-forward-char)
   (define-key evil-motion-state-map "F" 'evil-forward-word-begin)
   (define-key evil-motion-state-map "g" nil)
   (define-key evil-motion-state-map "gd" 'evil-goto-definition)
   (define-key evil-motion-state-map "ge" 'evil-backward-word-end)
   (define-key evil-motion-state-map "gE" 'evil-backward-WORD-end)
   (define-key evil-motion-state-map "gg" 'evil-goto-first-line)
   (define-key evil-motion-state-map "gj" 'evil-next-visual-line)
   (define-key evil-motion-state-map "gk" 'evil-previous-visual-line)
   (define-key evil-motion-state-map "g0" 'evil-beginning-of-visual-line)
   (define-key evil-motion-state-map "g_" 'evil-last-non-blank)
   (define-key evil-motion-state-map "g^" 'evil-first-non-blank-of-visual-line)
   (define-key evil-motion-state-map "gm" 'evil-middle-of-visual-line)
   (define-key evil-motion-state-map "g$" 'evil-end-of-visual-line)
   (define-key evil-motion-state-map "g\C-]" 'evil-jump-to-tag)
   (define-key evil-motion-state-map "g*" 'evil-search-unbounded-word-forward)
   (define-key evil-motion-state-map "h" 'evil-backward-WORD-begin)
   (define-key evil-motion-state-map "j" 'evil-forward-WORD-begin)
   (define-key evil-motion-state-map "k" nil)
   (define-key evil-motion-state-map "l" 'evil-forward-word-end)
   (define-key evil-motion-state-map "L" 'evil-forward-WORD-end)
   ;(define-key evil-motion-state-map "M" 'evil-window-middle)
   (define-key evil-motion-state-map "n" 'evil-next-line)
   (define-key evil-motion-state-map "N" 'evil-window-bottom)
   (define-key evil-motion-state-map "p" 'evil-previous-line)
   ;(define-key evil-motion-state-map "P" 'evil-lookup)
   (define-key evil-motion-state-map "P" 'evil-window-top)
   (define-key evil-motion-state-map "t" 'evil-find-char-to)
   (define-key evil-motion-state-map "T" 'evil-find-char-to-backward)
   (define-key evil-motion-state-map "w" 'evil-yank)
   (define-key evil-motion-state-map "W" 'evil-yank-line)
   (define-key evil-motion-state-map ")" 'evil-find-char)
   (define-key evil-motion-state-map "(" 'evil-find-char-backward)
   (define-key evil-motion-state-map "{" 'evil-backward-paragraph)
   (define-key evil-motion-state-map "}" 'evil-forward-paragraph)
   (define-key evil-motion-state-map "#" 'evil-search-word-backward)
   (define-key evil-motion-state-map "x#" 'evil-search-unbounded-word-backward)
   (define-key evil-motion-state-map "%" 'evil-jump-item)
   (define-key evil-motion-state-map "`" 'evil-goto-mark)
   (define-key evil-motion-state-map "'" 'evil-goto-mark-line)
   (define-key evil-motion-state-map "]]" 'evil-forward-section-begin)
   (define-key evil-motion-state-map "][" 'evil-forward-section-end)
   (define-key evil-motion-state-map "[[" 'evil-backward-section-begin)
   (define-key evil-motion-state-map "[]" 'evil-backward-section-end)
   (define-key evil-motion-state-map "[(" 'evil-previous-open-paren)
   (define-key evil-motion-state-map "])" 'evil-next-close-paren)
   (define-key evil-motion-state-map "[{" 'evil-previous-open-brace)
   (define-key evil-motion-state-map "]}" 'evil-next-close-brace)
   (define-key evil-motion-state-map "]s" 'evil-next-flyspell-error)
   (define-key evil-motion-state-map "[s" 'evil-prev-flyspell-error)
   (define-key evil-motion-state-map "*" 'evil-search-word-forward)
   (define-key evil-motion-state-map "," 'evil-repeat-find-char-reverse)
   (define-key evil-motion-state-map ";" 'evil-repeat-find-char)
   (define-key evil-motion-state-map "?" 'evil-search-backward)
   (define-key evil-motion-state-map "|" 'evil-goto-column)
   (define-key evil-motion-state-map "+" 'evil-next-line-first-non-blank)
   (define-key evil-motion-state-map "_" 'evil-next-line-1-first-non-blank)
   (define-key evil-motion-state-map "-" 'evil-previous-line-first-non-blank)
   (define-key evil-motion-state-map "\C-w" 'evil-window-map)
   (define-key evil-motion-state-map (kbd "C-6") 'evil-switch-to-windows-last-buffer)
   (define-key evil-motion-state-map "\C-]" 'evil-jump-to-tag)
   (define-key evil-motion-state-map (kbd "C-b") 'evil-scroll-page-up)
   (define-key evil-motion-state-map (kbd "C-d") 'evil-scroll-down)
   (define-key evil-motion-state-map (kbd "C-e") 'evil-scroll-line-down)
   (define-key evil-motion-state-map (kbd "C-f") 'evil-scroll-page-down)
   (define-key evil-motion-state-map (kbd "C-o") 'evil-jump-backward)
   (define-key evil-motion-state-map (kbd "C-y") 'evil-scroll-line-up)
   (define-key evil-motion-state-map "\\" 'evil-execute-in-emacs-state)
   (define-key evil-motion-state-map "z^" 'evil-scroll-top-line-to-bottom)
   (define-key evil-motion-state-map "z+" 'evil-scroll-bottom-line-to-top)
   (define-key evil-motion-state-map "zt" 'evil-scroll-line-to-top)
   ;; TODO: z RET has an advanced form taking an count before the RET
   ;; but this requires again a special state with a single command
   ;; bound to RET
   ;;(define-key evil-motion-state-map (vconcat "z" [return]) "zt^")
   ;;(define-key evil-motion-state-map (kbd "z RET") (vconcat "z" [return]))
   ;;(define-key evil-motion-state-map "zz" 'evil-scroll-line-to-center)
   ;;(define-key evil-motion-state-map "z." "zz^")
   ;;(define-key evil-motion-state-map "zb" 'evil-scroll-line-to-bottom)
   ;;(define-key evil-motion-state-map "z-" "zb^")
   ;;(define-key evil-motion-state-map "gv" 'evil-visual-restore)
   ;;(define-key evil-motion-state-map (kbd "C-^") 'evil-buffer)
   ;;(define-key evil-motion-state-map "zl" 'evil-scroll-column-right)
   ;;(define-key evil-motion-state-map [?z right] "zl")
   ;;(define-key evil-motion-state-map "zh" 'evil-scroll-column-left)
   ;;(define-key evil-motion-state-map [?z left] "zh")
   ;;(define-key evil-motion-state-map "zL" 'evil-scroll-right)
   ;;(define-key evil-motion-state-map "zH" 'evil-scroll-left)
   (define-key evil-motion-state-map
     (read-kbd-macro evil-toggle-key) 'evil-emacs-state)

   ;;; Operator-Pending state
   ;;; a is beg line. i remains available.

   (define-key evil-operator-state-map "a" nil)
   (define-key evil-operator-state-map "o" evil-outer-text-objects-map)
   (define-key evil-operator-state-map "i" evil-inner-text-objects-map)
   (define-key evil-visual-state-map "o" evil-outer-text-objects-map)

   ;; text objects
   (define-key evil-outer-text-objects-map "f" 'evil-a-word)
   (define-key evil-outer-text-objects-map "j" 'evil-a-WORD)
   (define-key evil-outer-text-objects-map "s" 'evil-a-sentence)
   (define-key evil-outer-text-objects-map "p" 'evil-a-paragraph)
   (define-key evil-outer-text-objects-map "b" 'evil-a-paren)
   (define-key evil-outer-text-objects-map "(" 'evil-a-paren)
   (define-key evil-outer-text-objects-map ")" 'evil-a-paren)
   (define-key evil-outer-text-objects-map "[" 'evil-a-bracket)
   (define-key evil-outer-text-objects-map "]" 'evil-a-bracket)
   (define-key evil-outer-text-objects-map "B" 'evil-a-curly)
   (define-key evil-outer-text-objects-map "{" 'evil-a-curly)
   (define-key evil-outer-text-objects-map "}" 'evil-a-curly)
   (define-key evil-outer-text-objects-map "<" 'evil-an-angle)
   (define-key evil-outer-text-objects-map ">" 'evil-an-angle)
   (define-key evil-outer-text-objects-map "'" 'evil-a-single-quote)
   (define-key evil-outer-text-objects-map "\"" 'evil-a-double-quote)
   (define-key evil-outer-text-objects-map "`" 'evil-a-back-quote)
   (define-key evil-outer-text-objects-map "t" 'evil-a-tag)
   (define-key evil-outer-text-objects-map "o" 'evil-a-symbol)
   (define-key evil-inner-text-objects-map "f" 'evil-inner-word)
   (define-key evil-inner-text-objects-map "j" 'evil-inner-WORD)
   (define-key evil-inner-text-objects-map "s" 'evil-inner-sentence)
   (define-key evil-inner-text-objects-map "p" 'evil-inner-paragraph)
   (define-key evil-inner-text-objects-map "b" 'evil-inner-paren)
   (define-key evil-inner-text-objects-map "(" 'evil-inner-paren)
   (define-key evil-inner-text-objects-map ")" 'evil-inner-paren)
   (define-key evil-inner-text-objects-map "[" 'evil-inner-bracket)
   (define-key evil-inner-text-objects-map "]" 'evil-inner-bracket)
   (define-key evil-inner-text-objects-map "B" 'evil-inner-curly)
   (define-key evil-inner-text-objects-map "{" 'evil-inner-curly)
   (define-key evil-inner-text-objects-map "}" 'evil-inner-curly)
   (define-key evil-inner-text-objects-map "<" 'evil-inner-angle)
   (define-key evil-inner-text-objects-map ">" 'evil-inner-angle)
   (define-key evil-inner-text-objects-map "'" 'evil-inner-single-quote)
   (define-key evil-inner-text-objects-map "\"" 'evil-inner-double-quote)
   (define-key evil-inner-text-objects-map "`" 'evil-inner-back-quote)
   (define-key evil-inner-text-objects-map "t" 'evil-inner-tag)
   (define-key evil-inner-text-objects-map "o" 'evil-inner-symbol)
   (define-key evil-motion-state-map "gn" 'evil-next-match)
   (define-key evil-motion-state-map "gN" 'evil-previous-match)


   ;; alt keys
   ;; TODO make menu outside of slowM
   (global-set-key (kbd "M-a") 'nu-new-prompt)
   (global-set-key (kbd "M-b") 'backward-char)
   (global-set-key (kbd "M-c") 'nu-change-prompt)
   (global-set-key (kbd "M-k") 'nu-delete-prompt)
   (global-set-key (kbd "M-e") 'newline-and-indent)
   (global-set-key (kbd "M-f") 'forward-char)
   (global-set-key (kbd "M-g") 'ace-window) ; menu is rare => space g
   (global-set-key (kbd "M-i") 'nu-back-to-indentation)
   (global-set-key (kbd "M-m") 'save-buffer) ; menu is rare. space s
   (global-set-key (kbd "M-n") 'next-line)
   (global-set-key (kbd "M-o") 'nu-do-prompt) ; menu is space o
   (global-set-key (kbd "M-p") 'previous-line)
   (global-set-key (kbd "M-q") 'nu-print-prompt)
   (global-set-key (kbd "M-r") 'nu-replace-prompt)
   (global-set-key (kbd "M-s") 'nu-find-prompt)
   (global-set-key (kbd "M-t") 'split-window-right)
   (global-set-key (kbd "M-u") 'undo-tree-visualize)
   (global-set-key (kbd "M-v") 'nu-quit-prompt)
   (global-set-key (kbd "M-w") 'nu-copy-region-or-line)
   (global-set-key (kbd "M-x") 'nu-M-x)
   (global-set-key (kbd "M-y") 'nu-insert-prompt)
   (global-set-key (kbd "M-z") 'nu-quit-document) ; menu is space


   ;; visual line mode
   (define-key evil-visual-state-map "M" 'evil-append)
   (define-key evil-visual-state-map "I" 'evil-insert)
   (define-key evil-visual-state-map "o" 'exchange-point-and-mark)
   (define-key evil-visual-state-map "O" 'evil-visual-exchange-corners)
   (define-key evil-visual-state-map "R" 'evil-change)
   (define-key evil-visual-state-map "u" 'evil-downcase)
   (define-key evil-visual-state-map "U" 'evil-upcase)
   (define-key evil-visual-state-map "z=" 'ispell-word)
   (define-key evil-visual-state-map "a" evil-outer-text-objects-map)
   (define-key evil-visual-state-map "i" evil-inner-text-objects-map)
   (define-key evil-visual-state-map (kbd "<insert>") 'undefined)
   (define-key evil-visual-state-map (kbd "<insertchar>") 'undefined)
   (define-key evil-visual-state-map [remap evil-repeat] 'undefined)
   (define-key evil-visual-state-map [escape] 'evil-exit-visual-state))


(defun nu-slow-motion ()
"setup evil to fit emacs, then activates evil"

  ;; which key mode + menus init
  (nu-initialize)
  (defalias 'nu-prompt-for-keymap 'nu-which-key-prompt-for-keymap)

  ;; no vi paddle
  (setq nu-use-vi-paddle nil)

  ;; show a warm welcome screen
  (when nu-show-welcome-screen
	   (add-hook 'emacs-startup-hook '(lambda ()
             (nu-slow-motion-help-prompt))))

  ; default is insert mode, unless you edit text...
  (setq evil-default-state 'insert
      evil-normal-state-modes '(text-mode prog-mode fundamental-mode))

  (add-to-list 'evil-insert-state-modes 'Emacs-Lisp)

  ;; SPACE key -- long term make this outside of slowM
  (setq nu-evil-map (make-keymap))
  (define-key evil-normal-state-map (kbd "<SPC>") nu-evil-map)
  (define-key nu-evil-map "c" 'nu-change-prompt)
  (define-key nu-evil-map "k" 'nu-delete-prompt)
  (define-key nu-evil-map "s" 'nu-find-prompt)
  (define-key nu-evil-map "g" 'nu-goto-prompt)
  (define-key nu-evil-map "h" 'help-map)
  (define-key nu-evil-map "m" 'nu-save-prompt)
  (define-key nu-evil-map "n" 'nu-new-prompt)
  (define-key nu-evil-map "o" 'nu-open-prompt)
  (define-key nu-evil-map "y" 'nu-insert-prompt)
  (define-key nu-evil-map "q" 'nu-print-prompt)
  (define-key nu-evil-map "r" 'nu-replace-prompt)
  (define-key nu-evil-map "u" 'undo-tree-visualize)
  (define-key nu-evil-map "x" 'nu-display-prompt)
  (define-key nu-evil-map "w" 'nu-copy-prompt)
  (define-key nu-evil-map "z" 'nu-quit-prompt)
  (define-key nu-evil-map (kbd "<SPC>") 'nu-M-x)

  ;; normal / motion / visual state
  (nu-slow-motion-set-keys-a-la-emacs)
  (global-set-key "²" 'which-key-show-top-level)

  ;; help map
   (define-key help-map "f" 'nu-describe-function)
   (define-key help-map "v" 'nu-describe-variable)
   (define-key help-map (kbd "<SPC>") 'nu-cheat-sheet)

  ;;
  ;; now adapt menus
  ;;
  (add-hook 'nu-populate-hook '(lambda ()
    (progn

       ;; inserted inside open menu
       (define-key nu-open-map "o" 'evil-jump-backward)

       ;; reset!
       (nu-define-prefix 'nu-a-map)
       (define-key nu-a-map (kbd "r") 'evil-visual-block)
       (define-key nu-a-map (kbd "l") 'evil-visual-line)
       (define-key nu-a-map (kbd "k") 'evil-visual-char))))

  ;; finally trigger evil-mode
  (evil-mode))

(provide 'nu-slow-motion) 
