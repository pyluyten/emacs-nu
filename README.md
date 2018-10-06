# OVERVIEW

`nu` is an Emacs package which does not add features, but focus on interface, like `ErgoEmacs`, or `Hydra`. There are two parts of nu :
- The menu
  This part of nu aims to allow to run hundred(s) of different funcs without too much trouble, to leverage Emacs strength. It is possible to only employ nu menu and disregard keybindings.
- The keybindings (flavours)
  This part of nu deals with keys. It is possible to use of of them without the menu but other imply the menu to be used.

## The menu ##

I dislike `Control+x` because many keybindings are difficult both to memorize and to type. Also it is not merged with `Alt+g` (**goto-map**) or `Control+c` (**mode specific map**). I rather prefer a global menu. `Spacemacs` uses a similar menu, except it is grouped by theme and might contain less modes specific items.

So nu has several Submenus ("operators" or also "verbs"), and also a leader menu to call these. Submenus are like operators : *save*; *open*, *new*, ... Menus content are like objects : *buffer*, *file*, *frame*, ... 

For example, a "file" command is bound to "f" when possible, so inside save menu one can type "f" to call "write-file", while inside delete menu "f" will call "delete-file".

### Menu verbs ###

in order to offer many funcs we need many operators. Currently 13 + help-map
so it is useful to have a good description of operators to knwow where to find funcs.

| verb    | synonym   | keymap         | description                                                                                 |
|---------|-----------|----------------|---------------------------------------------------------------------------------------------|
| change  | bold      | nu-change-map  | change an existing item, not a string replacement : rather emphasize, promote, face, toggle |
| copy    |           | nu-copy-map    | copy any element  (file buffer...)                                                          |
| kill    | delete    | nu-kill-map    | kill buffer, string, file, lines.                                                           |
| display | narrow    | nu-display-map | outline, shrink, hide, ...                                                                  |
| goto    |           | nu-goto-map    | goto next link, list item, error, other-win, register. But otherwise use "open"             |
| insert  |           | nu-insert-map  | insert file, register, command                                                              |
| mark    |           | nu-mark-map    | mark (only useful for modes)                                                                |
| new     | create    | nu-new-map     | create buffer, file, frame, win, compose mail, record macro                                 |
| print   | eval      | nu-print-map   | print, export, compile code or eval code / expression                                       |
| quit    | archive   | nu-quit-map    | quit something, archive something. For refile use save. For undo use undo-tree              |
| replace | transpose | nu-replace-map | replace string, transpose (string frame)                                                    |
| save    | refile    | nu-save-map    | save, refile, push to list                                                                  |
| switch  | setting   | nu-switch-map  | toggle a settings, customize. (but toggle a box is "change")                                |


### Menu objects ###

any of these verbs acts on an object.

| key | functions              | example                         |
|-----|------------------------|---------------------------------|
| a   | bookmark, rectangle    | bookmark-set                    |
| b   | buffer                 | insert-buffer                   |
| c   | calc                   | calc                            |
| d   | directory, dired       | dired                           |
| e   |                        |                                 |
| f   | file, frame            | write-file                      |
| g   | agenda                 |                                 |
| h   |                        |                                 |
| i   | interactive, list      |                                 |
| j   | next, down             |                                 |
| k   | previous, up           |                                 |
| l   | link                   |                                 |
| m   | mail                   |                                 |
| n   |                        |                                 |
| o   | other, inversed        | other-window, save-some-buffers |
| p   | macro, package, eval   | kmacro-end-or-call-macro        |
| q   | QUIT ANY MENU          | ~~~~~~~~~~~~~~~~~~~~~~~~~~~     |
| r   | recent, register, mark | point-to-register               |
| s   | string, save           |                                 |
| t   | tag                    |                                 |
| u   | undo, url, revert      |                                 |
| v   | abbrev                 |                                 |
| w   | window                 | ace-window                      |
| x   | [reg]exp               |                                 |
| y   | all                    |                                 |
| z   | command                | term                            |

## FLAVOURS ##

so, nu comes with several flavours.

### No Flavour : just add nu menus to Emacs ###

Actually this flavour is just adding nu menus to your Emacs. It is nice if you want to keep Emacs keybindings or if you already rebind, for example with ErgoEmacs. Obviously the drawback is that if menus are too difficult to reach, they might lose their power.

### vim flaour : Nu State is based on evil ###

nu state is vim. It is simply integration of nu-menu into evil.
Well, nu state does preserve vim keys but adds some alt keys (y=copy, p=paste, d=cut, f=find)
So, vim states (normal, insert, visual) are used. Command state is available but not useful.

Some alt keys trigger immediate func (eg to switch windows without leaving home row), some trigger submenus.

### slowMotion flavour : an Emacs respectful modal keybinding ###

slowm is modal and based on evil. It basically binds evil funcs to other keys, in order to respect emacs conventions like `a` for beginning-of-line or `k` to kill. slowm makes sense on itself but may also be used together with nu menu.

### notepad Flavour : Nu Mode keybinding ###

It's a modern keybinding (c =copy, v=insert, x=cut, f=find)
Everything is done in insert mode.
Hands remain most of time in home row because of alt keys.
Paddle is like evil (hjkl) or invesed T-like (jkil).
Default is to have alt keys do "immediate" funcs
So, menus , which are generally not necessary, are invoked using Control key like Control+f to have "Find menu"
nu mode also allows to have alt keys do menus, and control do immediate func.
This is compabilble with today's conventions (conrol+c copy, control+v find and so on)

Technically nu mode is now based on evil. This allows not to reinvet few functions.
This includes selecting text (like vim selection mode), which is the only modal part of nu mode.
This also includes a keybinding to run evil-delete, like vi "d" operator to delete any vi motion.


# INSTALLATION

Common for just menus or nu-mode or nu-state
Install package on melpa

Nu-mode is in Melpa. So it's the usual deal.
(or look at https://melpa.org)

    (require 'package)

    (add-to-list 'package-archives
    '("melpa" . "https://melpa.org/packages") t)
 


Then, M-x package-list-packages, then search for nu-mode.
Finally add this to your init.el.

You can also install git repo but then you're a big boy and do not need
instructions, do you?


## NO KEYMAP : JUST BIND MENUS

    (require 'nu-mode)
    (nu-initialize)
	(nu-populate-prompters)
	
  Then bind the key or sequence you want to nu-menu-map, like

    (global-set-key (kbd "<menu>") 'nu-prompt-for-menus)
    (global-set-key (kbd "C-c") 'nu-prompt-for-menus)

## NU-MODE

a global minor mode to push a keymap. Also add some hooks to help the user.

    (require 'nu-mode)
    (nu-mode)

if you want to use ijkl paddle rather than default (vi),
use the variable nu-use-vi-paddle.
If you want control to be like CUA ; and alt keys trigger menus,
then use variables nu-immediate-key and nu-menu-key.

     (require 'nu-mode)
     (setq nu-immediate-key "C"
           nu-menu-key      "M"
           nu-use-vi-paddle t)
     (nu-mode)

nu-mode also has other customization, see below.

## NU-STATE (evil)

nu state preserves vi keys, but adds alt keys to invoke immediate funcs and some menus.
for example in vi, d is delete.
with nu state, both in normal state and insert state, you can use altd for delete menu.

     (require 'nu-state)
     (nu-state)

So in nu state, to invoke a prompt like _insert_ will leverage the usual vi key for _insert_, which is _p_. This should be good enough for a vimer. But you're a stupid mormon like me, right? so you can use : 

    (require 'nu-state)
    (nu-state-set-alt-func-using-notepad-keys)

This one will set keys so you can have alt+v for insert menu, and more generally notepad like keys to invoke menus. Yeah so now we have a mix of vim notepad and emacs, and this is currently the keymap i'm using. it rocks because vi keys are easy enough for classic operations, then you leverage the usual vim visual mode which rocks, then you have alt+d for invoking function (M-x) which rocks because it's just here (yeah `space space` works, too) then finally for more difficult operations you have the menus with easy mnemonics. So you're a noob but still do black magic.

# COMMON CUSTOMIZATIONS

no matter if you use just menus, nu-mode, or nu-state, you have some common customizations.

## add a func to a menu

when nu does populate the menu to fit current modes, it runs the hook *nu-populate-hook*. After this hook runs, you can add a key to any of the keymap.

## Completion framework

Things should be ok if you just enable your ido / helm / ivy or whatever
Anyway i'd recommend to tell nu about your usage

    (nu-set-ivy)
    (nu-set-helm)

## Prompter

Default prompter is which-key

You can refer to which-key to customize behaviour like, how long it takes for menu to appear.
Actually which-key already offers a lot of options.
Or you can use another prompter
Other prompter allow more features : "+" to trigger repeat menu , "-" or "1", "2", … to customize universal argument, "?" to describe a command rather than describing it.

    (defalias 'nu-prompt-for-keymap 'nu-light-prompt-for-keymap)
    (defalias 'nu-prompt-for-keymap 'nu-completion-prompt-for-keymap)
    (defalias 'nu-prompt-for-keymap 'nu-buffer-prompt-for-keymap)


# Extend nu

## add a mode

First make a list of all funcs a mode offers. Do not use describe-mode because some funcs may not be mapped. Instead you can use for example ivy : run counsel-M-x, then input the mode prefix (org-), then type `C-c C-o` to run *ivy-occur*, then save the list. Here you are. Now you just have to define a func to add to relevant nu keymaps the relevant funcs. 

`describe-mode` is still useful to note which funcs are actually a simple remap. You may not want to bind these to menu. Keep these documented so it is feasible to check all funcs are handled.

