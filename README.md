_OVERVIEW_

NU does not propose any new feature in Emacs.
It merely changes the Emacs interface. But do not worry it's minimal, lightweight, and have poneys.

NU is about menus, but not GUI menus. Yes, rather like spacemacs : "which-key" menu (but there are other prompters than which keys. HelpBuffer, Helm or Ivy work, too)
Text based menus, allow to discover or simply invoke many commands.
This is a very powerful system. (There is a reason why hydra is a popular package ; but sticky menus are not always useful)

Emacs has prefixes, like Ctrl-x, but not menus.
Which-key transforms prefixes in menus, but what if prefixes were wrong?
So, we need properly designed menus : "operator->motion" (like a delete menu to delete anything : delete  file, buffer, this.window, other windows,…)
This is way more easy than potatoes menus like "File menu" "Buffers menu"

Also menus needs to have a hook to allows packages to populate these menus (either add or even remove parts of menus)
For example, for major mode it is better to add its features to existing menus ("new", "delete", "open"), rather than having a distinct "major mode menus" which is a bag of noodles.

Right, but where do i call menus from?
Hmm, if you love Emacs keybindings (seriously?), you can bind C-c o to open, C-c d to delete and so on. There is a func for this. But i do not recommend.
(Unless you are already ErgoEmacs user.)
 i recommend to use : either a comfortable keybinding "nu-mode" ; or an addon to evil "nu state" if you want modal editing
These are described below but a few words are needed to understand.

_WORDING_

MENUS : the different commands available for one "menu key". Its very close to Emacs prefix keys bound to a keymap.
PROMPTER : the func that opens the menus. It might be which-key, or a simple message, it might be a help buffer, it might be 
PADDLE : the shortcuts to move cursor (up down left right).
IMMEDIATE : an "immediate" binding is a key that directly triggers a command, like "yank"
                            as opposed to "yank menu", which contains several "insertion related" features (insert file; quoted insert and so on)

_Flavour 1 : just add nu menus to Emacs_

Actually this flavour is just addind nu menus to your Emacs. It is nice if you want to keep Emacs keybindings or if you already rebind, for example with ErgoEmacs.

_Flavour 2 : Nu Mode is just a keybinding_

It's a modern keybinding (c =copy, v=insert, x=cut, f=find)
Everything is done in insert mode.
Hands remain most of time in homerow because of alt keys.
Paddle is like evil (hjkl) or T-like (ijkl).
Default is to have alt keys do "immediate" funcs
So, menus , which are generally not necessary, are invoked using Control key like Control+f to have "Find menu"
nu mode also allows to have alt keys do menus, and control do immediate func.
This is compabilble with today's conventiosn (conrol+c copy, control+v find and so on)


_Flavour 3 : Nu State is based on evil_


So, vim states (normal, insert, visual) are used. Command state is available but not useful.
It does preserve vim keys but adds some alt keys
(y=copy, p=paste, d=cut, f=find)

Some alt keys trigger immeidate func (eg to switch windows without leaving home row), some trigger menus

_INSTALLATION_

Common for just menus or nu-mode or nu-state
Install package on melpa

Nu-mode is in Melpa. So it's the usual deal.
(or look at https://melpa.org)

    (require 'package)

    (add-to-list 'package-archives
    '("melpa" . "https://melpa.org/packages") t)
 
 

Then, M-x package-list-packages, then search for nu-mode.
Finally add this to your init.el.



_NO KEYMAP : JUST BIND MENUS TO CONTROL-C._

    (require 'nu-mode)
    (nu-fill-mode-map-with-nu-menus)

_NU-MODE_

a global minor mode to push a keymap. Also add some hooks to help the user.

    (require 'nu-mode)
    (nu-mode)

if you want to use ijkl paddle rather than default (vi).
    (nu-set-classic-paddle)

if you want control to be like CUA ; and alt keys trigger menus

    (nu-set-control-mode)

_NU-STATE (evil)_

nu state preserves vi keys, but adds alt keys to invoke immediate funcs and some menus.
for example in vi, d is delete.
with nu state, both in normal state and insert state, you can use altd for delete menu.

     (require 'nu-state)
     (nu-state)

Nu state cannot be customized yet (appart below)


_COMMON CUSTOMIZATIONS_

no matter if you use just menus, nu-mode, or nu-state, you have some common customizations.

=Prompter=
Default prompter is which-key

You can refer to which key to customize it.
Or you can use another prompter
Other prompter allow more features : "+" to trigger repeat menu , "-" or "1", "2", … to customize universal argument, "?" to run help of command

    (defalias 'nu-prompt-for-keymap 'nu-light-prompt-for-keymap)
    (defalias 'nu-prompt-for-keymap 'nu-completion-prompt-for-keymap)
    (defalias 'nu-prompt-for-keymap 'nu-buffer-prompt-for-keymap)


=Completion framework=

Things should be ok if you just enable your ido / helm / ivy or whatever
Anyway i'd recommend to tell nu about your usage


    (nu-set-ivy)
    (nu-set-helm)
