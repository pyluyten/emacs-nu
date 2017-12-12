__OVERVIEW__

_nu_ does not propose any shiny new feature in Emacs.
Rather, it merely changes Emacs interface. But do not worry it's minimal, lightweight, and have poneys.

nu is about menus, but not GUI menus. Text. Yes, rather like spacemacs : "which-key" menu by default (but there are other available prompters : *Help* buffer, Helm or Ivy work, too)
Text based menus, allow to discover or simply invoke many commands.
This is a very powerful system. There is a reason why hydra is a popular package. (But since sticky menus are not always useful nu is not based on hydra.)

Emacs has prefixes, like Ctrl-x, but not menus.
Which-key transforms prefixes in menus, so it makes C-x somewhat usable, but what if Emacs native prefixes were wrong?
So, we need properly designed menus : "verb->object" (like a delete menu to delete anything : delete  file, buffer, this.window, other windows,…)
This is way more easy than potatoes menus like "File menu" "Buffers menu". Verbs are easy to understand. CUA menus like copy/paste are verbs. VI operator->motion is also a verb->object description.

Also menus have a hook to allow packages to populate them - either add or even remove parts.
For example, for a mode it is better to add its features to existing menus ("new", "delete", "open"), rather than having a distinct "major mode menus" which is a bag of noodles.

the content of _nu_ menus first follow a guideline which is provided at the end of this README.

Right, but where do i call menus from?
Hmm, if you love Emacs keybindings (seriously?), you can bind C-c o to open, C-c d to delete and so on. There is a func for this. While it is not what i recommend first, it is fair to prefer vanilla Emacs keys, which are also bash keys after all! It might also be good if you already use one of the many keybinding, like ErgoEmacs.
If you use evil, there is *nu-state*, which mostly adds menus to evil. This is what i use currently.
If you want to stick to Emacs & Notepad like non-modal editing, then nu-mode is a CUA binding including nu menus. It is both very easy to learn and ergonomic. Actually nu-menus started with this.

__IS THAT ALL?__
Well, yes and no.
Yes, because i do not want to revisit every single emacs package. So dired binding remain dired binding.

No, because there are still few addon. First, the key ² stands for *CheatSheet*. In every circumstance it should display available bindings. When there is a pop up, when editing, when selecting, always.
Also, alt+j alt+k is useful for example to select ivy or helm candidated, navigating into dired and so on.

__WORDING__

MENUS : the different commands available for one "menu key". Its very close to Emacs prefix keys bound to a keymap.
PROMPTER : the func that opens the menus. It might be which-key, or a simple message, it might be a help buffer, it might be 
PADDLE : the shortcuts to move cursor (up down left right).
IMMEDIATE : an "immediate" binding is a key that directly triggers a command, like "yank"
                            as opposed to "yank menu", which contains several "insertion related" features (insert file; quoted insert and so on)

__FLAVOURS__

so, nu comes with several flavours.

_Flavour 1 : just add nu menus to Emacs_

Actually this flavour is just addind nu menus to your Emacs. It is nice if you want to keep Emacs keybindings or if you already rebind, for example with ErgoEmacs. Obviously the drawback is that if menus are too difficult to reach, they might lose their power.

_Flavour 2 : Nu Mode is just a keybinding_

It's a modern keybinding (c =copy, v=insert, x=cut, f=find)
Everything is done in insert mode.
Hands remain most of time in homerow because of alt keys.
Paddle is like evil (hjkl) or T-like (ijkl).
Default is to have alt keys do "immediate" funcs
So, menus , which are generally not necessary, are invoked using Control key like Control+f to have "Find menu"
nu mode also allows to have alt keys do menus, and control do immediate func.
This is compabilble with today's conventiosn (conrol+c copy, control+v find and so on)
Technically nu mode is now based on evil.

_Flavour 3 : Nu State is based on evil_

nu state does preserve vim keys but adds some alt keys (y=copy, p=paste, d=cut, f=find)
So, vim states (normal, insert, visual) are used. Command state is available but not useful.

Some alt keys trigger immediate func (eg to switch windows without leaving home row), some trigger menus.

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

You can also install git repo but then you're a big boy and do not need
instructions, do you?


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


__MENU CONTENT GUIDELINE__

When a function is to be mapped to a menu key,
it is mapped to the dedicated key if possible.
For example, a "file" is binded to "f" when possible,
so inside save menu one can type "f" to call "write-file".

| key | functions              | example                         |
|-----+------------------------+---------------------------------|
| a   | bookmark, rectangle    | bookmark-set                    |
| b   | buffer                 | insert-buffer                   |
| c   | calc                   | calc                            |
| d   | directory, dired       |                                 |
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
| q   | ======NO ============  |                                 |
| r   | recent, register, mark | point-to-register               |
| s   | string, save           |                                 |
| t   | tag                    |                                 |
| u   | undo, url, revert      |                                 |
| v   | abbrev                 |                                 |
| w   | window                 | ace-window                      |
| x   | [reg]exp               |                                 |
| y   | all                    |                                 |
| z   | command                | term                            |

