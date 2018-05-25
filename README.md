# OVERVIEW

**nu** does not propose any shiny new feature in Emacs.
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

# FLAVOURS

so, nu comes with several flavours.

## Flavour 1 : just add nu menus to Emacs

Actually this flavour is just adding nu menus to your Emacs. It is nice if you want to keep Emacs keybindings or if you already rebind, for example with ErgoEmacs. Obviously the drawback is that if menus are too difficult to reach, they might lose their power.

## Flavour 2 : Nu Mode is just a keybinding

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

## Flavour 3 : Nu State is based on evil

nu state is vim.
Well, nu state does preserve vim keys but adds some alt keys (y=copy, p=paste, d=cut, f=find)
So, vim states (normal, insert, visual) are used. Command state is available but not useful.

Some alt keys trigger immediate func (eg to switch windows without leaving home row), some trigger menus.

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

## COMMON CUSTOMIZATIONS

no matter if you use just menus, nu-mode, or nu-state, you have some common customizations.

**Prompter**
Default prompter is which-key

You can refer to which-key to customize behaviour like, how long it takes for menu to appear.
Actually which-key already offers a lot of options.
Or you can use another prompter
Other prompter allow more features : "+" to trigger repeat menu , "-" or "1", "2", … to customize universal argument, "?" to describe a command rather than describing it.

    (defalias 'nu-prompt-for-keymap 'nu-light-prompt-for-keymap)
    (defalias 'nu-prompt-for-keymap 'nu-completion-prompt-for-keymap)
    (defalias 'nu-prompt-for-keymap 'nu-buffer-prompt-for-keymap)

**Completion framework**

Things should be ok if you just enable your ido / helm / ivy or whatever
Anyway i'd recommend to tell nu about your usage

    (nu-set-ivy)
    (nu-set-helm)


# MENU CONTENT EXPLANATION

Menus are like operators : *save*; *open*, *new*, ...
Menus content are like objects : *buffer*, *file*, *frame*, ...

emacs nu tries to dedicate keys to these objects.
When a function is to be mapped to a menu key,
it is mapped to the dedicated key if possible.

For example, a "file" command is bound to "f" when possible,
so inside save menu one can type "f" to call "write-file",
while inside delete menu "f" will call "delete-file".


| key | functions              | example                         |
|-----|------------------------|---------------------------------|
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
| q   | ====== NOT BOUND ====================================== |
| r   | recent, register, mark | point-to-register               |
| s   | string, save           |                                 |
| t   | tag                    |                                 |
| u   | undo, url, revert      |                                 |
| v   | abbrev                 |                                 |
| w   | window                 | ace-window                      |
| x   | [reg]exp               |                                 |
| y   | all                    |                                 |
| z   | command                | term                            |

