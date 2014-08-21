_Welcome to emacs-nu!_


- Nu-mode (global minor) theme. A keybinding and more importantly
  its specific prompt mechanism.
  Its emphasis is being modern, easy to use,
  reducing learning curve as much as possible.
  While _not_ loosing features.
  This is the most important part of emacs-nu.

  See its dedicated readme. Now, if you're lazy, some example here
  , Press Alt-g,f or example, for a goto-prompt. (Not the most standard of my prompts, well...)
  You will directly see keys to perform some action. For example, "u" to backward-word.
  You will also see you could have directly used a shortcut rather than using this prompt.
  , Press 3 to trigger numeric argument +3.
  , Press + to ask the prompt to repeat itself.
  , Now, press "u" for backward-word. You go three words backward. As the prompt was asked for repetition,
  press u again and you go three times backward-word again. Press o, and that's three times forward-word.
  , Press ? to ask for prompt help. Now press u and you have backward-word definition.


- Aliases : to easier defining aliases.
  Define your aliases in org-mode files.
  See its dedicated readme.


- Dhammacakka : "the wheel of truth"...
  Yet another basic setup to cure emacs.
  See its dedicated readme.




_How to install?_


1/ Install emacs.
   Some packages are built in : recentf, cua...

2/ Mx package-list-package.
   Install undo-tree. (You don't need to change .emacs afterward)

3/ Add to your .emacs Marmalde:
   (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

4/ Still with Mx package-*:
   install ace-jump, eventually magit
   You do not need to change .emacs

5/ (add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
   Now you can install help-fns+

6/ github : add to list emacs-nu.
   (require 'emacs-nu)


-  To activate NU-MODE :   (nu-mode 1)
-  To activate the setup : (require 'dhammacakka)
-  To add an aliases org file : (nu-alias-add-file "~/.emacs.d/some-aliases.org")
