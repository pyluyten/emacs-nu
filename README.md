_Welcome to emacs-nu!_


- Nu-mode (global minor) theme. A keybinding and more importantly
  its specific prompt mechanism.
  Its emphasis is being modern, easy to use,
  reducing learning curve as much as possible.
  While _not_ loosing features.
  This is the most important part of emacs-nu.
  See its dedicated readme.


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
