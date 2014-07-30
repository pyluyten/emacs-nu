Welcome to emacs-nu!
Most comprehensive README is nu-mode README
However three things are there so let's start
with a quick intro


- Nu-mode (global minor) theme. A keybinding.
  It's emphasis is being modern, easy to use,
  reducing learning curve (& human memory load)
  as much as possible.
  While _not_ loosing features. Seriously. It's
  rather the opposite, the idea is to make
  possible to use _more_ features. Not only
  the two or three tricks you like.


- Aliases : to easier defining aliases.
  This makes sense whichever mode / setup you are
  using, as soon as you like some aliases.
  But the more aliases you like, the more it makes
  sense. The common point with the keybinding is to easier
  memory.


- Dhammacakka : "the wheel of truth"...
  Yet another basic setup to cure emacs.
  It's like posting you dot emacs, except i did pay
  attention to split things, so dhamacakka is compatible
  with thinks like evil-mode, ergo-emacs mode, or emacs
  vanilla keybinding,...


* How to install emacs-nu?


well it depends; but basically:

1/ Install emacs. (Some packages are built in : recentf, cua)
2/ package-list-package. Undo tree. You don't need to change .emacs.
3/ (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
4/ Now you can install ace-jump, magit
   You do not need to change .emacs
5/ (add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
   Now you can install help-fns+
6/ github : add to list emacs-nu.
   (nu-mode)
   (require 'dhammacakka)
   
