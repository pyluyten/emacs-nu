_Welcome to emacs-nu!_


- Nu-mode (global minor) theme. A keybinding and more importantly
  its specific prompt mechanism.
  Its emphasis is being modern, easy to use,
  reducing learning curve as much as possible.
  While _not_ loosing features.
  This is the most important part of emacs-nu.

  See its texinfo manual, or this site wiki, or the http://nupad.wordpress.com/ blog.


- Aliases : to easier defining aliases.
  Define your aliases in org-mode files.
  See its dedicated readme.


- Dhammacakka : "the wheel of truth"...
  Yet another basic setup to cure emacs.
  See its dedicated readme.




_How to install?_

* Using Emacs Package Manager

There's a recipe for nu-mode.
Make sure you do require package. In your .emacs put:

    (require 'package)

Now, make sure you did add melpa.

    (add-to-list 'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/") t)

Now, if you are interested in nu-mode, just search nu-mode.
Once intalled, add the elpa path to your load path and require nu-mode.

      (require 'nu-mode)
      (nu-mode 1)

* Manually

first clone github, add the path to your list

    (add-to-list 'load-path "~/contrib/git/emacs-nu/starterkit/")

You can already require nu-alias. And add file.

    (require 'nu-alias)
    (nu-alias-add-file "~/.emacs.d/some-aliases.org")

and require dhammacakka. This will add packages.

    (require 'dhammacakka)

reboot Emacs if you want to install packages for nu-mode.
Optional dependencies are
undo-tree
ace-jump-mode
help-fns+
magit


    (require nu-mode)
    (nu-mode 1)
