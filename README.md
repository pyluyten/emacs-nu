_Welcome to emacs-nu!_

Emacs is the best editor in the universe.
Nu-mode is an Emacs package.

What is nu-mode?
NU-MODE wants to make Emacs easier & faster, stronger & better.

- It is a keymap.
  This keymap is a combo of
  * ergonomy. one do not want to constangly switch between Control, Alt, Super!
  * conventions. one want control+s to allow to save, control+f to find & so

- It is a prompter mechanism
  A prompt is a menu displaying many commands.
  Also, it does display how to obtain these commands more quickly, when available
  Also, it does offer to describe these commands.

_WHAT KEYMAP_ and _WHAT PROMPTS_ ?

  any very useful command should be available keeping your thumb on Alt.
  This obviously includes navigation.
  This includes also searching for text, saving file, selecting...

  other functions are available using a prompt.
  either you use the classic shortcuts, eg control+f , while standing for "find"
  in common text editors, will stand for "any find related command" in nu-mode.
  so when you type control+f, you will see how to call many different commands.
  
See its texinfo manual, or this site wiki, or the http://nupad.wordpress.com/ blog.

_How to install?_

* Using Emacs Package Manager

Nu-mode is in Melpa. So it's the usual easy process.
You can look at https://melpa.org
They provide instructions to put something like this in your init.el

    (require 'package)

    (add-to-list 'package-archives
    '("melpa" . "https://melpa.org/packages") t)

Then, M-x package-list-packages, then search for nu-mode.
Finally add this to your init.el.

      (require 'nu-mode)
      (nu-mode 1)

* Manually

first clone github, add the path to your list

    (add-to-list 'load-path "~/path/to/git/emacs-nu/starterkit/")
