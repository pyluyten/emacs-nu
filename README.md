_Welcome to emacs-nu!_

Emacs is the best editor in the universe.
But it is so hard to learn & discovers its features!

Nu-mode is an Emacs package.
What is nu-mode?
nu-mode wants to make Emacs : easier and faster.
Easier and faster to learn, memorize, easier and faster
to discover, and even easier and faster to use.

To achieve this nu-mode is several pieces.

- nu-mode is a consistent and easy keymap.

  Usual commands, navigation can be done keeping thumb on
  Alt (err, "Meta" in Emacs world.. no! let's say Alt!)

  Navigation is done using a paddle : i j k l
  stand for up left down right.

- nu-mode is a prompter mechanism

  A prompt is a menu displaying commands.
  Call it then you are discovering many things.
  The prompt also offers to describe these commands.
  It allows to use unversal argument or "repeat" mode
  (like hydra, which sometimes emacs-nu uses).

  Finally, whenever a command appears in a prompt
  but have a direct shortcut, the prompt tells the user
  about it. So, it is easy enough to learn new shortcuts.

_WHAT KEYMAP_ and _WHAT PROMPTS_ ?

  any very useful command should be available keeping your thumb on Alt.
  This obviously includes navigation.
  This includes also searching for text, saving file, selecting...

  Less common functions are available using a prompt.
  The usual way to call a prompt is to use the classic shortcuts, eg control+f. Instead of "find", emacs-nu binds control+f to find-prompt.
   So type ctrl+f in emacs-nu and you will discover many search features,
   see their direct shortcuts when it is there.
  
See its texinfo manual.
(http://nupad.wordpress.com/ blog is not yet up to date.)

_How to install?_

* Using Emacs Package Manager

Nu-mode is in Melpa. So it's the usual deal.
(or look at https://melpa.org)

    (require 'package)

    (add-to-list 'package-archives
    '("melpa" . "https://melpa.org/packages") t)

Then, M-x package-list-packages, then search for nu-mode.
Finally add this to your init.el.

      (require 'nu-mode)
      (nu-mode 1)

* Manually

Well, clone github, and add path to your list

    (add-to-list 'load-path "~/path/to/git/emacs-nu/nu-mode/")

Then the usual require & acitvation.
But to visit info file the simpler is to call

     (info "~/path/to/git/emacs-nu/doc/nu-mode.info")

