# Wyvernscale Source Code Editor (wsedit)

Build Status:

[![forthebadge](http://forthebadge.com/images/badges/fuck-it-ship-it.svg)](http://forthebadge.com)

## Introduction

`wsedit` is a clean, intuitive terminal-based editor with CUA keybinds. It is
designed to get the job done in a simple, elegant and pragmatic manner. If
you've ever worked with a text editor and with a terminal, you already know
how `wsedit` works, except some small quirks here and there maybe.


## Features

* __Dynamic dictionary-based autocompletion__: Specify which files to index, and
  how (e.g. ignore comments, only global definitions, ...).

* __Pragmatic syntax highlighting__: Highlights keywords, strings, matching
  brackets and comments declared in your config files.  Default patterns for
  some languages are availabe for import in `lang/*.wsconf`, writing your own
  should take no longer than 30 minutes, including the submission of a pull
  request on GitHub =).

* __Character class highlighting__: This will colour your text by character
  class (e.g. operators -> yellow, brackets -> brown, numbers -> red, ...),
  supplementing the already mentioned syntax highlighting quite nicely and
  provides a base-line readability boost if no highlighting rules have been set.

* __Simple configuration interface via config files__: Not really much to say
  here.

* __Reader mode__: Want to glance over a file without accidentally editing it?
  Start the editor in read-only mode, or toggle it via keybind.

* __Protects you data__: Special routines are in place to ensure your work
  doesn't just vanish, should the editor crash. I sure wish having this wasn't
  something to be proud of.

## Platforms / Installation

### Windows: not supported

The whole idea of running a terminal editor on Windows seems a bit strange to
me.  However, you are free to try building it from source (see below).

### Linux: build from source

No packages available yet, contact me if you want to package `wsedit` for your
distribution.

### OSX: Homebrew

Thanks to Alex Arslan for providing a homebrew formula for wsedit:

    brew tap ararslan/pints
    brew install wsedit

**IMPORTANT**: I have no access to OSX systems myself, so the amount of support
I can provide for platform-specific issues will be heavily limited and I might
need your help to test some things for me.

### Building from source

#### "I know how to linux"

1.  Install:
  * [stack](http://docs.haskellstack.org/en/stable/README/),
  * `ncurses` with unicode support,
  * `xclip` or `xsel`, optional, makes `wsedit` use the system clipboard.
2.  Grab the latest `wsedit` release off GitHub.
3.  Run `stack install`, the binary will be placed into `$HOME/.local/bin/`.
4.  Either add `$HOME/.local/bin/` to your `$PATH` or copy/link/symlink the
    execuable where you actually want it.
5.  Copy all relevant definitions from `lang/` to `$HOME/.config/wsedit`. Just
    copying everything is fine too.
6.  Done! I recommend opening two terminals next to each other, running `wsedit`
    in one of them and looking up keybinds in the other one with `wsedit -hk`.
    Most importantly: press `Ctrl-Q` to quit.

#### "I'm new, please be gentle"

First of all, welcome to Linux! If you encounter any problems, take a look at
the `Troubleshooting` section further down below and see if it helps.

1.  Install the
    [Haskell Tool Stack](http://docs.haskellstack.org/en/stable/README/).
    (If you don't have root access to install stack, pick the
    __Linux (general)__ option and call the `stack` binary inside the archive
    directly.)
2.  Make sure you have `ncurses` with unicode support installed. This should be
    default on most popular distributions.
3.  *Optional*, Linux only: Install either `xclip` or `xsel` with your package
    manager. If this step is skipped, `wsedit` will use an internal buffer
    instead of the system facilities for copy/paste functionality.
4.  Grab the latest stable release of `wsedit` from the `Releases` tab on
    GitHub.
5.  Extract the archive and point your shell towards its contents.
6.  Run `stack setup` to pull in the correct version of `ghc`.
7.  Run `stack install` to build the dependencies and `wsedit`.
8.  Check whether `$HOME/.local/bin` is already part of your `$PATH` variable:
    if the command `echo "$HOME" | grep "$HOME/.local/bin"` has no output, add
    the line `PATH="${PATH}:${HOME}/.local/bin"` to the file `~/.bashrc`. This
    file will be executed every time you open a shell, so you either need to
    re-open the terminal or run `source ~/.bashrc` to re-run it manually.
9.  To get syntax highlighting, first create the folder `~/.config/wsedit` by
    running `mkdir -p ~/.config/wsedit`. Then take a look at the repository's
    `lang/` folder. Copy everything you need (or just everything, it won't hurt)
    to the newly created folder using `cp lang/* ~/.config/wsedit/`. If your
    favourite language has no definitions available, you can easily create them
    yourself, take a look at `lang/README.md` for instructions.
10. Done! I recommend opening two terminals next to each other, running `wsedit`
    in one of them and looking up keybinds in the other one with `wsedit -hk`.
    Most importantly: press `Ctrl-Q` to quit, or `Ctrl-Alt-Q` to quit discarding
    all changes.


## Bugs / Crashes and how to report them properly

`wsedit` is written with a focus on limiting bug severity and protecting your
data in case of failure rather than on extensive test suites. For now, I have
completely ditched the concept of automated testing in favour of some run-time
sanity checks as well as sophisticated exception handlers, making data loss
almost impossible to occur. I believe this approach is superior for two reasons:

 * It makes much better use of my limited time.
 * Even with test suites of absolutely monstrous proportions you will never
   catch all the bugs.

However, this does not mean that `wsedit` is full of bugs. There will be some,
but since I use `wsedit` for everything myself, you can rest assured that
nothing critical will stay in the code for too long.

Please submit every kind of weird behaviour you encounter as an
[issue on GitHub](https://github.com/SirBoonami/wsedit/issues/new). If possible,
obtain a state dump as described below.

### Crashes

The editor main loop runs inside an exception handler that will do the following:

1. Dump the current state of your file to `${HOME}/CRASH-RESCUE` if it has been
   modified since the last save. As long as this location stays writeable, next
   to nothing can happen to your data.
2. Dump the editor's configuration, state and some additional info to
   `${HOME}/CRASH-DUMP`. This file can be used to restart the editor in the last
   coherent state before it crashed.
3. Safely shut down everything.

The state dump is of great importance to fixing the bug. However, it contains
all active configuration as well as the entire file you edited when the crash
happened. Make sure you're okay with that becoming public before uploading it.
Also, please don't provide a modified dump file, as any changes made will throw
off the caching system.

### Non-fatal bugs

Most non-fatal bugs will probably be rendering glitches. Reproduce the
situation, point the cursor at it if possible, then press Meta + ".".
This will simulate a crash and create the above-mentioned files.


## Known issues / Troubleshooting / FAQ

### My cursor is invisible!

Deactivate the `-db` switch.

### The language definitions won't work!

Make sure they're placed directly into `~/.config/wsedit/`, not in sub-folders.

### `wsedit` is slow (on older machines)!

  * Disable `-db` if it is active.
  * Performance is highly dependant on your terminal emulator. I can personally
    recommend `sakura`, `xterm` and to a lesser degree also `rxvt-unicode` if
    you really hate yourself.

### The build fails with some obscure error message

  * Make sure all dependencies listed above are satisfied.
  * Try `stack clean`.
  * If that doesn't work, delete the `.stack-work` folder and try again.

### `wsedit` destroys Unicode on `xterm`

__Symptoms:__ After running `wsedit`, any unicode output by other programs (e.g.
`tree`) will be garbled.

This seems to be a problem wit `vty`, the terminal library `wsedit` uses, since
`yi`, another terminal editor based on `vty`, suffers from the same issue. For
now I can only recommend using another terminal emulator if you need the unicode
support.

### Some inputs (e.g. `Ctrl-Down`) don't work in `rxvt-unicode`

Yeah, `urxvt` is a mess. I recommand changing to another terminal, but adding
this to your `.Xresources` file will soothe your pain:

    ! From http://thedarnedestthing.com/urxvt
    urxvt*keysym.C-Up: \033[1;5A
    urxvt*keysym.C-Down: \033[1;5B
    urxvt*keysym.C-Right: \033[1;5C
    urxvt*keysym.C-Left: \033[1;5D
    urxvt*keysym.S-Up: \033[1;2A
    urxvt*keysym.S-Down: \033[1;2B
    urxvt*keysym.S-Right: \033[1;2C
    urxvt*keysym.S-Left: \033[1;2D
    urxvt*keysym.M-Up: \033[1;3A
    urxvt*keysym.M-Down: \033[1;3B
    urxvt*keysym.M-Right: \033[1;3C
    urxvt*keysym.M-Left: \033[1;3D

    urxvt*iso14755: False
    urxvt*iso14755_52: False
