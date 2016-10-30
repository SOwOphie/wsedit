# Wyvernscale Source Code Editor (wsedit)

Build Status: [![forthebadge](http://forthebadge.com/images/badges/fuck-it-ship-it.svg)](http://forthebadge.com)

## Introduction

`wsedit` is a clean, intuitive terminal-based editor with CUA keybinds. It is
designed to get the job done in a simple, elegant and pragmatic manner. If you
read the documentation once, go "huh, go figure." and only return once a month
to look up that one option you never used before, `wsedit` has archieved its
goal.


## Features

* __Dynamic dictionary-based autocompletion__: Specify which files to index, and
  how (e.g. ignore comments, only global definitions, ...).

* __Pragmatic syntax highlighting__: Highlights keywords, strings, matching
  brackets and comments declared in your config files.  Default patterns for
  some languages are availabe for import in `lang/*.wsconf`, writing your own
  should take no longer than 30 minutes, including the submission of a pull
  requrest on GitHub =).

* __Character class highlighting__: This will color your text by character class
  (e.g. operators -> yellow, brackets -> brown, numbers -> red, ...),
  supplementing the already mentioned syntax highlighting quite nicely and
  provides a base-line readability boost if no highlighting rules have been set.

* __Simple configuration interface via config files__: Not really much to say
  here.

* __Reader mode__: Want to glance over a file without accidentally editing it?
  Start the editor in read-only mode, or toggle it via keybind.

* __Protects you data__: Special routines are in place to ensure your work
  doesn't just vanish, should the editor crash. I sure with this wasn't
  something to be proud of...

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
  * [stack](http://docs.haskellstack.org/en/stable/README/)
  * `ncurses` with unicode support
  * `xclip` or `xsel`, optional
2.  Grab the latest `wsedit` release off GitHub
3.  Run `stack install`, the binary will be placed into `$HOME/.local/bin/`.
4.  Copy all relevant definitions from `lang/` to `$HOME/.config/wsedit`. Just
    copying everything is fine too.
5.  Done! I recommend opening two terminals next to each other, running `wsedit`
    in one of them and looking up keybinds in the other one with `wsedit -hk`.
    Most importantly: press `Ctrl-Q` to quit, or `Ctrl-Meta-Q` to quit
    discarding all changes.

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
    file will be executed everytime you open a shell, so you either need to
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

## A small note on interface stability

`wsedit`'s configuration interface is not stable as of 1.1.* . I intend to
clean up the somewhat strange syntax for 1.2, and this will lead to changes in
the syntax of config files. However, I do promise two things:

 * All syntax changes will be for the better, allowing for more expressive
   configuration.

 * I will provide a `sed` script to update configs from release to release.


## Bugs / Crashes and how to report them properly

There will be bugs. No doubt about that. I'm trying my best to keep their
severity and frequency down, but I *will* miss some.

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
3. Shutdown `vty` + `ncurses` so your terminal doesn't get rekt.

This means that even in the event of a crash, data loss is highly unlikely to
occur.

The state dump is of great importance to fixing the bug. However, it contains
all active configuration as well as the entire file you edited when the crash
happened. Make sure you're okay with that becoming public before uploading it.
Also, please don't provide a modified dump file, as any changes made will throw
off the caching system.

### Non-fatal bugs

Most non-fatal bugs will probably be rendering glitches. Reproduce the
situation, point the cursor at it if possible, then press Meta + ".".
This will simulate a crash and create the above-mentioned files.


## Known issues / Troubleshooting

### My cursor is invisible!

Add `*: -db` to your global config file (`wsedit -ocg`).

### `wsedit` is slow on older machines

  * Use `*: -db` to disable background rendering, which remedies this for the
    most part.
  * Performance is highly dependant on your terminal emulator. I can personally
    recommend `sakura` and `xterm`.

### The build fails with some obscure error message

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
