# Wyvernscale Source Code Editor (wsedit)


## Introduction

`wsedit` is a neat **little** (as in *don't expect too much from a student's
first piece of code on github*) terminal-based editor written in haskell,
sitting comfortably in the niche between `nano` and `vim`.  It is designed to be
intuitive (as in "Press Ctrl-C to copy stuff"), simple, elegant and pragmatic.


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
  supplementing the already mentioned syntax highlighting quite nicely.

* __Simple configuration interface via config files__: Not really much to say
  here.

* __Read-only mode__: Want to glance over a file without accidentally editing
  it? Start the editor in read-only mode, or toggle it via keybind.

* __The usual selection editing, interacting directly with the system
  clipboard__: Make sure to have `xclip` or `xsel` installed; an internal
  fallback is provided.


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

1.  Install the
    [Haskell Tool Stack](http://docs.haskellstack.org/en/stable/README/).
2.  Make sure you have `ncurses` with unicode support installed.
3.  *Optional*, Linux only: Install either `xclip` or `xsel` with your package
    manager. If this step is skipped, `wsedit` will use an internal buffer
    instead of the system facilities for copy/paste functionality.
4.  Clone the repository (`git clone https://github.com/SirBoonami/wsedit`).
5.  `cd` into the newly created directory (`cd wsedit`).
6.  Run `stack setup` to pull in the correct version of `ghc`.
7.  Run `stack install` to build the dependencies and `wsedit`.
8.  Either:
    * Add `~/.local/bin/` to your `$PATH`
    * Copy `~/.local/bin/wsedit` to a directory in your `$PATH`, e.g.
      `/usr/local/bin/`.
9.  To install language definitions, create the folder `~/.config/wsedit` and
    paste them there.  Quite a few languages and formats have pre-defined
    highlighting rules in the `lang` subdirectory of this repository, feel free
    to write your own and create a pull request!
10. Run `wsedit <some file>` to test everything, or `wsedit -h` for a list of
    all the available options.


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

1. Dump the current state of your file to `~/CRASH-RESCUE` if it has been
   modified since the last save.
2. Dump the editor's configuration, state and some additional info to
   `~/CRASH-DUMP`. This file can be used to restart the editor in the last
   coherent state before it crashed.
3. Shutdown `vty` + `ncurses` so your terminal doesn't get rekt.

This means that even in the event of a crash, data loss is highly unlikely to
occur.

The state dump is of great importance to fixing the bug. However, it contains
your entire configuration as well as the entire file you edited when the crash
happened. Make sure you're okay with that becoming public before uploading it.
Also, please don't provide a modified dump file, as any changes made will throw
off the line hash based caching system.

### Non-fatal bugs

Most non-fatal bugs will probably be rendering glitches. Reproduce the
situation, point the cursor at it if possible, then press Meta + ".".
This will simulate a crash and create the above-mentioned files.


## Known issues / Troubleshooting

### `wsedit` is slow on older machines

  * Use `-b` to disable background rendering, which remedies this for the most
    part.
  * Performance is highly dependant on your terminal emulator. I can personally
    recommend `rxvt-unicode` and `xterm`.

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

Yeah, input in `urxvt` is a mess. Try adding this to your `.Xresources`:

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
