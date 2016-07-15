# Wyvernscale Source Code Editor (wsedit)

## Introduction

`wsedit` is a neat **little** (as in *don't expect too much from a student's
first piece of code on github*) terminal-based editor written in haskell,
sitting comfortably in the niche between `nano` and `vim`.  It is designed to be
intuitive (as in "Press Ctrl-C to copy stuff"), simple, elegant and pragmatic.

## Features

* __Dynamic dictionary-based autocompletion__: Specify which files to index, and
  how (e.g. ignore comments, only global definitions, ...).

* __Pragmatic syntax highlighting__: Highlights keywords, strings and comments
  declared in your config files.  Default patterns for some languages are
  availabe for import in `lang/*.wsconf`, writing your own should take no longer
  than 30 minutes, including the submission of a pull requrest on GitHub =).

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

## Upcoming features / Developer's wishlist

__There is no guarantee that any of these features will ever see the light of
day!__

In no particular order:

* __Support for block comments.__ Unfortunately, our current, line-based
  renderer cannot do it.

* __Elastic tabstops.__ Another thing our current renderer cannot do.

* __Bracket matching.__ Are you starting to notice a pattern here?

## Platforms / Installation

### Windows: not supported

The whole idea of running a terminal editor on Windows seems a bit strange to
me.  However, you are free to try building it from source (see below).

### Linux: use the precompiled binary (`x86_64` only) **or** build from source

Check out __Releases__ on the bottom of the GitHub page header (directly above
the green bar).

Contact me if you want to package `wsedit` for your distribution.

### OSX: Homebrew

Thanks to Alex Arslan for providing a homebrew formula for wsedit:

    brew tap ararslan/pints
    brew install wsedit

### Building from source

1. Install the
   [Haskell Tool Stack](http://docs.haskellstack.org/en/stable/README/).
2. *Optional*, Linux only: Install either `xclip` or `xsel` with your package
   manager. If this step is skipped, `wsedit` will use an internal buffer
   instead of the system facilities for copy/paste functionality.
3. Clone the repository (`git clone https://github.com/SirBoonami/wsedit`).
4. `cd` into the newly created directory (`cd wsedit`).
5. Run `stack setup` to pull in the correct version of `ghc`.
6. Run `stack install` to build the dependencies and `wsedit`.
7. Either:
    * Add `~/.local/bin/` to your `$PATH`
    * Copy `~/.local/bin/wsedit` to a directory in your `$PATH`, e.g.
      `/usr/local/bin/`.
8. To install language definitions, create the folder `~/.config/wsedit` and
   paste them there.  Quite a few languages and formats have pre-defined
   highlighting rules in the `lang` subdirectory of this repository, feel free
   to write your own and create a pull request!
9. Run `wsedit <some file>` to test everything, or `wsedit -h` for a list of all
   the available options.

## Known issues / Troubleshooting

### `wsedit` is slow on older machines

  * Use `-b` to disable background rendering, which remedies this for the most
    part.
  * Performance is highly dependant on your terminal emulator. I can personllay
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
