# Wyvernscale Source Code Editor (wsedit)

# IMPORTANT NOTICE regarding the `0.3.*` update

* The parameter syntax for highlighting has changed slightly, therefore your old
  config files may be invalid.  To avoid breakage, we now use new file locations:

  * `~/.config/wsedit.wsconf` instead of `~/.config/wsedit.conf`
  * `./.local.wsconf` instead of `./.wsedit`

  Check out the new syntax for `-f*` parameters with `wsedit -h`.

* Entire language definitions are now available in the `lang/` subdirectory.
  Install them as follows:

  * `cd` into the main git directory, `git pull origin master` to ensure you
    have the latest version.
  * Run `lang/install.sh` to list all available languages.
  * Use `lang/install.sh <language name>` to append a definition to your
    global config.
  * Pull requests for your favourite language are always welcome!

## Introduction

`wsedit` is a neat **little** (as in *don't expect too much from a student's
first piece of code on github*) terminal-based editor written in haskell,
sitting comfortably in the niche between `nano` and `vim`.  It is designed to be
intuitive (as in "Press Ctrl-C to copy stuff"), work out-of-the-box with every
conceivable language and to require only a minimal amount of configuration.

## Features

* __Read-only mode__: Want to glance over a file without accidentally editing
  it?  Start the editor in read-only mode (by passing `-r`), and toggle it in
  the editor with `Ctrl-Meta-R`.

* __Dynamic dictionary-based autocompletion__: When activated, everytime you
  load or save, `wsedit` will read all files with the same ending as the one
  you're currently editing, filter all lines by indentation depth and build a
  dictionary out of those at a specified level.

* __Pragmatic syntax highlighting__: Highlights keywords, strings and comments
  according to your configuration file.  Default patterns are availabe in
  `lang/*.wsconf`.

* __Character class highlighting__: Not as powerful as full-on syntax
  highlighting, it will instead color your text by character class (e.g.
  operators -> yellow, brackets -> brown, numbers -> red, ...).  This, in
  combination with the syntax highlighting, offers a comfortable editing
  experience while being easy to tweak yourself.

* __The usual selection editing, interacting directly with the system
  clipboard__: Make sure to have `xclip` or `xsel` installed; an internal
  fallback is provided.

* __Easiest possible method of configuration__: Type `wsedit -cg` (global) or
  `wsedit -cl` (directory-local) to open the configuration file, then put down
  all the command line parameters you'd like to be default.  Prefix lines with
  e.g. `hs:` to make them apply to .hs-files only.

## Platforms / Installation

### Windows: not supported

The whole idea of running a terminal editor on Windows seems a bit strange to
me.  However, you are free to try building it from source (see below).

### Linux: no packages available (yet), build from source

Until the v1.0 release, there won't be any packages available.  After that, I'll
probably create an AUR package.  Contact me if you're interested in packaging
wsedit for your distribution.

### OSX: Homebrew

Thanks to Alex Arslan for providing a homebrew formula for wsedit:

    brew tap ararslan/pints
    brew install wsedit

### Building from source

1. Install the
   [Haskell Tool Stack](http://docs.haskellstack.org/en/stable/README/).
2. *Optional*: Install either `xclip` or `xsel` with your package manager.  If
   this step is skipped, `wsedit` will use an internal buffer instead of the
   system facilities for copy/paste functionality.
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
9. Run `wsedit <some file>` to test everything, or `wsed -h` for a list of all
   the available options.

## Known issues / Troubleshooting

### wsedit is slow on older machines

  * Use `-b` to disable background rendering, which remedies this for the most
    part.
  * Consider switching to a faster terminal emulator, e.g. rxvt-unicode.

### The build fails with some obscure error message

  * Try `stack clean`.
  * If that doesn't work, delete the `.stack-work` folder and try again.

### wsedit destroys Unicode on XTerm

__Symptoms:__ After running `wsedit`, any unicode output by other programs (e.g.
`tree`) will be garbled.

This seems to be a problem wit vty, the terminal library wsedit uses, since
yi, another terminal editor based on vty, suffers from the same issue. For now I
can only recommend using another terminal emulator.

### Some inputs (e.g. Ctrl-Down) don't work in urxvt

Yeah, input in urxvt is a mess. Try adding this to your .Xresources:

    ! From http://thedarnedestthing.com/urxvt
    urxvt*keysym.C-Up: \033[1;5A
    urxvt*keysym.C-Down: \033[1;5B
    urxvt*keysym.C-Right: \033[1;5C
    urxvt*keysym.C-Left: \033[1;5D
    urxvt*keysym.S-Up: \033[1;2A
    urxvt*keysym.S-Down: \033[1;2B
    urxvt*keysym.S-Right: \033[1;2C
    urxvt*keysym.S-Left: \033[1;2D

    urxvt*iso14755: False
    urxvt*iso14755_52: False
