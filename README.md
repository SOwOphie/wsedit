# Wyvernscale Source Code Editor (wsedit)

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

* __Character class highlighting__: Not as powerful as full-on syntax
  highlighting, it will instead color your text by character class (e.g.
  operators -> yellow, brackets -> brown, numbers -> red, ...).  This looks
  almost as good as the real deal in most modern languages, will hardly
  misbehave, and work with every language out there (even natural ones, like
  English) without requiring anyone to write syntax files!

* __The usual selection editing, interacting directly with the system
  clipboard__: Make sure to have `xclip` or `xsel` installed.

* __Easiest possible method of configuration__: Type `wsedit -cg` (global) or
  `wsedit -cl` (directory-local) to open the configuration file, then put down
  all the command line parameters you'd like to be default.  Prefix lines with
  e.g. `hs:` to make them apply to .hs-files only.

* __Numbered lines__: I know it isn't much, but hey!

## Building

1. Install the
   [Haskell Tool Stack](http://docs.haskellstack.org/en/stable/README/).
2. Install either `xclip` or `xsel` with your package manager.
3. Clone the repository (`git clone https://github.com/SirBoonami/wsedit`).
4. Run `stack setup` to pull in the correct version of `ghc`.
5. Run `stack install` to build the dependencies and `wsedit`.
6. Either:
    * Add `~/.local/bin/` to your `$PATH`
    * Copy `~/.local/bin/wsedit` to a directory in your path, e.g.
      `/usr/local/bin/`.
7. Run `wsedit <some file>` to test it, see the keybinds table below.

Sometimes the build may fail due to obscure reasons, deleting the local
`.stack-work` build folder fixed it everytime for me.

## Known issues

* `wsedit` may be a bit on the slow side on older systems. Use `-b` to disable
  background rendering, which remedies this for the most part.

* `wsedit` is currently designed for dark, but not completely black terminals.
  Support for bright colour schemes is coming some time in the near future.
  Since the background won't be rendering on black terminals anytime soon, set
  `-b` to disable it and save some performance.

* `wsedit` crashes violently (devastating your terminal in the process, use
  `reset` to get it back to normal) when opening a binary file. I will fix this
  as soon as I think of a good way to:
     * detect binary files
     * behave instead (simply refusing to open is one thing, but being shown a
       hexdump and eventually even editing files in hex would be much cooler)

## Current keybindings (at the moment not to be found anywhere else beside
`Keymaps.hs`)

Key | Action
----|--------
**Ctrl + Q** | Quit.
**Ctrl + Meta + Q** | Force quit (in case of unsaved changes blocking your way out).
**Ctrl + Meta + C** | Crash simulator 3000. Do not press.
**Ctrl + u/d/l/r** | Move the viewport 10 characters.
**Ctrl + Up/Down/Left/Right** | Move the cursor 10 characters.
**Ctrl + Space** | Move the cursor to the upper left corner of the viewport.
**Shift + _Movement_** | Move the cursor, selecting text.
**Tab** | Indent selection *or* apply autocomplete *or* advance to the next tab stop.
**Shift + Tab** | Unindent selection.
**Ctrl + C** | Copy selection to clipboard.
**Ctrl + X** | Cut selection to clipboard.
**Ctrl + V** | Paste clipboard over selection.
**Ctrl + Z** | Undo last action.
**Ctrl + S** | Remove trailing space and save.
**Ctrl + Meta + Tab** | Toggle tab replacement (see the icon in the lower left corner).
**Ctrl + Meta + R** | Toggle read-only mode.
