# v1.2.2

## New Features

### Elastic Tabstops

`wsedit` now supports Elastic Tabstops as introduced on
http://nickgravgaard.com/elastic-tabstops/ . Enable via `-el`.

## Packaging Changes

These changes should make packaging `wsedit` a lot easier than it used to be.

### Adjusted build flags, added new development build flag

- By default, `wsedit` will no longer be compiled using `-Werror`. This should
  increase compiler compatibility both forwards and backwards.
- Debugging functions will no longer be compiled in by default. Therefore, the
  `time` package is no longer required.

- Passing `--flag wsedit:dev` to stack will produce a development build. This:

     - enables `-Werror`
     - disables `-O2` to speed up recompilations
     - creates a separate executable called `wsed-dev`
     - enables debugging functions
     - pulls in `time` again
     - sets the badge text to "!!! DEVELOPMENT BUILD !!!"

### Additional, system-wide config location

Placing files into `/etc/wsedit/` will also work now. I've also added a
primitive `install_lang.sh` that takes care of that automatically while
complying to the `$DESTDIR` convention.

## Fixes

- Fixed another instance of the viewport not following the cursor correctly.
- Fixed a config parse error with paths containing subdirectories.
- Made it possible to include escaped quotes inside quoted options, e.g.
  `-lk "this\"is\"stupid\"but\"works\"now"`

## Other Changes

- Local config files are now parsed independently of the working directory:
  Starting from the file system root ("/"), check for a ".local.wsconf" file,
  parse it if possible, descend into the folder leading to the opened file.
  Repeat for every folder along the way.
- Due to this, the way file matching works has been adjusted. If no path prefix
  is given, only the file name portion will be matched. Otherwise, the full path
  is matched. Relative paths are relative to the parent folder of the config
  file containing them, or to the current working directory if given via
  command-line argument. For examples, see `wsed -hc`.
- Improved the text rendering algorithm for `-h*`. It now prioritizes small
  words instead of large ones for additional padding.
- When opening the file at a specific line, the viewport will be placed so that
  the target line is at 1/3 of the visible area instead of the bottom.

# v1.2.1

This release doesn't contain any big features, but lots of small improvements
and bug fixes.

## New Features

### Overlay Badge

Since "Fork me on GitHub" badges are so popular these days I decided to join in
on the hype. The `-ds <str>` switch can now be used to place a ribbon with
arbitrary text over the top right corner of the editor. It was a cheap change
for a cheap joke but maybe somebody will get something useful out of this.

### Keybinds Menu

Another step towards increasing user-friendliness. Press `F1` to view all key
bindings.

## Other Changes

- Lots of new language definitions, thanks to @ararslan for providing the
  majority of them =)
- Renamed the executable to `wsed`. I've meant to do this from the very
  beginning but I haven't gotten around to looking it up until now.
- Atomic saves can now be disabled using `-fa`.
- The config directory traversal is now deep, i.e. files in subdirectories will
  now also be read.
- Adjusted the colour scheme of unprintable characters to be a little bit less
  obnoxious.
- `wsedit` now prompts before opening large files, as the initial cache building
  can take up to some minutes for bigger logs.
- The status box (bottom left corner) is now coloured.
- Switched to stack `lts-8.0`, making use of the new `ghc 8.0.2`.
- Escape sequences can now be longer than a single character.
- Retroactively added `CHANGES.md` to mirror the GitHub releases page.

## Fixes

- Fixed an error where the cursor position detection for bracket highlighting
  was off by one character.
- Saving a file with jump marks present no longer crashes the editor.
- Fixed `-ad` not picking up subdirectory rules correctly.
- The viewport is now limited to the text's bounds, at least one row / column
  must be displayed at any time.
- More performance tweaking.
- `ssh` was sometimes breaking the clipboard, it should no longer do that.
- The clipboard file (if present) is no longer world-readable by default.



# v1.2.0

This is going to be the last "big" release with sweeping changes for the
foreseeable future, since I consider `wsedit` somewhat feature-complete. Of
course smaller updates will still happen, but don't expect any mind-blowing new
bells and whistles.

## New Features

### Atomic saves

This feature ensures that even in the case of a power outage during write, your
data will be protected.

### Sanity check on save

`wsedit` will now verify that the file has been written correctly and take
appropriate measures if it wasn't.

### New line counter in the bottom right corner

### Release stability check

The editor can now be in onew of four stability states:
- Release
- RC
- WIP
- Prototype

By default, only _Release_ instances will start, any other version will warn you
about its lack of stability and exit. You can use e.g. `-ys WIP` to accept all
stability levels down to _WIP_.

## Other Changes

### Completely rewrote the options system

For the new syntax of config files, check `wsedit -hc`.

The existing options all got new, hierarchical names to keep the namespace clean
as well as make memorizing them a bit easier. Also, the way you give parameters
to arguments has changed. Furthermore, now double quotes can be used to enclose
spaces, e.g. `"end function"`.

The new commands are:

```
-! ==> -mf
-b ==> -dB (now default behaviour)
-B ==> -db (no longer default behaviour)
-cg ==> -ocg
-cl ==> -ocl
-d+<n><f> ==> -ad <n> <f> (New syntax for file matching)
-d~+<n> ==> -as <n>
-D ==> -A
-e<s> ==> -fe <s>
-E ==> -fE
-fbc+<s1>_<s2> ==> -lcb <s1> <s2>
-fbr+<s1>_<s2> ==> -lb <s1> <s2>
-fbr-<s1>_<s2> ==> -lB <s1> <s2>
-fe+<c> ==> -les <c> and -leo <c>
-fe- ==> -leS and -leO
-fh+<s> ==> -gh <s>
-fh-<s> ==> -gH <s>
-fk+<s> ==> -lk <s>
-fk-<s> ==> -lK <s>
-flc+<s> ==> -lcl <s>
-flc-<s> ==> -lcL <s>
-fms+<s1>_<s2> ==> -lsm <s1> <s2>
-fms-<s1>_<s2> ==> -lsM <s1> <s2>
-fs+<s1>_<s2> ==> -lsr <s1> <s2>
-fs-<s1>_<s2> ==> -lsR <s1> <s2>
-fsc+<s1>_<s2> ==> -lsc <s1> <s2>
-fsc-<s1>_<s2> ==> -lsC <s1> <s2>
-h unchanged
-hc unchanged
-hk unchanged
-i<n> ==> -ei <n>
-j<n> ==> -ej <n>
-J<n> ==> -eJ <n>
-lu ==> -flu
-lw ==> -flw
-L ==> -fL
-p ==> -op
-P ==> -oP
-r ==> -gr
-R ==> -gR
-s ==> -ms
-ts ==> -ets
-tt ==> -ett
-T ==> -eT
-V ==> -hv
-x ==> -dx
-X ==> -dX
-y ==> -ye
-Y ==> -yE
```

Some time ago, I foolishly promised a `sed` script for easier migration. Even
though I consider myself a man of my word, this will _not_ initially be
provided, because:

- The new syntax provides much better ways of declaring options, which can't be
  mapped 1:1 by a simple script. Therefore, anything created by that
  hypothetical script would work, but be ... let's say ... less than optimal
  regarding performance and readability, and in need of manual changes anyways.
- I suspect that the ~3 people out there using `wsedit` haven't bothered to
  create huge configs that would require automatic updates.

**But!** If you would find such a `sed` script helpful, create an issue and I
will deliver.

### Crash dumps and rescue files now end up in the home directory instead of the
    working directory

Now your data isn't lost if your editor crashes inside a non-writable folder.

### Split up escape characters

There are now two versions of escape characters: one for inside strings, and one
for outside. This is necessary to display both the haskell sequence `"asdf"\'s'`
(read: `"asdf"` without `'s'`) and the bash sequence `echo \"` (read: print a
quotation mark) correctly.

### Cleaned up character strings

The  definition of a character string is now:

```
Either
 * <opening sequence><single char><closing sequence>
 * <opening sequence><escape char><single char><closing sequence>
```

instead of some wonky `maximum length <= 2`.

### Some more optimizations for the rendering pipeline

Probably hardly noticable for smaller files, but should make scrolling log files
much smoother.

### Changed the behaviour of dictionary scanning.

- You can now specify wildcarded file paths.
- Scanning now ignores hidden folders and files. This may decrease the startup
  time by orders of magnitude, depending on your directory layout. For my
  version of the `wsedit` repository, the change cuts down the amount of scanned
  files from 9076 to 53.

### We now use the shiny, new GHC 8.0.1.

Not much to say here.

## Fixes
- The first input no longer takes ages to process on huge files.
- Cursor position after multi-line paste operations should now be correct.




# v1.1.0

Oh, you thought this was just another abandoned pet project, did you not? Well
fasten your seatbelts, because crafted using dark, forbidden algorithms devised
in the shady depths of a shaded hammock somewhere in Croatia, Release 1.1 is
here, bigger, better, faster and cooler than ever before!

## New features

### Highlight: New Rendering Engine

Featuring:
- Block Comments
- Multi-line strings
- Bracket highlighting

Take a look at `wsedit -h` to see how to get started.

### Additional optimizations

`wsedit` should now be much faster.

### Loading screens

You will now see what `wsedit` is doing while it's doing what it's doing instead
of staring at an unresponsive editor screen.

### More source code documentation forr all you hackers out there

The new file `ARCHITECTURE.md` details the internals of `wsedit`. Using it in
conjunction with `haddock` + `hscolour` output should make understanding my
syntactic ramblings much easier.

## Fixes

- Fixed some issues regarding other character encodings. Shit's still somewhat
  broke though.
- Finally implemented `-j` to allow you to set jump marks from the command line
  / config files.
- `-!` didn't work for integer parsing errors, fixed that one as well.
- `-s` crashed ungloriously on parse errors, taking the terminal with itself in
  the process. Created an error message for this.



# v1.0.0

## Fixes
- `-d~` should now work correctly.
- Build should now be much faster.
- Fixed some issues with copying and pasting newlines.



# v1.0.0RC

## New Features

### New help section.
- Now with text justification. Death to all left-aligned paragraphs!
- The general help is now divided into sections.
- The keybinds help now also fits into 80 columns and wraps as needed.

### Added `-ff` to override the file extension.

Shell scripts, for example, seldomly end on `.sh`. Now you can run
`wsedit -ffsh /etc/profile` to remedy that. Unfortunately, this option is not
quite ready for use inside config files, but this is (almost) certain to come at
a later date.

### Added `-!` to ignore command errors.

Got your global config broken? Tired of using nano to clean it up? Use
`wsed -! -cg` to ignore all errors and clean up your mess!

### Added Meta-<movement> to jump 100 lines.

Not really much to say about this, see `wsedit -hk` for more info.

### Added options for file encoding and line separator.

`wsedit` will open every file, but rewrite it to your locale's settings, unless
specifically told otherwise. On that note:

### Improved rendering for invalid characters.

Unprintable characters and unicode marks will now show up as `?#<abcd>;`, where
`<abcd>` is the hexadecimal code for that character. With this, `wsedit` can now
open (and somewhat edit) binary files!

## Fixes
- Typing no longer creates two undo states per character.



# v0.4.0

## New features

### Jump marks
- Press Ctrl-Y to toggle a jump mark in your current line.
- Press Ctrl-B/Ctrl-N to jump to the previous/next jump mark or search term
  (`-fh`).

### Overwrite mode

You know, the thing where you press Ins and overwrite your previously written
text.

### New dictionary

You can now explicitly specify:
- explicit file filters via suffix, e.g. `.hs`, `WSEdit/Data.hs`, ...
- multiple (or no) indentation levels per filter.
- special rules for your current file.

**The old `-d` syntax is incompatible with these changes**, make sure to modify
your config files! (I don't want to accumulate heaps of deprecated / legacy
features before even v1.0...)

## Fixes
- Fixed `-s` ungloriously crashing the editor.
- Fixed a minor cursor misplacement when pressing Backspace at the start of a
  line.



# v0.3.1

- New Search / Highlight function:
  - Use `-fh+<xxx>` to mark <xxx> as a search term
  - OR select some text, then press Ctrl-F to add it to the search terms stack
  - All search terms will appear in bright red
  - Press Ctrl-F without having anything selected to pop one entry off the
    search terms stack
- Crash dumps sholud now be much smaller
- Added a vertical scroll position indicator
- New directory for language definitions: `~/.config/wsedit`. Place all your
  language modules there instead of having to paste them into your global
  config!
- New rendering code increases performance noticably
- `-b` now keeps the lines intact, since the speed increase comes almost
  exclusively from not rendering the dots.
