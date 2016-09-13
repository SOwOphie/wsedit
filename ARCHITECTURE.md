# Architectural overview

This document outlines the system architecture of `wsedit`. It is not meant for
the average user, but for those who want to understand and maybe even improve
the source code. It might at some point start lagging behind the actual code,
but I will try to keep it updated for every major change. I'd also strongly
recommend you to run `stack haddock` and take a look at its output (located at
`.stack-work/install/<arch>-<os>/<stackVer>/<ghcVer>/doc/all/index.html`).

## Files and Modules

The file tree looks somewhat like this:

    exec/
        Main.hs
    lang/
        < many .wsconf files >
    Setup.hs
    stack.yaml
    WSEdit/
        Buffer.hs
        Control/
            Autocomplete.hs
            Base.hs
            Marks.hs
            Selection.hs
            Text.hs
        Control.hs
        Data/
            Algorithms.hs
            Pretty.hs
        Data.hs
        Help.hs
        Keymaps.hs
        Output.hs
        Renderer.hs
        Util.hs
        WordTree.hs
    wsedit.cabal
    WSEdit.hs

Let's go through them one by one.

 * __exec/Main.hs__: This file is the root of the executable `wsedit`, but it
   contains no actual code. Necessary due to the library/executable distinction
   made by `cabal`.

 * __lang/*.wsconf__: Collection of language definitions. If you've come this
   far, you should be pretty familiar with them, otherwise take a brief look at
   the output of `wsedit -hc`.

 * __Setup.hs__: Another auto-generated `cabal` artifact. I'm actually not sure
   what it even does.

 * __stack.yaml__: `stack`'s main config file, mostly auto-generated. Contains
   information on how to obtain packages (I'm sure there are better explanations
   out there if you're really curious).

 * __wsedit.cabal__: `cabal`'s main config file, details the build process.
   Should be self-explanatory.

Now that we've covered all the boring boilerplate, let's get to the juicy part.

 * __WSEdit.hs__: Main file, gets called first. Contains the whole
   initialization process, options parsing, the main loop and runtime error
   catching / disaster procedures.

   * __WSEdit/Data.hs__: Declares all data structures as well as their default
     instances. Corresponds to the __Model__ part of the MVC design pattern.

     * __WSEdit/Data/Algorithms.hs__: Contains some algorithms used to interface
       with the structures declared in `WSEdit/Data.hs`.

     * __WSEdit/Data/Pretty.hs__: Declares reduced versions of some data
       structures that can be `read` and `show`n as well as functions converting
       between them and the real deal. Integral to the `CRASH-DUMP` mechanic.

   * __WSEdit/Keymaps.hs__: Declares the default keymap as a pairing of
     _key combination_, _handler_ and _help text_.

   * __WSEdit/Control.hs__: Aggregator for its sub-modules, contains no actual
     code. These modules define the handler functions used mostly in
     `WSEdit/Keymaps.hs`, but also in some other places, as well as some common
     building blocks for handler functions. Corresponds to the __Controller__
     part of the MVC design pattern.

     * __WSEdit/Control/Base.hs__: Some common actions, e.g. moving the cursor,
       creating an undo history entry, displaying a loading screen, ...

     * __WSEdit/Control/Autocomplete.hs__: Contains everything related to tab
       completion.

     * __WSEdit/Control/Global.hs__: Some functions operating on a global scale,
       e.g. termination, saving, toggling various editing modes, undo, ...

     * __WSEdit/Control/Marks.hs__: Handlers related to jump marks.

     * __WSEdit/Control/Selection.hs__: Handlers related to selection editing,
       e.g. copy/paste, (un-)indenting a block, ...

     * __WSEdit/Control/Text.hs__: Text editing stuff like inserting a
       character, ...

   * __WSEdit/Output.hs__: Everything related to producing a coherent image on
     the screen. Especially the main `draw` call and its subcomponents, but also
     some utility functions like getting display bounds, character widths, ...
     Corresponds to the __View__ part of the MVC design pattern.

   * __WSEdit/Renderer.hs__: A bit of a misnomer actually. Contains the
     functions that rebuild the rendering caches. Since they're intended to be
     called in immediate succession, only one meta-function is exported.

   * __WSEdit/Help.hs__: Contains help strings as well as some functions related
     to formatting them.

   * __WSEdit/Util.hs__: Unsorted collection of helper functions that are not
     necessarily related to `wsedit`.

   * __WSEdit/Buffer.hs__: Declares the data structure used to store the edited
     file in an efficient way. Loosely based on
     [PointedList](http://hackage.haskell.org/package/pointedlist)s, but with
     some additional features to improve efficiency for our use case.

   * __WSEdit/WordTree.hs__: Implements the prefix tree used as the autocomplete
     dictionary.

## System outline

There are two central data structures that everything in `wsedit` revolves
around: `EdState` and `EdConfig`, both declared in `WSEdit/Data.hs`. They are
very similar to each other in that together, they hold all the information there
is to `wsedit`. (Nearly) all code runs in the `WSEdit` monad, which is an `IO`
monad enriched with a `MonadReader` for `EdConfig` and a `MonadState` for
`EdState` by the convenient `RWST` monad transformer. This means that all code
can only access data that is in either of those two structures and only write
that which is in `EdState` (ignoring possible IO interaction).

The whole compartmentalization concept is set up around that principle in that
every function reads what it needs from those two structures and then puts the
results of its work into `EdState`.

This reflects in the structure of the main loop, which is:

 1. Draw Call, uses the structures to draw an image
 2. Event Handler, rearrange `EdState` in some way.
 3. GoTo 1

All this runs inside an exception handler that will dump the structures should
an error occur. This can later be used to trace what went wrong and even to
re-launch the editor in the state before the error occured.

## The Rendering Pipeline

<TODO>
