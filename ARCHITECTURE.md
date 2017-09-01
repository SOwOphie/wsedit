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
        Arguments/
            Data.hs
            Parser.hs
        Arguments.hs
        Buffer.hs
        Control/
            Autocomplete.hs
            Base.hs
            Marks.hs
            Selection.hs
            Text.hs
        Data/
            Algorithms.hs
            Pretty.hs
        Data.hs
        ElasticTabstops.hs
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

Now that we've covered all the boring boilerplate, let's get to the juicy part!

 * __src/WSEdit.hs__: Main file, gets called first. Contains the main loop and
   runtime error catching / disaster procedures.

   * __src/WSEdit/Arguments.hs__: Handles all the argument processing.

     * __src/WSEdit/Arguments/Data.hs__: Contains some data types used for
       argument processing.

     * __src/WSEdit/Arguments/Parser.hs__: Evil module full of black parsec
       magic.

   * __src/WSEdit/Buffer.hs__: Declares the data structure used to store the
     edited file in an efficient way. Loosely based on
     [PointedList](http://hackage.haskell.org/package/pointedlist)s, but with
     some additional features to improve efficiency for our use case.

   * __src/WSEdit/Control/__: These modules define the handler functions used
     mostly in `WSEdit/Keymaps.hs`, but also in some other places, as well as
     some common building blocks for handler functions. Corresponds to the
     __Controller__ part of the MVC design pattern.

     * __src/WSEdit/Control/Autocomplete.hs__: Contains everything related to
       tab completion.

     * __src/WSEdit/Control/Base.hs__: Some common actions, e.g. moving the
       cursor, creating an undo history entry, displaying a loading screen, ...

     * __src/WSEdit/Control/Global.hs__: Some functions operating on a global
       scale, e.g. termination, saving, toggling various editing modes, undo,
       ...

     * __src/WSEdit/Control/Marks.hs__: Handlers related to jump marks.

     * __src/WSEdit/Control/Selection.hs__: Handlers related to selection
       editing, e.g. copy/paste, (un-)indenting a block, ...

     * __src/WSEdit/Control/Text.hs__: Text editing stuff like inserting a
       character, ...

   * __src/WSEdit/Data.hs__: Declares all data structures as well as their
     default instances. Corresponds to the __Model__ part of the MVC design
     pattern.

     * __src/WSEdit/Data/Algorithms.hs__: Contains some algorithms used to
       interface with the structures declared in `WSEdit/Data.hs`.

     * __src/WSEdit/Data/Pretty.hs__: Declares reduced versions of some data
       structures that can be `read` and `show`n as well as functions converting
       between them and the real deal. Integral to the `CRASH-DUMP` mechanic.

   * __src/WSEdit/ElasticTabstops.hs__: Contains the implementation of elastic
     tabstops, exported as a single function to rebuild the related cache.

   * __src/WSEdit/Help.hs__: Contains help strings as well as some functions
     related to formatting them for output.

   * __src/WSEdit/Keymaps.hs__: Declares the default keymap as a pairing of
     _key combination_, _handler_ and _help text_.

   * __src/WSEdit/Output.hs__: Everything related to producing a coherent image
     on the screen. Especially the main `draw` call and its subcomponents, but
     also some utility functions like getting display bounds, character widths,
     ... Corresponds to the __View__ part of the MVC design pattern.

   * __src/WSEdit/Renderer.hs__: A bit of a misnomer actually. Contains the
     functions that rebuild the rendering caches. Since they're intended to be
     called in immediate succession, only one meta-function is exported.

   * __src/WSEdit/Util.hs__: Unsorted collection of generic helper functions
     that are not necessarily related to `wsedit`s data model.

   * __src/WSEdit/WordTree.hs__: Implements the prefix tree used as the
     autocomplete dictionary.

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

Rendering the output of `wsedit` is a three-step process:

 1. Extract all syntactically relevant tokens from each line of text and put
    them into a cache. Some tokens may have no relevance at the moment (e.g. a
    single quote inside a double quoted string), this will have to be taken into
    account in step 2.

 2. Run a FSM over these lines to obtain ranges of to-be-coloured text as well
    as ranges of matching brackets. Put these into caches as well.

 3. On the actual draw call, look up each character's position in the caches
    from step 2 to determine its designated colour.

This process is optimized by smart recalculation as follows:

 1. Since changes in the document are most likely to happen close to the cursor,
    we can use a blockchain-like hashing scheme to do fast difference checks
    (this one is a bit hard to explain, take a look at `WSEdit.Buffer`s
    implementation, especially the unexposed `HashList` functions). As the
    results of the last run are stored per line, we can simply cut out all the
    outdated lines from the old cache and replace them by running step 1 over
    the altered area again. Due to the data structure used for the cache (also a
    `WSEdit.Buffer`), this is pretty memory-efficient, since the unchanged areas
    will not take up new space.

 2. We don't need to calculate any formatting beyond the visible area, so we
    simply don't do that. As the FSM's state leaving each line is stored in the
    cache it writes, we can resume parsing tokens at the first changed line and
    stop as soon as we hit the lower edge of the screen. Also, since the cache
    is stored as a regular list in reverse order, the common tail of two steps
    won't take up any additional space.

For some operations it is necessary to ignore all existing caches (e.g. changes
in search term highlighting). `EdState` contains the field `fullRebdReq :: Bool`
to facilitate just that.
