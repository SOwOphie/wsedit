{-# LANGUAGE LambdaCase #-}

module WSEdit.Help
    ( confHelp
    , keymapHelp
    , usageHelp
    , versionHelp
    ) where


import Data.Char          (toUpper)
import Data.List          (delete, sortOn)
import Data.Maybe         (catMaybes)
import Data.Ord           (Down (Down))
import Graphics.Vty       ( Button (BLeft, BMiddle, BRight)
                          , Event (EvKey, EvMouseDown)
                          , Key (KBS, KChar, KFun)
                          , Modifier (MCtrl, MMeta, MShift)
                          )
import Safe               (lastDef, maximumNote)

import WSEdit.Data        (Keymap, licenseVersion, upstream, version)
import WSEdit.Data.Pretty (prettyKeymap)
import WSEdit.Util        ( chunkWords, padRight, rotateR, withFst, withN
                          , withSnd
                          )



fqn :: String -> String
fqn = ("WSEdit.Help." ++)



-- | Aligns the given lines for the given amount of columns.
renderText :: Int -> [String] -> String
renderText nCols = unlines . map (renderLine nCols)
    where
        spl :: String -> [(Int, String)]
        spl []       = []
        spl (' ':xs) = case spl xs of
                            (r:rs) -> withFst (+1) r : rs
                            []     -> []

        spl ( x :xs) = case spl xs of
                            (r:rs) | fst r == 0 -> withSnd (x:) r : rs
                            l                   -> (0, [x]) : l


        padBy :: [Int] -> Int -> [(Int, String)] -> [(Int, String)]
        padBy (x:xs) n l | n > 0     = padBy xs (n-1) $ withN x (withFst (+1)) l
                         | otherwise = l
        padBy []     _ _             = error "padBy: list too short."


        renderLine :: Int -> String -> String
        renderLine n s | length s > n = "[!!!] " ++ s ++ " [!!!]"
                       | otherwise    =
                        let
                            sp     = spl s
                            maxPad = 2 * sum ( map fst
                                             $ filter ((== 1) . fst) sp
                                             )

                            lenPrm = map fst
                                   $ sortOn (Down . snd . snd)
                                   $ filter ((== 1) . fst . snd)
                                   $ zip [0..]
                                   $ zipWith (\(_, s1) (m, s2) -> (m, length s1
                                                                    + length s2
                                                                  )
                                             ) (rotateR sp) sp

                        in
                            if n - length s > maxPad || lastDef '.' s == '.'
                               then s
                               else concatMap (\(m, w) -> replicate m ' ' ++ w)
                                  $ padBy (cycle lenPrm) (n - length s) sp





keymapHelp :: Keymap -> String
keymapHelp km =
    let
        wdt = 4 + maximumNote (fqn "keymapInfo")
                              ( map (length . showEv. fst)
                              $ catMaybes $ prettyKeymap km
                              )
    in
        "Dumping keymap (Meta = Alt on most systems):\n\n"
            ++ renderText 80
               ( map (uncurry (++))
               $ concatMap (\case
                    Nothing     -> [("",""), ("","")]
                    Just (e, s) ->
                        zip ( padRight wdt ' ' (showEv e)
                            : repeat (replicate wdt ' ')
                            ) $ chunkWords (80 - wdt) s
                           )
               $ prettyKeymap km
               )

    where
        showEv :: Event -> String
        showEv (EvKey           k ml) = showMods ml ++ showKey k
        showEv (EvMouseDown c r b ml) = showMods ml ++ showBtn b ++ " @ "
                                                                 ++ show (r, c)
        showEv _                      = "<unknown event>"

        showMods :: [Modifier] -> String
        showMods ml | MCtrl  `elem` ml = "Ctrl-"  ++ showMods (delete MCtrl  ml)
                    | MMeta  `elem` ml = "Meta-"  ++ showMods (delete MMeta  ml)
                    | MShift `elem` ml = "Shift-" ++ showMods (delete MShift ml)
                    | otherwise        = ""

        showKey :: Key -> String
        showKey (KChar '@' ) = "Space"
        showKey (KChar '\t') = "Tab"
        showKey (KChar  c  ) = [toUpper c]
        showKey  KBS         = "Backspace"
        showKey (KFun   n  ) = 'F' : show n
        showKey  k           = drop 1 $ show k

        showBtn :: Button -> String
        showBtn BLeft   = "LMB"
        showBtn BMiddle = "MMB"
        showBtn BRight  = "RMB"





--                                                                        margin (87) |
confHelp :: String
confHelp = renderText 80
    [ "Persistent Configuration"
    , ""
    , "There are four distinct locations from which you can influence the behaviour of"
    , "wsedit:"
    , ""
    , "  *  The files \"~/.config/wsedit/*.wsconf\", intended primarily for language"
    , "     definitions (see \"wsedit -h\" --> Formatting)"
    , ""
    , "  *  The file \"~/.config/wsedit.wsconf\" (shortcut: \"wsedit -cg\"), intended for"
    , "     global settings"
    , ""
    , "  *  The file \"./.local.wsconf\" (shortcut: \"wsedit -cl\"), intended for project-"
    , "     specific settings"
    , ""
    , "  *  The command line parameters"
    , ""
    , "You can place (almost) every command listed by \"wsedit -h\" in each of those"
    , "locations. They will be evaluated in the order defined above, with later"
    , "switches overriding the earlier ones, should they collide."
    , ""
    , "In every file, you can prefix lines with \"<ext>:\", so that they will only"
    , "apply to files matching *.<ext>. Within a file, extension-specific options will"
    , "always override generic ones, regardless of the order they are given in, e.g."
    , ""
    , "hs: -i4"
    , "-i8"
    , ""
    , "will evaluate to  -i4  for all .hs files, and to -i8 for every other file."
    ]



--                                                                        margin (87) |
usageHelp :: String
usageHelp = renderText 80
    [ "Usage: wsedit [-s] [<arguments>] [filename [line no. [column no.]]]"
    , ""
    , ""
    , "For information on how to set these options permanently, see \"wsedit -hc\"."
    , ""
    , ""
    , "Possible arguments (the uppercase options are on by default):"
    , ""
    , "-*- Help -*-"
    , ""
    , "  -h            Show this help."
    , "  -hc           Show config help."
    , "  -hk           Show current keybinds."
    , ""
    , ""
    , ""
    , "  -V            Displays the current version number."
    , ""
    , ""
    , ""
    , "-*- General -*-"
    , ""
    , "  -!            Enables failsafe mode. When this option is present, invalid"
    , "                switches will be ignored. Use this if the editor refuses to"
    , "                start due to invalid config parameters."
    , ""
    , ""
    , ""
    , "  -cg           Open global configuration file (~/.config/wsedit.wsconf)."
    , "  -cl           Open local configuration file (./.local.wsconf)."
    , ""
    , "                See \"wsedit -hc\"."
    , ""
    , ""
    , ""
    , "  -i<n>         Set indentation width to <n> (default = -i4)."
    , ""
    , ""
    , ""
    , "  -r            Open file in read-only mode."
    , "  -R            Open file in read-write mode."
    , ""
    , "                Pressing \"Ctrl-Meta-R\" in the editor will also toggle this."
    , ""
    , ""
    , ""
    , "  -ts           Insert the appropriate amount of spaces instead of tabs."
    , "  -tt           Insert a tab character when pressing tab."
    , ""
    , "  -T            Automagically detect the opened file's indentation pattern,"
    , "                assume spaces for new files."
    , ""
    , "                Pressing \"Ctrl-Meta-Tab\" in the editor will also toggle tab"
    , "                replacement."
    , ""
    , ""
    , ""
    , "-*- Formatting -*-"
    , ""
    , "  -fe+<c>       Set <c> as an escape character for strings."
    , "  -fe-          Unset the existing escape character."
    , ""
    , "  -ff<str>      (Command line parameter only) Format this file as if it had"
    , "                extension <str>."
    , ""
    , "  -fh+<s>       Search for <s> and highlight every occurence in bright red."
    , "  -fh-<s>       Remove <s> from the search terms list."
    , ""
    , "  -fk+<s>       Mark <s> as a keyword."
    , "  -fk-<s>       Remove <s> from the keywords list.\n"
    , ""
    , "                See also \"wsedit -hk\" --> \"Ctrl-F\"."
    , ""
    , "  -flc+<s>      Mark everything from <s> to the end of the line as a comment."
    , "  -flc-<s>      Remove <s> from the line comment delimiters list."
    , ""
    , "  -fs+<c1><c2>  Mark everything form char <c1> to char <c2> as a string."
    , "  -fs-<c1><c2>  Remove <c1>, <c2> from the string delimiters list."
    , ""
    , ""
    , ""
    , "                Pre-built language definitions are available in the \"lang/\""
    , "                subdirectory of the source repository. If your distribution"
    , "                does not ship them with wsedit, grab them from"
    , ""
    , "                    " ++ upstream
    , ""
    , "                and put them into \"~/.config/wsedit/\". See \"wsedit -hc\" for more"
    , "                information on persistent configuration."
    , ""
    , ""
    , ""
    , "-*- Dictionary Building -*-"
    , ""
    , "  -d+<n><f>     Add all files ending with <f> to the dictionary at indentation"
    , "                depth <n>, use * for <n> to disable the indentation filter."
    , ""
    , "  -d~<n>        Add the opened file at indentation depth <n> to the dictionary,"
    , "                use * for <n> to disable the indentation filter."
    , ""
    , "  -D            Disable dictionary building."
    , ""
    , "                With dictionary building enabled, wsedit will scan all files"
    , "                and directories under the current working directory. Every"
    , "                matching file will be read, and a dictionary will be built from"
    , "                all words from lines at depth n (either n tabs or n*tabWidth"
    , "                spaces). This dictionary will then be used to feed the"
    , "                autocomplete function. The scan will take place everytime you"
    , "                save or load."
    , "                SETTING THIS IN THE GLOBAL CONFIG WILL MAKE YOUR EDITOR TAKE"
    , "                AGES TO START UP, E.G. WHEN RUNNING FROM THE HOME DIRECTORY!"
    , ""
    , ""
    , ""
    , "-*- Renderer Configuration -*-"
    , ""
    , "  -b            Draw a single dot terminating each line instead of the dotted"
    , "                background. May speed up the editor on older systems, as it"
    , "                seems to be quite the resource hog."
    , ""
    , "  -B            Draw the usual background dots."
    , ""
    , ""
    , ""
    , "  -x            Assume a bright terminal background color."
    , "  -X            Assume a dark terminal background color."
    , ""
    , "                Make sure that every foreground color is clearly legible on"
    , "                your background and distinct from each other (many popular"
    , "                terminal color themes, e.g. Solarized, violate this), and that"
    , "                black / white is similar but different to your background color."
    , "                If the latter is impossible in your environment, use -b to"
    , "                disable background rendering and save some performance."
    , ""
    , ""
    , ""
    , "-*- File Format Options -*-"
    , ""
    , "  -e<str>       Use <str> as file output encoding."
    , "  -E            Use the locale's encoding setting."
    , ""
    , ""
    , ""
    , "  -lu           Use UNIX line endings (LF) for output."
    , "  -lw           Use Windows line endings (CR + LF) for output."
    , "  -L            Use the system's native line endings."
    , ""
    , ""
    , ""
    , "-*- Development and Debugging Options -*-"
    , ""
    , "  -s            (Command line parameter only) Resume state from crash file"
    , "                instead of opening it. If present, this must be the first"
    , "                argument passed. There are currently a few limiting factors to"
    , "                exactly resuming where a crash occured. The following properties"
    , "                cannot be restored:"
    , ""
    , "                  * The keymap"
    , "                  * The shading of the active line"
    , ""
    , "                They will be replaced with the local defaults, which should be"
    , "                fine for unmodified builds."
    , ""
    , ""
    , ""
    , "  -y            Debug: enable event dumping."
    , "  -Y            Debug: disable event dumping."
    , ""
    , "                Event dumping will show every event in the status bar."
    , ""
    , ""
    , ""
    , "-*- Other Options -*-"
    , ""
    , "  -p            Purge the clipboard file everytime the editor is closed."
    , "  -P            Do not purge the clipboard file."
    , ""
    , "                wsedit normally uses external facilities to provide copy/paste"
    , "                functionality, but defaults to ~/.wsedit-clipboard if those are"
    , "                unavailable. When left alone, this file may sit around"
    , "                indefinitely, but you can tell wsedit to purge it everytime it"
    , "                exits if you are concerned about your privacy."
    ]



--                                                                        margin (87) |
versionHelp :: String
versionHelp = renderText 80
    [ "Wyvernscale Source Code Editor (wsedit) Version " ++ version
    , ""
    , "Licensed under the Wyvernscale Source Code License Version " ++ licenseVersion
    , ""
    , "Upsteam URL: " ++ upstream
    , ""
    , "The licensed software is to be regarded as an awful, insecure, barely-working"
    , "hack job.  It should only be used in a secured environment that prevents the"
    , "software from causing any damage, including, but not limited to damage from"
    , "unexpected side effects or refusal to run at all.  Any potential damage caused"
    , "by the software is to blame on failure to implement sufficient safety measures"
    , "and therefore on the user, not on the developer of the software."
    ]
