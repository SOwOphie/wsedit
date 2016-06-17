{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad            (when)
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.RWS.Strict (ask, get, local, modify, put, runRWST)
import Data.Default             (def)
import Data.List                (isPrefixOf, partition, stripPrefix)
import Data.Maybe               (fromMaybe)
import Graphics.Vty             ( Event (EvKey)
                                , Key (KChar)
                                , mkVty
                                , nextEvent
                                , shutdown
                                )
import Safe                     (atDef, headMay, readDef)
import System.Directory         (getHomeDirectory)
import System.Environment       (getArgs)

import WSEdit.Control           ( bail, deleteSelection, insert
                                , listAutocomplete, load, quitComplain, save
                                )
import WSEdit.Data              ( EdConfig ( drawBg, dumpEvents, edDesign
                                           , keymap, purgeOnClose, vtyObj
                                           , tabWidth
                                           )
                                , EdDesign (dCurrLnMod)
                                , EdState ( buildDict, changed, continue
                                          , detectTabs, fname, lastEvent
                                          , loadPos, readOnly, replaceTabs
                                          , status
                                          )
                                , WSEdit
                                , brightTheme, catchEditor, mkDefConfig
                                , setStatus
                                )
import WSEdit.Data.Pretty       (prettyKeymap, unPrettyEdConfig)
import WSEdit.Keymaps           (defaultKM)
import WSEdit.Output            (draw, drawExitFrame)
import WSEdit.Util              ( getExt, mayReadFile, padRight, withFst
                                , withSnd
                                )



-- | Version number constant.
version :: String
version = "0.2.0.2"

-- | License version number constant.
licenseVersion :: String
licenseVersion = "1.1"



-- | Main editor loop. Runs once per input event processed.
mainLoop :: WSEdit ()
mainLoop = do
    draw

    setStatus ""

    c <- ask
    ev <- liftIO $ nextEvent $ vtyObj c

    modify (\s -> s { lastEvent = Just ev })

    -- look up the event in the keymap
    -- if not found: insert the pressed key
    -- if it's not alphanumeric: show an "event not bound" warning
    catchEditor
        ( fromMaybe (case ev of
                    EvKey (KChar k) [] -> deleteSelection
                                       >> insert k
                                       >> listAutocomplete

                    _                  -> setStatus $ "Event not bound: "
                                                   ++ show ev
              )
        $ fmap fst
        $ lookup ev
        $ keymap c
        )  $ \e -> do
            b <- changed <$> get
            if b
               then do
                    modify (\s -> s { fname = "CRASH-RESCUE" })
                    save
                    bail $ "An error occured: " ++ show e
                        ++ "\n\n"
                        ++ "Your unsaved work has been rescued to"
                        ++ " ./CRASH-RESCUE ."

               else bail $ "An error occured: " ++ show e


    when (dumpEvents c) $ do
        s <- get
        setStatus $ show ev ++ status s

    b <- continue <$> get
    when b mainLoop





-- | Main function. Reads in the parameters, then passes control to the main
--   loop.
main :: IO ()
main = do
    -- partition the parameters into switches and arguments
    (sw, args) <- partition (isPrefixOf "-") <$> getArgs

    -- initialize vty
    v <- mkVty def

    -- create the configuration object
    let filename = headMay args
        tLnNo    = readDef 1 $ atDef "1" args 1
        tColNo   = readDef 1 $ atDef "1" args 2
        conf     = mkDefConfig v defaultKM

    -- Read the global and local config files. Use an empty string in case of
    -- nonexistence.
    h <- liftIO $ getHomeDirectory
    glob <- fromMaybe "" <$> mayReadFile (h ++ "/.config/wsedit.conf")
    loc  <- fromMaybe "" <$> mayReadFile "./.wsedit"

    -- Assemble the switches from all possible config locations
    let swChain = filterFileArgs (getExt <$> filename) glob
               ++ filterFileArgs (getExt <$> filename) loc
               ++ sw

    -- Run the argument loop with the default config and state objects.
    _ <- runRWST (argLoop swChain) conf
       $ def { fname   = fromMaybe "" filename
             , loadPos = (tLnNo, tColNo)
             }

    -- Shutdown vty
    shutdown v

    where
        -- | Takes maybe the file extension and the raw string read from a
        --   config location and returns a list of switches, throwing out
        --   comment lines as well as those specific to other extensions.
        filterFileArgs :: Maybe String -> String -> [String]
        filterFileArgs Nothing    s = concatMap words
                                    $ filter (\x -> notElem ':' x
                                                 && not (isPrefixOf "#" x)
                                             )
                                    $ lines s

        filterFileArgs (Just ext) s =
            let
                (loc, gl) = partition (elem ':')
                          $ filter (not . isPrefixOf "#")
                          $ lines s
            in
                concatMap words
              $ gl
             ++ ( filter (/= "")
                $ map (fromMaybe "" . stripPrefix (ext ++ ":"))
                  loc
                )



-- | Parse all switches passed to it, then starts the main loop.
argLoop :: [String] -> WSEdit ()
argLoop (('-':'V'    :_ ):_ ) =
    versionInfo

argLoop (('-':'h':'k':_ ):_ ) =
    keymapInfo


argLoop (('-':'b'    :x ):xs) = do
    local (\c -> c { drawBg = False })
        $ argLoop (('-':x):xs)

argLoop (('-':'B'    :x ):xs) = do
    local (\c -> c { drawBg = True })
        $ argLoop (('-':x):xs)

argLoop (('-':'p'    :x ):xs) = do
    local (\c -> c { purgeOnClose = True })
        $ argLoop (('-':x):xs)

argLoop (('-':'P'    :x ):xs) = do
    local (\c -> c { purgeOnClose = False })
        $ argLoop (('-':x):xs)

argLoop (('-':'x'    :x ):xs) = do
    local (\c -> c { edDesign = brightTheme })
        $ argLoop (('-':x):xs)

argLoop (('-':'X'    :x ):xs) = do
    local (\c -> c { edDesign = def })
        $ argLoop (('-':x):xs)

argLoop (('-':'y'    :x ):xs) = do
    local (\c -> c { dumpEvents = True })
        $ argLoop (('-':x):xs)

argLoop (('-':'Y'    :x ):xs) = do
    local (\c -> c { dumpEvents = False })
        $ argLoop (('-':x):xs)


argLoop (('-':'c':'g':x ):xs) = do
    h <- liftIO getHomeDirectory
    modify (\s -> s { fname = h ++ "/.config/wsedit.conf" })
    argLoop (('-':x):xs)

argLoop (('-':'c':'l':x ):xs) = do
    modify (\s -> s { fname = "./.wsedit" })
    argLoop (('-':x):xs)

argLoop (('-':'d': c :x ):xs) = do
    modify (\s -> s { buildDict = Just $ read [c] })
    argLoop (('-':x):xs)

argLoop (('-':'D'    :x ):xs) = do
    modify (\s -> s { buildDict = Nothing })
    argLoop (('-':x):xs)

argLoop (('-':'r'    :x ):xs) = do
    modify (\s -> s { readOnly = True })
    argLoop (('-':x):xs)

argLoop (('-':'i': n    ):xs) = do
    local (\c -> c { tabWidth = read n })
        $ argLoop xs

argLoop (('-':'R'    :x ):xs) = do
    modify (\s -> s { readOnly = False })
    argLoop (('-':x):xs)

argLoop (('-':'t':'s':x ):xs) = do
    modify (\s -> s { replaceTabs = True
                    , detectTabs  = False
                    }
           )
    argLoop (('-':x):xs)

argLoop (('-':'t':'t':x ):xs) = do
    modify (\s -> s { replaceTabs = False
                    , detectTabs  = False
                    }
           )
    argLoop (('-':x):xs)

argLoop (('-':'T'    :x ):xs) = do
    modify (\s -> s { detectTabs  = True })
    argLoop (('-':x):xs)


argLoop (('-':'s'    :_ ):_ ) = do
    c <- ask
    s <- get

    r <- liftIO $ readFile $ fname s

    let (cLines, sLines) = withSnd (drop 2)
                         $ span (/= "")
                         $ drop 3
                         $ lines r

        conf = unPrettyEdConfig (vtyObj c) (keymap c) (dCurrLnMod $ edDesign c)
             $ read
             $ unlines cLines

        st   = read
             $ unlines sLines

    put st
    local (const conf) $ mainLoop >> drawExitFrame

argLoop []                      = do
    f <- fname <$> get
    if f == ""
       then usage "No file specified."
       else do
            catchEditor load $ \e ->
                quitComplain $ "An I/O error occured:\n\n"
                            ++ show e
                            ++ "\n\nAre you trying to open a binary file?"

            mainLoop
            drawExitFrame


argLoop (['-']           :xs) =
    argLoop xs

argLoop (('-': x     :_ ):_ ) =
    usage $ "Unknown argument: -" ++ [x]

argLoop (x               :_ ) =
    usage $ "Unexpected parameter: " ++ x



-- | Prints out version and licensing information, then exits with code 1.
versionInfo :: WSEdit ()
versionInfo = quitComplain
            $ "Wyvernscale Source Code Editor (wsedit) Version "
                ++ version ++ "\n"
           ++ "\n"
           ++ "Licensed under the Wyvernscale Source Code License Version "
                ++ licenseVersion ++ ".\n"
           ++ "\n"
           ++ "The licensed software is to be regarded as an awful, insecure, barely-working\n"
           ++ "hack job.  It should only be used in a secured environment that prevents the\n"
           ++ "software from causing any damage, including, but not limited to damage from\n"
           ++ "unexpected side effects or refusal to run at all.  Any potential damage caused\n"
           ++ "by the software is to blame on failure to implement sufficient safety measures\n"
           ++ "and therefore on the user, not on the developer of the software.\n"



-- | Dumps the keymap, then exits with code 1.
keymapInfo :: WSEdit ()
keymapInfo = do
    k <- keymap <$> ask

    let tbl  = map (withFst show) $ prettyKeymap k
        maxW = maximum $ map (length . fst) tbl
        tbl' = map (withFst (padRight maxW ' ')) tbl

    quitComplain $ "Dumping keymap:\n"
                ++ unlines ( map (\(e, s) -> e ++ "\t" ++ s)
                             tbl'
                           )



-- | Prints an error message, followed by the usage help. Shuts down vty and
--   exits with code 1.
usage :: String -> WSEdit ()
usage s = quitComplain
        $ s ++ "\n"
       ++ "\n"
       ++ "Usage: wsedit [<arguments>] [filename [line no. [column no.]]]\n"
       ++ "\n"
       ++ "Arguments (the uppercase options are on by default):\n"
       ++ "\n"
       ++ "\t-b\tDon't draw the background (dots + lines). May speed up the\n"
       ++ "\t\teditor on older systems, as it seems to be quite the resource hog.\n"
       ++ "\t-B\tDraw the usual background (dots + lines).\n"
       ++ "\n"
       ++ "\n"
       ++ "\n"
       ++ "\t-cg\tOpen global configuration file (~/.config/wsedit.conf).\n"
       ++ "\t-cl\tOpen local configuration file (./.wsedit).\n"
       ++ "\n"
       ++ "\t\tThose files will be concatenated with the command line arguments\n"
       ++ "\t\tand then evaluated. You can prefix lines with \"<ext>:\" so that\n"
       ++ "\t\tthey are only read for files with extension .<ext> , e.g.\n"
       ++ "\n"
       ++ "\t\t\ths: -i4 -t\n"
       ++ "\n"
       ++ "\t\tLines starting with a '#' will be ignored. The order of evaluation\n"
       ++ "\t\tis\n"
       ++ "\n"
       ++ "\t\t\t* Generic global options\n"
       ++ "\t\t\t* Extension-specific global options\n"
       ++ "\t\t\t* Generic local options\n"
       ++ "\t\t\t* Extension-specific local options\n"
       ++ "\t\t\t* Command line options\n"
       ++ "\n"
       ++ "\t\t, earlier options get overridden by later ones.\n"
       ++ "\n"
       ++ "\n"
       ++ "\n"
       ++ "\t-d<n>\tEnable dictionary building at indentation depth n.\n"
       ++ "\t-D\tDisable dictionary building.\n"
       ++ "\n"
       ++ "\t\tWith dictionary building enabled, wsedit will scan all files and\n"
       ++ "\t\tdirectories under the current working directory, skipping hidden\n"
       ++ "\t\tones (starting with a dot).  Every file with the same file ending\n"
       ++ "\t\tas the opened file will be read, and a dictionary will be built\n"
       ++ "\t\tfrom all words from lines at depth n (either n tabs or n*tabWidth\n"
       ++ "\t\tspaces).  This dictionary will then be used to feed the\n"
       ++ "\t\tautocomplete function.  The scan will take place everytime you\n"
       ++ "\t\tsafe or load.\n"
       ++ "\t\tSETTING THIS GLOBALLY WILL MAKE YOUR EDITOR TAKE AGES TO\n"
       ++ "\t\tSTART UP, E.G. WHEN RUNNING FROM THE HOME DIRECTORY!\n"
       ++ "\n"
       ++ "\n"
       ++ "\n"
       ++ "\t-hk\tShow current keybinds.\n"
       ++ "\n"
       ++ "\n"
       ++ "\n"
       ++ "\t-i<n>\tSet indentation width to n (default = -i 4).\n"
       ++ "\n"
       ++ "\n"
       ++ "\n"
       ++ "\t-p\tPurge the clipboard file everytime the editor is closed.\n"
       ++ "\t-P\tDo not purge the clipboard file.\n"
       ++ "\n"
       ++ "\t\twsedit normally uses xclip or xsel to provide copy/paste\n"
       ++ "\t\tfunctionality, but defaults to ~/.wsedit-clipboard if those are\n"
       ++ "\t\tunavailable.  When left alone, this file may sit around\n"
       ++ "\t\tindefinitely, but you can tell wsedit to purge it everytime it\n"
       ++ "\t\texits if you are concerned that it might compromise your privacy.\n"
       ++ "\n"
       ++ "\n"
       ++ "\n"
       ++ "\t-r\tOpen file in read-only mode.\n"
       ++ "\t-R\tOpen file in read-write mode.\n"
       ++ "\n"
       ++ "\t\tPressing Ctrl-Meta-R in the editor will also toggle this.\n"
       ++ "\n"
       ++ "\n"
       ++ "\n"
       ++ "\t-s\tResume state from crash file instead of opening it.\n"
       ++ "\t\tThere are currently a few limiting factors to exactly resuming\n"
       ++ "\t\twhere a crash occured.  The following properties cannot be\n"
       ++ "\t\trestored:\n"
       ++ "\n"
       ++ "\t\t\t*The keymap\n"
       ++ "\t\t\t*The shading of the active line\n"
       ++ "\n"
       ++ "\t\tThese properties will be replaced with local defaults.\n"
       ++ "\n"
       ++ "\n"
       ++ "\n"
       ++ "\t-ts\tInsert the appropriate amount of spaces instead of tabs.\n"
       ++ "\t-tt\tInsert a tab character when pressing tab.\n"
       ++ "\t-T\tAutomatically detect the opened file's indentation pattern.\n"
       ++ "\t\tAssume spaces for new files.\n"
       ++ "\n"
       ++ "\t\tPressing Ctrl-Meta-Tab in the editor will also toggle tab\n"
       ++ "\t\treplacement.\n"
       ++ "\n"
       ++ "\n"
       ++ "\n"
       ++ "\t-V\tDisplays the current version number.\n"
       ++ "\n"
       ++ "\n"
       ++ "\n"
       ++ "\t-x\tAssume a bright terminal background color.\n"
       ++ "\t-X\tAssume a dark terminal background color.\n"
       ++ "\n"
       ++ "\t\tMake sure that every foreground color is clearly legible on your\n"
       ++ "\t\tbackground and distinct from each other (many popular terminal\n"
       ++ "\t\tcolor themes, e.g. Solarized, violate this), and that black / white\n"
       ++ "\t\tis similar but different to your background color.  If the latter\n"
       ++ "\t\tshould prove impossible in your environment, use -b to disable\n"
       ++ "\t\tbackground rendering and save some performance.\n"
       ++ "\n"
       ++ "\n"
       ++ "\n"
       ++ "\t-y\tDebug: enable event dumping (y as in \"y u no work?!?\").\n"
       ++ "\t-Y\tDebug: disable event dumping.\n"
