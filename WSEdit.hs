{-# LANGUAGE LambdaCase #-}

module WSEdit where


import Control.Monad            (when)
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.RWS.Strict (ask, get, modify, runRWST)
import Data.Default             (def)
import Data.List                (isInfixOf, isPrefixOf, partition, stripPrefix)
import Data.Maybe               (fromMaybe)
import Graphics.Vty             ( Event (EvKey)
                                , Key (KChar)
                                , mkVty
                                , nextEvent
                                , shutdown
                                )
import Safe                     (atDef, headDef, headMay, readDef)
import System.Directory         ( doesDirectoryExist, getHomeDirectory
                                , listDirectory
                                )
import System.Environment       (getArgs)
import System.Exit              ( ExitCode (ExitFailure, ExitSuccess)
                                , exitFailure, exitWith
                                )

import WSEdit.Control           ( bail, deleteSelection, insert
                                , listAutocomplete, load, quitComplain, save
                                )
import WSEdit.Data              ( EdConfig ( drawBg, dumpEvents, edDesign
                                           , escape, keymap, keywords
                                           , lineComment, purgeOnClose
                                           , searchTerms, strDelim, vtyObj
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
                                , setStatus, version
                                )
import WSEdit.Data.Pretty       (prettyKeymap, unPrettyEdConfig)
import WSEdit.Keymaps           (defaultKM)
import WSEdit.Output            (draw, drawExitFrame)
import WSEdit.Util              ( getExt, mayReadFile, padRight, withFst
                                , withSnd
                                )



-- | License version number constant.
licenseVersion :: String
licenseVersion = "1.1"




-- | Splits up commandline arguments, global, and local config contents into
--   @(Maybe target file, target line no, target col no, list of switches,
--   whether -s is present)@.
splitArgs :: [String] -> [String] -> [String]
          -> (Maybe FilePath, Int, Int, [String], Bool)
splitArgs args glob loc =
    let
        (sw, ar) = partition (isPrefixOf "-") args
        filename = headMay ar
        tLnNo    = readDef 1 $ atDef "1" ar 1
        tColNo   = readDef 1 $ atDef "1" ar 2

        fext     = if any (isInfixOf "-c") sw
                      then Just "wsconf"
                      else getExt <$> filename

        swChain  = filterFileArgs fext glob
                ++ filterFileArgs fext loc
                ++ sw

        dashS    = headDef "" sw == "-s"
    in
        (filename, tLnNo, tColNo, swChain, dashS)





-- | Main function. Reads in the parameters, then passes control to the main
--   loop.
start :: IO ()
start = do
    -- partition the parameters into switches and arguments
    args <- getArgs

    -- Read the global and local config files. Use an empty string in case of
    -- nonexistence.
    h <- liftIO $ getHomeDirectory

    b <- doesDirectoryExist $ h ++ "/.config/wsedit"

    mods <- if b
               then listDirectory (h ++ "/.config/wsedit")
                >>= mapM ( fmap (lines . fromMaybe "")
                         . mayReadFile
                         . ((h ++ "/.config/wsedit/") ++ )
                         )
                >>= return . concat
               else return [""]


    glob <- fromMaybe "" <$> mayReadFile (h ++ "/.config/wsedit.wsconf")
    loc  <- fromMaybe "" <$> mayReadFile "./.local.wsconf"

    -- split the parameters into a more comfortable format
    let (filename, tLnNo, tColNo, sw, dashS)
            = splitArgs args (mods ++ lines glob) $ lines loc

    -- initialize vty
    v <- mkVty def

    -- create the configuration object
    let conf = mkDefConfig v defaultKM

    -- If the "-s" switch is set: read starting conf and state from the file
    -- passed, otherwise use defaults.
    (conf', st) <- if dashS
                     then case filename of
                               Nothing -> do
                                            shutdown v
                                            putStrLn "-s: file expected."
                                            exitFailure

                               Just  f -> do
                                    r <- readFile f

                                    let (cLines, sLines) = withSnd (drop 2)
                                                         $ span (/= "")
                                                         $ drop 3
                                                         $ lines r

                                        conf' = unPrettyEdConfig
                                                (vtyObj conf)
                                                (keymap conf)
                                                (dCurrLnMod $ edDesign conf)
                                             $ read
                                             $ unlines cLines

                                        st   = read
                                             $ unlines sLines

                                    return (conf', st)

                     else return (conf, def { fname   = fromMaybe "" filename
                                            , loadPos = (tLnNo, tColNo)
                                            }
                                 )

    _ <- case argLoop h sw (conf', st) of
              Right (c, s)    -> runRWST (exec $ not dashS) c s
              Left  (ex, msg) -> do
                                    shutdown v
                                    putStrLn msg
                                    exitWith ex

    -- Shutdown vty
    shutdown v

    where
        -- | Possibly loads the file specified in `fname`, then runs the editor.
        exec :: Bool -> WSEdit ()
        exec b = do
            when b $ catchEditor load $ \e ->
                quitComplain $ "An I/O error occured:\n\n"
                            ++ show e
                            ++ "\n\nAre you trying to open a binary file?"

            mainLoop
            drawExitFrame



-- | Takes maybe the file extension and the raw string read from a
--   config location and returns a list of switches, throwing out
--   comment lines as well as those specific to other extensions.
filterFileArgs :: Maybe String -> [String] -> [String]
filterFileArgs Nothing    s = concatMap words
                            $ filter (\x -> ':' `notElem` takeWhile (/= '-') x
                                         && not (isPrefixOf "#" x)
                                     ) s

filterFileArgs (Just ext) s =
    let
        (loc, gl) = partition (\x -> ':' `elem` takeWhile (/= '-') x)
                  $ filter (not . isPrefixOf "#") s
    in
        concatMap words
      $ gl
     ++ ( filter (/= "")
        $ map (fromMaybe "" . stripPrefix (ext ++ ":"))
          loc
        )





-- | Parse all switches passed to it.  The first parameter takes the user's home
--   directory.
argLoop :: String -> [String] -> (EdConfig, EdState) -> Either (ExitCode, String) (EdConfig, EdState)
argLoop _ (('-':'h':'k'        :_ ):_ ) (c, _) = keymapInfo c
argLoop _ (('-':'h'            :_ ):_ ) _      = usage
argLoop _ (('-':'V'            :_ ):_ ) _      = versionInfo
argLoop h (('-':'s'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s) -- gets handled elsewhere, ignore it here.
argLoop h (('-':'b'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c { drawBg       = False                           }, s)
argLoop h (('-':'B'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c { drawBg       = True                            }, s)
argLoop h (('-':'f':'e':'+': e :[]):xs) (c, s) = argLoop h          xs  (c { escape       = Just e                          }, s)
argLoop h (('-':'f':'e':'-'    :[]):xs) (c, s) = argLoop h          xs  (c { escape       = Nothing                         }, s)
argLoop h (('-':'f':'h':'+'    :x ):xs) (c, s) = argLoop h          xs  (c { searchTerms  = x : searchTerms c               }, s)
argLoop h (('-':'f':'h':'-'    :x ):xs) (c, s) = argLoop h          xs  (c { searchTerms  = filter (/= x) $ searchTerms c   }, s)
argLoop h (('-':'f':'k':'+'    :x ):xs) (c, s) = argLoop h          xs  (c { keywords     = x : keywords c                  }, s)
argLoop h (('-':'f':'k':'-'    :x ):xs) (c, s) = argLoop h          xs  (c { keywords     = filter (/= x) $ keywords c      }, s)
argLoop h (('-':'f':'l':'c':'+':x ):xs) (c, s) = argLoop h          xs  (c { lineComment  = x : lineComment c               }, s)
argLoop h (('-':'f':'l':'c':'-':x ):xs) (c, s) = argLoop h          xs  (c { lineComment  = filter (/= x) $ lineComment c   }, s)
argLoop h (('-':'f':'s':'+':a:b:[]):xs) (c, s) = argLoop h          xs  (c { strDelim     = (a, b) : strDelim c             }, s)
argLoop h (('-':'f':'s':'-':a:b:[]):xs) (c, s) = argLoop h          xs  (c { strDelim     = filter (/= (a, b)) $ strDelim c }, s)
argLoop h (('-':'i'            :n ):xs) (c, s) = argLoop h          xs  (c { tabWidth     = read n                          }, s)
argLoop h (('-':'p'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c { purgeOnClose = True                            }, s)
argLoop h (('-':'P'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c { purgeOnClose = False                           }, s)
argLoop h (('-':'x'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c { edDesign     = brightTheme                     }, s)
argLoop h (('-':'X'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c { edDesign     = def                             }, s)
argLoop h (('-':'y'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c { dumpEvents   = True                            }, s)
argLoop h (('-':'Y'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c { dumpEvents   = False                           }, s)
argLoop h (('-':'c':'g'        :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s { fname       = h ++ "/.config/wsedit.wsconf" })
argLoop h (('-':'c':'l'        :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s { fname       = "./.local.wsconf"             })
argLoop h (('-':'d': d         :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s { buildDict   = Just $ read [d]               })
argLoop h (('-':'D'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s { buildDict   = Nothing                       })
argLoop h (('-':'r'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s { readOnly    = True                          })
argLoop h (('-':'R'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s { readOnly    = False                         })
argLoop h (('-':'t':'s'        :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s { replaceTabs = True
                                                                              , detectTabs  = False                         })
argLoop h (('-':'t':'t'        :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s { replaceTabs = False
                                                                              , detectTabs  = False                         })
argLoop h (('-':'T'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s { detectTabs  = True                          })

argLoop h (['-']                   :xs) (c, s) = argLoop h          xs  (c, s)
argLoop _ []                            (c, s) = if fname s == ""
                                                    then Left  (ExitFailure 1, "No file specified (try wsedit -h).")
                                                    else Right (c, s)

argLoop _ (('-'                :x ):_ ) _      = Left (ExitFailure 1, "Unknown argument: -" ++ x ++ " (try wsedit -h)")

argLoop _ (x                       :_ ) _      = Left (ExitFailure 1, "Unexpected parameter: " ++ x ++ " (try wsedit -h)")





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





-- | Prints out version and licensing information, then exits with code 0.
versionInfo :: Either (ExitCode, String) (EdConfig, EdState)
versionInfo = Left (ExitSuccess, "Wyvernscale Source Code Editor (wsedit) Version "
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
                   )



-- | Dumps the keymap, then exits with code 0.
keymapInfo :: EdConfig -> Either (ExitCode, String) (EdConfig, EdState)
keymapInfo c =
    let
        tbl  = map (withFst show)
             $ prettyKeymap
             $ keymap c

        maxW = maximum $ map (length . fst) tbl
    in
        Left (ExitSuccess
             , "Dumping keymap:\n"
                ++ ( unlines
                   $ map (\(e, s) -> (padRight maxW ' ' e ++ "\t" ++ s))
                     tbl
                   )
             )



-- | Prints the usage help, then exits with code 0.
usage :: Either (ExitCode, String) (EdConfig, EdState)
usage = Left
        (ExitSuccess
        , "Usage: wsedit [-s] [<arguments>] [filename [line no. [column no.]]]\n"
       ++ "\n"
       ++ "Arguments (the uppercase options are on by default):\n"
       ++ "\n"
       ++ "\t-b\tDon't draw the background (dots + lines). May speed up the\n"
       ++ "\t\teditor on older systems, as it seems to be quite the resource hog.\n"
       ++ "\t-B\tDraw the usual background (dots + lines).\n"
       ++ "\n"
       ++ "\n"
       ++ "\n"
       ++ "\t-cg\tOpen global configuration file (~/.config/wsedit.wsconf).\n"
       ++ "\t-cl\tOpen local configuration file (./.local.wsconf).\n"
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
       ++ "\t-d<n>\tEnable dictionary building at indentation depth <n>.\n"
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
       ++ "\t-fe+<c>\tSet <c> as an escape character for strings.\n"
       ++ "\t-fe-\tUnset the existing escape character.\n"
       ++ "\n"
       ++ "\t-fh+<s>\tSearch for <s> and highlight every occurence in bright red.\n"
       ++ "\t-fh-<s>\tRemove <s> from the search terms list.\n"
       ++ "\n"
       ++ "\t-fk+<s>\tMark <s> as a keyword.\n"
       ++ "\t-fk-<s>\tRemove <s> from the keywords list.\n"
       ++ "\n"
       ++ "\t-flc+<s>\tMark everything from <s> to the end of the line as a comment.\n"
       ++ "\t-flc-<s>\tRemove <s> from the line comment delimiters list.\n"
       ++ "\n"
       ++ "\t-fs+<c1><c2>\tMark everything form char <c1> to char <c2> as a string.\n"
       ++ "\t-fs-<c1><c2>\tRemove <c1>, <c2> from the string delimiters list.\n"
       ++ "\n"
       ++ "\n"
       ++ "\n"
       ++ "\t-h\tShow this help.\n"
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
       ++ "\t-s\tResume state from crash file instead of opening it.  If present,\n"
       ++ "\t\tthis must be the first argument passed.  There are currently a\n"
       ++ "\t\tfew limiting factors to exactly resuming where a crash occured.\n"
       ++ "\t\tThe following properties cannot be restored:\n"
       ++ "\n"
       ++ "\t\t\t*The keymap\n"
       ++ "\t\t\t*The shading of the active line\n"
       ++ "\n"
       ++ "\t\tThey will be replaced with the local defaults, which should be *fine*.\n"
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
        )
