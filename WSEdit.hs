{-# LANGUAGE LambdaCase #-}

module WSEdit where


import Control.Monad            (when)
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.RWS.Strict (ask, get, modify, runRWST)
import Data.Default             (def)
import Data.List                ( delete, isInfixOf, isPrefixOf, isSuffixOf
                                , partition, stripPrefix
                                )
import Data.Maybe               (catMaybes, fromMaybe)
import Graphics.Vty             ( Button (BLeft, BMiddle, BRight)
                                , Event (EvKey, EvMouseDown, EvResize)
                                , Key (KBS, KChar, KFun)
                                , Modifier (MCtrl, MMeta, MShift)
                                , mkVty
                                , nextEvent
                                , shutdown
                                )
import Safe                     ( atDef, headDef, headMay, lastNote, maximumNote
                                , readDef, readNote
                                )
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
                                           , strDelim, vtyObj, tabWidth
                                           )
                                , EdDesign (dCurrLnMod)
                                , EdState ( buildDict, changed, continue
                                          , detectTabs, fname, lastEvent
                                          , loadPos, readOnly, replaceTabs
                                          , searchTerms, status
                                          )
                                , WSEdit
                                , brightTheme, catchEditor, mkDefConfig
                                , setStatus
                                )
import WSEdit.Data.Pretty       (prettyKeymap, unPrettyEdConfig)
import WSEdit.Help              (confHelp, usageHelp, versionHelp)
import WSEdit.Keymaps           (defaultKM)
import WSEdit.Output            (draw, drawExitFrame)
import WSEdit.Util              ( getExt, mayReadFile, padRight, withFst
                                , withSnd
                                )



fqn :: String -> String
fqn = ("WSEdit." ++)





-- | Splits up commandline arguments, global, and local config contents into
--   @(Maybe target file, file extension, target line no, target col no, list of
--   switches, whether -s is present)@.
splitArgs :: [String] -> [String] -> [String]
          -> (Maybe FilePath, Int, Int, [String], Bool)
splitArgs args glob loc =
    let
        (sw, ar) = partition (isPrefixOf "-") args
        filename = headMay ar
        tLnNo    = readDef 1 $ atDef "1" ar 1
        tColNo   = readDef 1 $ atDef "1" ar 2

        fext     = case filter (isPrefixOf "-ff") sw of
                    [] | any (isInfixOf "-c") sw -> Just "wsconf"
                    [] | otherwise               -> getExt <$> filename
                    l                            -> Just
                                                  $ drop 3
                                                  $ lastNote (fqn "splitArgs") l

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
                  . filter (isSuffixOf ".wsconf")
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
                                                         $ drop 4
                                                         $ lines r

                                        conf' = unPrettyEdConfig
                                                (vtyObj conf)
                                                (keymap conf)
                                                (dCurrLnMod $ edDesign conf)
                                              $ readNote (fqn "start:1")
                                              $ unlines cLines

                                        st    = readNote (fqn "start:2")
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
filterFileArgs Nothing    s
    = concatMap words
    $ filter (\x -> ':' `notElem` takeWhile (/= ' ') x
                 && not ("#" `isPrefixOf` x)
             ) s

filterFileArgs (Just ext) s =
    let
        (loc, gl) = partition (\x -> ':' `elem` takeWhile (/= ' ') x)
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
argLoop _ (('-':'h':'c'        :_ ):_ ) (_, _) = Left (ExitSuccess, confHelp   )
argLoop _ (('-':'h'            :_ ):_ ) _      = Left (ExitSuccess, usageHelp  )
argLoop _ (('-':'V'            :_ ):_ ) _      = Left (ExitSuccess, versionHelp)
argLoop h (('-':'s'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s)  -- gets handled elsewhere, ignore it here.
argLoop h (('-':'f':'f'        :_ ):xs) (c, s) = argLoop h          xs  (c, s)  -- same
argLoop h (('-':'b'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c { drawBg       = False                           }, s)
argLoop h (('-':'B'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c { drawBg       = True                            }, s)
argLoop h (('-':'f':'e':'+': e :[]):xs) (c, s) = argLoop h          xs  (c { escape       = Just e                          }, s)
argLoop h (('-':'f':'e':'-'    :[]):xs) (c, s) = argLoop h          xs  (c { escape       = Nothing                         }, s)
argLoop h (('-':'f':'k':'+'    :x ):xs) (c, s) = argLoop h          xs  (c { keywords     = x : keywords c                  }, s)
argLoop h (('-':'f':'k':'-'    :x ):xs) (c, s) = argLoop h          xs  (c { keywords     = filter (/= x) $ keywords c      }, s)
argLoop h (('-':'f':'l':'c':'+':x ):xs) (c, s) = argLoop h          xs  (c { lineComment  = x : lineComment c               }, s)
argLoop h (('-':'f':'l':'c':'-':x ):xs) (c, s) = argLoop h          xs  (c { lineComment  = filter (/= x) $ lineComment c   }, s)
argLoop h (('-':'f':'s':'+':a:b:[]):xs) (c, s) = argLoop h          xs  (c { strDelim     = (a, b) : strDelim c             }, s)
argLoop h (('-':'f':'s':'-':a:b:[]):xs) (c, s) = argLoop h          xs  (c { strDelim     = filter (/= (a, b)) $ strDelim c }, s)
argLoop h (('-':'i'            :n ):xs) (c, s) = argLoop h          xs  (c { tabWidth     = readNote (fqn "argLoop") n      }, s)
argLoop h (('-':'p'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c { purgeOnClose = True                            }, s)
argLoop h (('-':'P'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c { purgeOnClose = False                           }, s)
argLoop h (('-':'x'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c { edDesign     = brightTheme                     }, s)
argLoop h (('-':'X'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c { edDesign     = def                             }, s)
argLoop h (('-':'y'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c { dumpEvents   = True                            }, s)
argLoop h (('-':'Y'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c { dumpEvents   = False                           }, s)
argLoop h (('-':'c':'g'        :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s { fname       = h ++ "/.config/wsedit.wsconf"                                })
argLoop h (('-':'c':'l'        :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s { fname       = "./.local.wsconf"                                            })
argLoop h (('-':'d':'+':'*'    :x ):xs) (c, s) = argLoop h          xs  (c, s { buildDict   = (Just x , Nothing                     ) : buildDict s        })
argLoop h (('-':'d':'+': d     :x ):xs) (c, s) = argLoop h          xs  (c, s { buildDict   = (Just x , Just $ readNote (fqn "argLoop") [d]) : buildDict s })
argLoop h (('-':'d':'~':'*'    :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s { buildDict   = (Nothing, Nothing                     ) : buildDict s        })
argLoop h (('-':'d':'~': d     :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s { buildDict   = (Nothing, Just $ readNote (fqn "argLoop") [d]) : buildDict s })
argLoop _ (('-':'d'            :_ ):_ ) _      = Left (ExitFailure 1, "Error: -d is deprecated, use -d+ instead.")
argLoop h (('-':'D'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s { buildDict   = []                                                           })
argLoop h (('-':'f':'h':'+'    :x ):xs) (c, s) = argLoop h          xs  (c, s { searchTerms = x : searchTerms s                                            })
argLoop h (('-':'f':'h':'-'    :x ):xs) (c, s) = argLoop h          xs  (c, s { searchTerms = filter (/= x) $ searchTerms s                                })
argLoop h (('-':'r'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s { readOnly    = True                                                         })
argLoop h (('-':'R'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s { readOnly    = False                                                        })
argLoop h (('-':'t':'s'        :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s { replaceTabs = True
                                                                                , detectTabs  = False                                                      })
argLoop h (('-':'t':'t'        :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s { replaceTabs = False
                                                                              , detectTabs  = False                                                        })
argLoop h (('-':'T'            :x ):xs) (c, s) = argLoop h (('-':x):xs) (c, s { detectTabs  = True                                                         })

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

                    EvResize _ _       -> return ()
                    _                  -> setStatus $ "Event not bound: "
                                                   ++ show ev
              )
        $ fmap fst
        $ lookup ev
        $ catMaybes
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



-- | Dumps the keymap, then exits with code 0.
keymapInfo :: EdConfig -> Either (ExitCode, String) (EdConfig, EdState)
keymapInfo conf =
    let
        tbl  = map (\case
                        Nothing -> ("", "")
                        Just  x -> withFst showEv x
                   )
             $ prettyKeymap
             $ keymap conf

        maxW = maximumNote (fqn "keymapInfo") $ map (length . fst) tbl
    in
        Left (ExitSuccess
             , "Dumping keymap (Meta = Alt on most systems):\n\n"
                ++ ( unlines
                   $ map (\(e, s) -> (padRight maxW ' ' e ++ "\t" ++ s))
                     tbl
                   )
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
        showKey (KChar  c  ) = [c]
        showKey  KBS         = "Backspace"
        showKey (KFun   n  ) = 'F' : show n
        showKey  k           = drop 1 $ show k

        showBtn :: Button -> String
        showBtn BLeft   = "LMB"
        showBtn BMiddle = "MMB"
        showBtn BRight  = "RMB"
