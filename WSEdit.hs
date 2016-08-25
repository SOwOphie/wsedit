{-# LANGUAGE LambdaCase #-}

module WSEdit where


import Control.Exception        (SomeException, try)
import Control.Monad            (when)
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.RWS.Strict (ask, get, modify, runRWST)
import Data.Default             (def)
import Data.List                ( isInfixOf, isPrefixOf, isSuffixOf, partition
                                , stripPrefix
                                )
import Data.Maybe               (catMaybes, fromMaybe)
import Graphics.Vty             ( Event (EvKey, EvResize)
                                , Key (KChar)
                                , mkVty, nextEvent, shutdown
                                )
import Safe                     ( atDef, headDef, headMay, lastNote, readDef
                                , readNote
                                )
import System.Directory         ( doesDirectoryExist, getHomeDirectory
                                , listDirectory
                                )
import System.Environment       (getArgs)
import System.Exit              ( ExitCode (ExitFailure, ExitSuccess)
                                , exitFailure, exitWith
                                )
import System.IO                ( Newline (LF, CRLF)
                                , NewlineMode (NewlineMode)
                                , mkTextEncoding
                                , universalNewlineMode
                                )

import WSEdit.Control           ( bail, deleteSelection, insert
                                , listAutocomplete, load, quitComplain, save
                                , standby
                                )
import WSEdit.Data              ( EdConfig ( blockComment, brackets, chrDelim
                                           , drawBg, dumpEvents, edDesign
                                           , encoding, escape, keymap, keywords
                                           , lineComment, mStrDelim, newlineMode
                                           , purgeOnClose, strDelim, vtyObj
                                           , tabWidth
                                           )
                                , EdState ( buildDict, changed, continue
                                          , detectTabs, fname, lastEvent
                                          , loadPos, readOnly, replaceTabs
                                          , searchTerms, status
                                          )
                                , WSEdit
                                , brightTheme, catchEditor, mkDefConfig
                                , setStatus
                                )
import WSEdit.Data.Pretty       (unPrettyEdConfig)
import WSEdit.Help              (confHelp, keymapHelp, usageHelp, versionHelp)
import WSEdit.Keymaps           (defaultKM)
import WSEdit.Renderer          (rebuildAll)
import WSEdit.Output            (draw, drawExitFrame)
import WSEdit.Util              ( getExt, linesPlus, mayReadFile, unlinesPlus
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
                       | otherwise               -> getExt <$> filename
                    l                            -> Just
                                                  $ drop 3
                                                  $ lastNote (fqn "splitArgs") l

        swChain  = (if "-!" `elem` sw then ["-!"] else ["-"])
                ++ filterFileArgs fext glob
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
    h <- liftIO getHomeDirectory

    b <- doesDirectoryExist $ h ++ "/.config/wsedit"

    mods <- if b
               then fmap concat
                    ( mapM ( fmap (linesPlus . fromMaybe "")
                           . mayReadFile
                           . ((h ++ "/.config/wsedit/") ++ )
                           )
                    . filter (isSuffixOf ".wsconf")
                  =<< listDirectory (h ++ "/.config/wsedit")
                    )
               else return [""]


    glob <- fromMaybe "" <$> mayReadFile (h ++ "/.config/wsedit.wsconf")
    loc  <- fromMaybe "" <$> mayReadFile "./.local.wsconf"

    -- split the parameters into a more comfortable format
    let (filename, tLnNo, tColNo, sw, dashS)
            = splitArgs args (mods ++ linesPlus glob) $ lines loc

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
                                                         $ linesPlus r

                                        conf' = unPrettyEdConfig
                                                (vtyObj conf)
                                                (keymap conf)
                                              $ readNote (fqn "start:1")
                                              $ unlinesPlus cLines

                                        st    = readNote (fqn "start:2")
                                              $ unlinesPlus sLines

                                    return (conf', st)

                     else return (conf, def { fname   = fromMaybe "" filename
                                            , loadPos = (tLnNo, tColNo)
                                            }
                                 )

    _ <- case argLoop h False sw (conf', st) of
              Right (c, s)    -> do

                -- Test whether the file encoding is actually usable
                case encoding c of
                     Nothing -> return ()
                     Just  e -> try (mkTextEncoding e) >>= \case
                            Right _ -> return ()
                            Left ex -> do
                                const (return ()) (ex :: SomeException)
                                shutdown v
                                putStrLn $ "-e: Encoding not available: " ++ e
                                exitFailure

                runRWST (exec $ not dashS) c s

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
                quitComplain $ "An uncommon I/O error occured while loading:\n\n"
                            ++ show e

            rebuildAll Nothing

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
     ++ filter (/= "") (map (fromMaybe "" . stripPrefix (ext ++ ":")) loc)





-- | Parse all switches passed to it.  The first parameter takes the user's home
--   directory, the second enables failsafe mode.
argLoop :: String -> Bool -> [String] -> (EdConfig, EdState) -> Either (ExitCode, String) (EdConfig, EdState)
argLoop _ _ (('-':'h':'c'        :_ ):_ ) (_, _) = Left (ExitSuccess, confHelp             )
argLoop _ _ (('-':'h':'k'        :_ ):_ ) (c, _) = Left (ExitSuccess, keymapHelp $ keymap c)
argLoop _ _ (('-':'h'            :_ ):_ ) _      = Left (ExitSuccess, usageHelp            )
argLoop _ _ (('-':'V'            :_ ):_ ) _      = Left (ExitSuccess, versionHelp          )
argLoop h _ (('-':'!'            :_ ):xs) (c, s) = argLoop h True xs (c, s)
argLoop h f (('-':'s'            :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c, s)  -- gets handled elsewhere, ignore it here.
argLoop h f (('-':'f':'f'        :_ ):xs) (c, s) = argLoop h f          xs  (c, s)  -- same
argLoop h f (('-':'b'            :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c { drawBg       = False                                                          }, s)
argLoop h f (('-':'B'            :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c { drawBg       = True                                                           }, s)
argLoop h f (('-':'e'            :x ):xs) (c, s) = argLoop h f          xs  (c { encoding     = Just x                                                         }, s)
argLoop h f (('-':'E'            :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c { encoding     = Nothing                                                        }, s)
argLoop h f (('-':'f':'b':'r':'+':x ):xs) (c, s) = argLoop h f          xs  (c { brackets     = withSnd (drop 1) (span (/='_') x) : brackets c                 }, s)
argLoop h f (('-':'f':'b':'r':'-':x ):xs) (c, s) = argLoop h f          xs  (c { brackets     = filter (/= withSnd (drop 1) (span (/='_') x)) $ brackets c     }, s)
argLoop h f (('-':'f':'e':'+': e :[]):xs) (c, s) = argLoop h f          xs  (c { escape       = Just e                                                         }, s)
argLoop h f (('-':'f':'e':'-'    :[]):xs) (c, s) = argLoop h f          xs  (c { escape       = Nothing                                                        }, s)
argLoop h f (('-':'f':'k':'+'    :x ):xs) (c, s) = argLoop h f          xs  (c { keywords     = x : keywords c                                                 }, s)
argLoop h f (('-':'f':'k':'-'    :x ):xs) (c, s) = argLoop h f          xs  (c { keywords     = filter (/= x) $ keywords c                                     }, s)
argLoop h f (('-':'f':'l':'c':'+':x ):xs) (c, s) = argLoop h f          xs  (c { lineComment  = x : lineComment c                                              }, s)
argLoop h f (('-':'f':'l':'c':'-':x ):xs) (c, s) = argLoop h f          xs  (c { lineComment  = filter (/= x) $ lineComment c                                  }, s)
argLoop h f (('-':'f':'b':'c':'+':x ):xs) (c, s) = argLoop h f          xs  (c { blockComment = withSnd (drop 1) (span (/='_') x) : blockComment c             }, s)
argLoop h f (('-':'f':'b':'c':'-':x ):xs) (c, s) = argLoop h f          xs  (c { blockComment = filter (/= withSnd (drop 1) (span (/='_') x)) $ blockComment c }, s)
argLoop h f (('-':'f':'s':'+':a:b:[]):xs) (c, s) = argLoop h f          xs  (c { strDelim     = ([a], [b]) : strDelim c                                        }, s)
argLoop h f (('-':'f':'s':'-':a:b:[]):xs) (c, s) = argLoop h f          xs  (c { strDelim     = filter (/= ([a], [b])) $ strDelim c                            }, s)
argLoop h f (('-':'f':'s':'+'    :x ):xs) (c, s) = argLoop h f          xs  (c { strDelim     = withSnd (drop 1) (span (/='_') x) : strDelim c                 }, s)
argLoop h f (('-':'f':'s':'-'    :x ):xs) (c, s) = argLoop h f          xs  (c { strDelim     = filter (/= withSnd (drop 1) (span (/='_') x)) $ strDelim c     }, s)
argLoop h f (('-':'f':'m':'s':'+':x ):xs) (c, s) = argLoop h f          xs  (c { mStrDelim    = withSnd (drop 1) (span (/='_') x) : mStrDelim c                }, s)
argLoop h f (('-':'f':'m':'s':'-':x ):xs) (c, s) = argLoop h f          xs  (c { mStrDelim    = filter (/= withSnd (drop 1) (span (/='_') x)) $ mStrDelim c    }, s)
argLoop h f (('-':'f':'c':'s':'+':x ):xs) (c, s) = argLoop h f          xs  (c { chrDelim     = withSnd (drop 1) (span (/='_') x) : chrDelim c                 }, s)
argLoop h f (('-':'f':'c':'s':'-':x ):xs) (c, s) = argLoop h f          xs  (c { chrDelim     = filter (/= withSnd (drop 1) (span (/='_') x)) $ chrDelim c     }, s)
argLoop h f (('-':'i'            :n ):xs) (c, s) = argLoop h f          xs  (c { tabWidth     = readNote (fqn "argLoop") n                                     }, s)
argLoop h f (('-':'l':'u'        :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c { newlineMode  = NewlineMode CRLF LF                                            }, s)
argLoop h f (('-':'l':'w'        :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c { newlineMode  = NewlineMode CRLF CRLF                                          }, s)
argLoop h f (('-':'L'            :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c { newlineMode  = universalNewlineMode                                           }, s)
argLoop h f (('-':'p'            :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c { purgeOnClose = True                                                           }, s)
argLoop h f (('-':'P'            :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c { purgeOnClose = False                                                          }, s)
argLoop h f (('-':'x'            :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c { edDesign     = brightTheme                                                    }, s)
argLoop h f (('-':'X'            :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c { edDesign     = def                                                            }, s)
argLoop h f (('-':'y'            :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c { dumpEvents   = True                                                           }, s)
argLoop h f (('-':'Y'            :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c { dumpEvents   = False                                                          }, s)
argLoop h f (('-':'c':'g'        :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c, s { fname       = h ++ "/.config/wsedit.wsconf"                                })
argLoop h f (('-':'c':'l'        :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c, s { fname       = "./.local.wsconf"                                            })
argLoop h f (('-':'d':'+':'*'    :x ):xs) (c, s) = argLoop h f          xs  (c, s { buildDict   = (Just x , Nothing                     ) : buildDict s        })
argLoop h f (('-':'d':'+': d     :x ):xs) (c, s) = argLoop h f          xs  (c, s { buildDict   = (Just x , Just $ readNote (fqn "argLoop") [d]) : buildDict s })
argLoop h f (('-':'d':'~':'*'    :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c, s { buildDict   = (Nothing, Nothing                     ) : buildDict s        })
argLoop h f (('-':'d':'~': d     :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c, s { buildDict   = (Nothing, Just $ readNote (fqn "argLoop") [d]) : buildDict s })
argLoop h f (('-':'D'            :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c, s { buildDict   = []                                                           })
argLoop h f (('-':'f':'h':'+'    :x ):xs) (c, s) = argLoop h f          xs  (c, s { searchTerms = x : searchTerms s                                            })
argLoop h f (('-':'f':'h':'-'    :x ):xs) (c, s) = argLoop h f          xs  (c, s { searchTerms = filter (/= x) $ searchTerms s                                })
argLoop h f (('-':'r'            :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c, s { readOnly    = True                                                         })
argLoop h f (('-':'R'            :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c, s { readOnly    = False                                                        })
argLoop h f (('-':'t':'s'        :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c, s { replaceTabs = True
                                                                                  , detectTabs  = False                                                      })
argLoop h f (('-':'t':'t'        :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c, s { replaceTabs = False
                                                                                  , detectTabs  = False                                                        })
argLoop h f (('-':'T'            :x ):xs) (c, s) = argLoop h f (('-':x):xs) (c, s { detectTabs  = True                                                         })

argLoop h f (['-']                   :xs) (c, s) = argLoop h f          xs  (c, s)
argLoop _ _ []                            (c, s) = if fname s == ""
                                                      then Left  (ExitFailure 1, "No file specified (try wsedit -h).")
                                                      else Right (c, s)

argLoop _ False (('-'            :x ):_ ) _      = Left (ExitFailure 1, "Unknown argument: -" ++ x ++ " (try wsedit -h)")
argLoop _ False (x                   :_ ) _      = Left (ExitFailure 1, "Unexpected parameter: " ++ x ++ " (try wsedit -h)")
argLoop h True  (_                   :xs) (c, s) = argLoop h True xs (c, s)





-- | Main editor loop. Runs once per input event processed.
mainLoop :: WSEdit ()
mainLoop = do
    flip catchEditor errHdl $ do
        draw
        setStatus ""

        c <- ask
        s <- get
        ev <- liftIO $ nextEvent $ vtyObj c

        modify (\st -> st { lastEvent = Just ev })

        -- look up the event in the keymap
        -- if not found: insert the pressed key
        -- if it's not alphanumeric: show an "event not bound" warning
        maybe (case ev of
                    EvKey (KChar k) [] -> deleteSelection
                                       >> insert k
                                       >> listAutocomplete

                    EvResize _ _       -> return ()
                    _                  -> setStatus $ "Event not bound: "
                                                   ++ show ev
              )
              fst
              $ lookup ev
              $ catMaybes
              $ keymap c

        rebuildAll $ Just s

        when (dumpEvents c) $ do
            st <- get
            setStatus $ show ev ++ status st

    b <- continue <$> get
    when b mainLoop

    where
        errHdl :: SomeException -> WSEdit ()
        errHdl e = do
            b <- changed <$> get
            if b
               then do
                    modify (\s -> s { fname = "CRASH-RESCUE" })
                    standby $ "An error occured: " ++ show e
                           ++ "\n\n"
                           ++ "Dumping unsaved changes..."
                    save
                    bail $ "An error occured: " ++ show e
                        ++ "\n\n"
                        ++ "All unsaved changes have been dumped to"
                        ++ " ./CRASH-RESCUE ."

               else bail $ "An error occured: " ++ show e
