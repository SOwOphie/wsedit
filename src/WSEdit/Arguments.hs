{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

{-# LANGUAGE LambdaCase #-}

module WSEdit.Arguments
    ( parseArguments
    ) where


import Control.Monad
    ( foldM
    , forM
    , unless
    , when
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Data.Default
    ( def
    )
import Data.Either
    ( lefts
    , rights
    )
import Data.List
    ( delete
    , inits
    , isSuffixOf
    , nub
    , null
    , (\\)
    )
import Data.Maybe
    ( catMaybes
    , fromMaybe
    )
import Safe
    ( lastMay
    , maximumDef
    , readMay
    )
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , getCurrentDirectory
    , getHomeDirectory
    )
import System.Environment
    ( getArgs
    )
import System.Exit
    ( ExitCode
        ( ExitSuccess
        , ExitFailure
        )
    , exitWith
    )
import System.FilePath
    ( joinPath
    , splitPath
    , takeDirectory
    , (</>)
    )
import System.IO
    ( Newline
        ( CRLF
        , LF
        )
    , NewlineMode
        ( NewlineMode
        )
    , universalNewlineMode
    )
import Text.Parsec
    ( runP
    )
import Text.Show.Pretty
    ( ppShow
    )

import WSEdit.Arguments.Data
    ( ArgBlock
        ( ArgBlock
        , abMatch
        , abArg
        )
    , Argument
        (..) -- not listing all those off one by one
    , unProtoAB
    , unProtoFM
    )
import WSEdit.Arguments.Parser
    ( configCmd
    , configFile
    )
import WSEdit.Data
    ( CanonicalPath
        ( CanonicalPath
        , getCanonicalPath
        )
    , EdConfig
        ( addnIdChars
        , atomicSaves
        , blockComment
        , brackets
        , chrDelim
        , drawBg
        , dumpEvents
        , edDesign
        , encoding
        , escapeO
        , escapeS
        , initJMarks
        , keymap
        , keyPrfxs
        , keywords
        , lineComment
        , mStrDelim
        , newlineMode
        , purgeOnClose
        , readEnc
        , strDelim
        , tabWidth
        , vtyObj
        , wriCheck
        )
    , EdState
        ( EdState
#ifndef dev
        , badgeText
#endif
        , buildDict
        , detectTabs
        , elTabCache
        , fname
        , loadPos
        , readOnly
        , replaceTabs
        , searchTerms
        )
    , ReadEnc
        ( ReadEncSet
        , ReadEncAuto
        , ReadEncDef
        )
    , Stability
        ( Release
        )
    , brightTheme
    , stability
    , upstream
    )
import WSEdit.Data.Algorithms
    ( canonicalPath
    , fileMatch
    )
import WSEdit.Data.Pretty
    ( unPrettyEdConfig
    )
import WSEdit.Help
    ( confHelp
    , keymapHelp
    , usageHelp
    , versionHelp
    )
import WSEdit.Util
    ( linesPlus
    , mayReadFile
    , listDirectoryDeep
    , readEncFile
    , unlinesPlus
    , withFst
    , withSnd
    )

import qualified WSEdit.Buffer as B



-- | Abort the loading process with a status code and a message.
abort :: ExitCode -> String -> IO a
abort c s = do
    putStrLn s
    exitWith c



-- | Some options provide files to match syntax against, this is where this
--   information is recorded.
providedFile :: Argument -> Maybe FilePath
providedFile  HelpGeneral        = Just ""
providedFile  HelpConfig         = Just ""
providedFile  HelpKeybinds       = Just ""
providedFile  HelpVersion        = Just ""
providedFile  OtherOpenCfLoc     = Just ".local.wsconf"
providedFile  OtherOpenCfGlob    = Just "/home/user/.config/wsedit.wsconf"
providedFile (SpecialSetFile  s) = Just s
providedFile  _                  = Nothing



-- | Takes initial config/state, reads in all necessary arguments and files,
--   then returns the modified config/state pair as well as whether the main
--   function needs to call `load` itself. The function will terminate the
--   running program directly without returning in case one of the options
--   mandates it (e.g. help requested, parse error, ...).
parseArguments :: (EdConfig, EdState) -> IO ((EdConfig, EdState), Bool)
parseArguments (c, s) = do

    -- read arguments and files
    args  <- getArgs
    cwd   <- getCurrentDirectory >>= canonicalPath Nothing

    -- parse them
    let
        parsedArgs  = runP configCmd cwd "command line"
                    $ fancyUnwords args

    -- errors in command line arguments?
    case parsedArgs of
         Left  e -> do
            -- Yes? Complain, quit.
            abort (ExitFailure 1)
                $ "Command line argument parse error:\n" ++ show e

         Right a ->
            -- No? => a contains all parsed cmd arguments
            let
                -- Figure out the initial file to match against
                targetFName = lastMay
                            $ catMaybes
                            $ map providedFile a
            in
                case targetFName of
                     -- No file? Complain, quit.
                     Nothing -> abort (ExitFailure 1)
                                    "No file selected, exiting now (see -h)."

                     -- File found. Note that this may not be the actual file
                     -- name that we're opening later, just something closely
                     -- resembling it. See 'providedFile' for more info.
                     Just f  -> do

                        -- Read and parse config files
                        files <- readConfigFiles =<< liftIO (canonicalPath Nothing f)

                        let
                            parsedFiles = map (\(p, x) -> runP configFile
                                                               ( CanonicalPath
                                                               $ takeDirectory
                                                               $ getCanonicalPath p
                                                               )
                                                               (getCanonicalPath p)
                                                               x
                                              ) files

                            -- list of all parse errors, regardless of source
                            parseErrors = lefts parsedFiles
                                       ++ lefts [parsedArgs]

                        -- list of successfully parsed options from files
                        -- only (!)
                        parsedSuccF <- mapM unProtoAB
                                     $ concat
                                     $ rights
                                     $ parsedFiles

                        -- Assemble all the file names to match against. This
                        -- includes the file we just obtained as well as all
                        -- names provided by -mi given in command line
                        -- parameters.
                        finf <- mapM (canonicalPath Nothing)
                             $ f : catMaybes (map (\case
                                                        MetaInclude n -> Just n
                                                        _             -> Nothing
                                                  )
                                                  a
                                             )


                        -- Apply recursive algorithm to select all active
                        -- arguments from files.
                        selArgs <- selectArgs finf parsedSuccF

                        let
                            -- All active arguments
                            allArgs = selArgs ++ a

                            -- Selected release stability: the smallest value
                            -- given, or 'Release' if omitted.
                            selStab = maximumDef Release
                                    $ catMaybes
                                    $ map (\case
                                                DebugStability x -> Just x
                                                _                -> Nothing
                                          )
                                    $ allArgs

                        -- Report parse errors and abort if no -mf is active.
                        when ( (not $ null parseErrors)
                            && MetaFailsafe `notElem` allArgs
                             )
                             $ abort (ExitFailure 1)
                             $ "Parse error(s) occured:\n"
                            ++ unlines (map show parseErrors)
                            ++ "Tip: Use -mf to ignore all files containing errors and fix them."

                        -- Abort if the release is not as stable as the user
                        -- wants it to be.
                        when (stability < selStab)
                             $ abort (ExitFailure 1)
                             $ "This release is not stable enough for your preferences:\n\n"
                            ++ "    " ++ show stability ++ " < " ++ show selStab ++ "\n\n"
                            ++ "Getting the latest stable release from the \"Releases\" section\n"
                            ++ "on " ++ upstream ++ " is highly recommended,\n"
                            ++ "but you can also continue using this unstable version by passing\n"
                            ++ "-ys " ++ show stability ++ " or adding it to a config file."

                        -- Dump arguments if desired
                        when (DebugDumpArgs `elem` allArgs) $ do
                            h <- getHomeDirectory
                            appendFile (h ++ "/wsedit-arg-dump")
                                $ "\n\n"
                               ++ ppShow allArgs

                        -- Apply arguments to config/state pair.
                        (c', s') <- foldM applyArg (c, s) allArgs

                        -- State file processing
                        let sf = MetaStateFile `elem` allArgs
                        (c'', s'') <- if sf
                                         then loadSF (c', s')
                                         else return (c', s')

                        return ((c'', s''), not sf)

    where
        -- | User home directory -> wsedit config directory
        confDirs :: FilePath -> [FilePath]
        confDirs home = [ "/etc/wsedit/"
                        , "/usr/local/etc/wsedit/"
                        , home </> ".config/wsedit/"
                        ]

        -- | User home directory -> global wsedit config file
        globC :: String -> String
        globC = (++ "/.config/wsedit.wsconf")


        -- | Read in all config files.
        readConfigFiles :: CanonicalPath -> IO [(CanonicalPath, String)]
        readConfigFiles (CanonicalPath p) = do
            h <- getHomeDirectory

            globConfs <- fmap (filter (isSuffixOf ".wsconf") . concat)
                       $ forM (confDirs h)
                       $ \d -> do
                            b <- doesDirectoryExist d
                            if b
                               then listDirectoryDeep d
                               else return []

            let locConfs = map ((</> ".local.wsconf") . joinPath)
                         $ filter (not . null)
                         $ inits
                         $ splitPath
                         $ takeDirectory p

            mapM (\n -> do
                            i <- canonicalPath Nothing n
                            x <- mayReadFile   n
                            return (i, fromMaybe "" x)
                 ) $ globConfs ++ [globC h] ++ locConfs


        -- | Unwords, escaping quotes, spaces and backslashes.
        fancyUnwords :: [String] -> String
        fancyUnwords = unwords . map esc

        esc :: String -> String
        esc []        = ""
        esc ('\\':xs) = "\\\\" ++ esc xs
        esc (' ' :xs) = "\\ "  ++ esc xs
        esc ('"' :xs) = "\\\"" ++ esc xs
        esc (x   :xs) = [x]    ++ esc xs


        -- | Load the file inside `fname` as state file.
        loadSF :: (EdConfig, EdState) -> IO (EdConfig, EdState)
        loadSF (cf, EdState { fname = f }) = do
            doesFileExist f >>= flip unless
                (abort (ExitFailure 1)
                    $ "File not found: " ++ f
                )

            putStrLn "Parsing state file, this may take a moment..."

            (_, sf) <- readEncFile f

            let t        = drop 1
                         $ dropWhile null
                         $ dropWhile (not. null)
                         $ linesPlus sf

                (tc, ts) = withSnd (drop 1 . dropWhile null)
                         $ span (not . null) t

                c'       = fmap ( unPrettyEdConfig (vtyObj cf)
                                                  (keymap cf)
                                )
                         $ readMay $ unlinesPlus tc
                s'       = readMay $ unlinesPlus ts

            case (c', s') of
                 (Nothing , _       ) -> do
                    abort (ExitFailure 1)
                        "Resume editor from dump: Parse error in config section."

                 (_       , Nothing ) -> do
                    abort (ExitFailure 1)
                        "Resume editor from dump: Parse error in state section."

                 (Just c'', Just s'') -> return (c'', s'')



-- | Given some files to match against as well as a bunch of argument blocks,
--   return a list of arguments that should be active.
selectArgs :: [CanonicalPath] -> [ArgBlock] -> IO [Argument]
selectArgs files args = do
    files' <- fmap nub
            $ mapM (canonicalPath Nothing)
            $ catMaybes
            $ map (\case { MetaInclude s -> Just s; _ -> Nothing })
            $ concatMap abArg
            $ filter (appliesTo files) args

    if null $ files' \\ files
       then return $ concatMap abArg
                   $ filter (appliesTo files) args

       else selectArgs (nub $ files ++ files') args

    where
        -- | Returns whether the argument block's selector is satisfied by any
        --   of the given files.
        appliesTo :: [CanonicalPath] -> ArgBlock -> Bool
        appliesTo fs (ArgBlock { abMatch = m }) = any (fileMatch m) fs



-- | Applies an argument to a config/state pair.
applyArg :: (EdConfig, EdState) -> Argument -> IO (EdConfig, EdState)
applyArg (c, s) (AutocompAdd     n f) = unProtoFM f >>= \f' -> return (c, s { buildDict   = (Just f', n) : buildDict s })

applyArg (c, s) (AutocompAddSelf n  ) = return (c, s { buildDict   = (Nothing, n) : buildDict s })
applyArg (c, s)  AutocompOff          = return (c, s { buildDict   = []                         })
applyArg (c, s)  EditorElTabsOn       = return (c, s { elTabCache  = B.fromList [[]]            })
applyArg (c, s)  EditorElTabsOff      = return (c, s { elTabCache  = Nothing                    })
applyArg (c, s)  EditorTabModeSpc     = return (c, s { replaceTabs = True
                                                     , detectTabs  = False
                                                     }
                                               )
applyArg (c, s)  EditorTabModeTab     = return (c, s { replaceTabs = False
                                                     , detectTabs  = False
                                                     }
                                               )
applyArg (c, s)  EditorTabModeAuto    = return (c, s { detectTabs  = True                         })
applyArg (c, s) (GeneralHighlAdd w  ) = return (c, s { searchTerms = w : delete w (searchTerms s) })
applyArg (c, s) (GeneralHighlDel w  ) = return (c, s { searchTerms =     delete w (searchTerms s) })
applyArg (c, s)  GeneralROOn          = return (c, s { readOnly    = True                         })
applyArg (c, s)  GeneralROOff         = return (c, s { readOnly    = False                        })


applyArg (c, s)  DebugDumpEvOn        = return (c { dumpEvents   = True                                    }, s)
applyArg (c, s)  DebugDumpEvOff       = return (c { dumpEvents   = False                                   }, s)
applyArg (c, s)  DebugWRIOff          = return (c { wriCheck     = False                                   }, s)
applyArg (c, s)  DebugWRIOn           = return (c { wriCheck     = True                                    }, s)
applyArg (c, s)  DisplayDotsOn        = return (c { drawBg       = True                                    }, s)
applyArg (c, s)  DisplayDotsOff       = return (c { drawBg       = False                                   }, s)
applyArg (c, s)  DisplayInvBGOn       = return (c { edDesign     = brightTheme                             }, s)
applyArg (c, s)  DisplayInvBGOff      = return (c { edDesign     = def                                     }, s)
applyArg (c, s) (EditorIndSet    n  ) = return (c { tabWidth     = n                                       }, s)
applyArg (c, s) (EditorJumpMAdd  n  ) = return (c { initJMarks   = n      : delete n      (initJMarks   c) }, s)
applyArg (c, s) (EditorJumpMDel  n  ) = return (c { initJMarks   =          delete n      (initJMarks   c) }, s)
applyArg (c, s)  FileAtomicOff        = return (c { atomicSaves  = False                                   }, s)
applyArg (c, s)  FileAtomicOn         = return (c { atomicSaves  = True                                    }, s)
applyArg (c, s) (FileEncodingSet e  ) = return (c { encoding     = Just e                                  }, s)
applyArg (c, s)  FileEncodingDef      = return (c { encoding     = Nothing                                 }, s)
applyArg (c, s)  FileLineEndUnix      = return (c { newlineMode  = NewlineMode CRLF   LF                   }, s)
applyArg (c, s)  FileLineEndWin       = return (c { newlineMode  = NewlineMode CRLF CRLF                   }, s)
applyArg (c, s)  FileLineEndDef       = return (c { newlineMode  = universalNewlineMode                    }, s)
applyArg (c, s) (FileReadEncSet  e  ) = return (c { readEnc      = ReadEncSet e                            }, s)
applyArg (c, s)  FileReadEncAuto      = return (c { readEnc      = ReadEncAuto                             }, s)
applyArg (c, s)  FileReadEncDef       = return (c { readEnc      = ReadEncDef                              }, s)
applyArg (c, s) (LangBracketAdd  a b) = return (c { brackets     = (a, b) : delete (a, b) (brackets     c) }, s)
applyArg (c, s) (LangBracketDel  a b) = return (c { brackets     =          delete (a, b) (brackets     c) }, s)
applyArg (c, s) (LangCommLineAdd a  ) = return (c { lineComment  = a      : delete a      (lineComment  c) }, s)
applyArg (c, s) (LangCommLineDel a  ) = return (c { lineComment  =          delete a      (lineComment  c) }, s)
applyArg (c, s) (LangCommBlkAdd  a b) = return (c { blockComment = (a, b) : delete (a, b) (blockComment c) }, s)
applyArg (c, s) (LangCommBlkDel  a b) = return (c { blockComment =          delete (a, b) (blockComment c) }, s)
applyArg (c, s) (LangEscOAdd     a  ) = return (c { escapeO      = a      : delete a      (escapeO      c) }, s)
applyArg (c, s) (LangEscODel     a  ) = return (c { escapeO      =          delete a      (escapeO      c) }, s)
applyArg (c, s) (LangEscSAdd     a  ) = return (c { escapeS      = a      : delete a      (escapeS      c) }, s)
applyArg (c, s) (LangEscSDel     a  ) = return (c { escapeS      =          delete a      (escapeS      c) }, s)
applyArg (c, s) (LangIdChrAdd    a  ) = return (c { addnIdChars  = a      : delete a      (addnIdChars  c) }, s)
applyArg (c, s) (LangIdChrDel    a  ) = return (c { addnIdChars  =          delete a      (addnIdChars  c) }, s)
applyArg (c, s) (LangKeywordAdd  a  ) = return (c { keywords     = a      : delete a      (keywords     c) }, s)
applyArg (c, s) (LangKeywordDel  a  ) = return (c { keywords     =          delete a      (keywords     c) }, s)
applyArg (c, s) (LangKeyPrfxAdd  a  ) = return (c { keyPrfxs     = a      : delete a      (keyPrfxs     c) }, s)
applyArg (c, s) (LangKeyPrfxDel  a  ) = return (c { keyPrfxs     =          delete a      (keyPrfxs     c) }, s)
applyArg (c, s) (LangStrChrAdd   a b) = return (c { chrDelim     = (a, b) : delete (a, b) (chrDelim     c) }, s)
applyArg (c, s) (LangStrChrDel   a b) = return (c { chrDelim     =          delete (a, b) (chrDelim     c) }, s)
applyArg (c, s) (LangStrMLAdd    a b) = return (c { mStrDelim    = (a, b) : delete (a, b) (mStrDelim    c) }, s)
applyArg (c, s) (LangStrMLDel    a b) = return (c { mStrDelim    =          delete (a, b) (mStrDelim    c) }, s)
applyArg (c, s) (LangStrRegAdd   a b) = return (c { strDelim     = (a, b) : delete (a, b) (strDelim     c) }, s)
applyArg (c, s) (LangStrRegDel   a b) = return (c { strDelim     =          delete (a, b) (strDelim     c) }, s)
applyArg (c, s)  OtherPurgeOn         = return (c { purgeOnClose = True                                    }, s)
applyArg (c, s)  OtherPurgeOff        = return (c { purgeOnClose = False                                   }, s)


applyArg (_, _)  HelpGeneral          = abort ExitSuccess     usageHelp
applyArg (_, _)  HelpConfig           = abort ExitSuccess      confHelp
applyArg (c, _)  HelpKeybinds         = abort ExitSuccess $  keymapHelp $ keymap c
applyArg (_, _)  HelpVersion          = abort ExitSuccess   versionHelp

applyArg (c, s)  DebugDumpArgs        = return (c, s)
applyArg (c, s) (DebugStability  _  ) = return (c, s)
applyArg (c, s) (MetaInclude     _  ) = return (c, s)
applyArg (c, s)  MetaFailsafe         = return (c, s)
applyArg (c, s)  MetaStateFile        = return (c, s)

applyArg (c, s)  OtherOpenCfLoc       =                            return (c, s { fname =               ".local.wsconf" })
applyArg (c, s)  OtherOpenCfGlob      = getHomeDirectory >>= \p -> return (c, s { fname = p ++ "/.config/wsedit.wsconf" })

applyArg (c, s) (SpecialSetFile  f  ) = return (c, s { fname = f })
applyArg (c, s) (SpecialSetVPos  n  ) = return (c, s { loadPos = withFst (const n) $ loadPos s })
applyArg (c, s) (SpecialSetHPos  n  ) = return (c, s { loadPos = withSnd (const n) $ loadPos s })

#ifndef dev
applyArg (c, s) (DisplayBadgeSet b  ) = return (c, s { badgeText   = Just b                     })
applyArg (c, s)  DisplayBadgeOff      = return (c, s { badgeText   = Nothing                    })
#else
applyArg (c, s) (DisplayBadgeSet _  ) = return (c, s)
applyArg (c, s)  DisplayBadgeOff      = return (c, s)
#endif
