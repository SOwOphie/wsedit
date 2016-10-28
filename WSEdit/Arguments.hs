{-# OPTIONS_GHC -fno-warn-missing-signatures
                -fno-warn-unused-do-bind
                #-}

{-# LANGUAGE LambdaCase #-}

module WSEdit.Arguments
    ( parseArguments
    ) where


import Control.Monad                 (foldM, when)
import Data.Default                  (def)
import Data.Either                   (lefts, rights)
import Data.List                     ( delete, intercalate, intersperse
                                     , isPrefixOf, isSuffixOf, nub
                                     , (\\)
                                     )
import Data.Maybe                    (catMaybes, fromMaybe)
import Safe                          (lastMay, maximumDef)
import System.Directory              ( doesDirectoryExist, getHomeDirectory
                                     , listDirectory
                                     )
import System.Environment            (getArgs)
import System.FilePath               (isRelative)
import System.IO                     ( Newline (CRLF, LF)
                                     , NewlineMode (NewlineMode)
                                     , universalNewlineMode
                                     )
import Text.ParserCombinators.Parsec ( Parser
                                     , char, choice, digit, eof, many, many1
                                     , newline, noneOf, oneOf, option, optional
                                     , parse, sepBy, sepEndBy, spaces, string
                                     , try
                                     , (<|>), (<?>)
                                     )

import WSEdit.Control.Global         (quitComplain)
import WSEdit.Data                   ( EdConfig ( blockComment, brackets
                                                , chrDelim, drawBg, dumpEvents
                                                , edDesign, encoding, escape
                                                , initJMarks, keymap, keywords
                                                , lineComment, mStrDelim
                                                , newlineMode, purgeOnClose
                                                , strDelim, tabWidth
                                                )
                                     , EdState ( buildDict, detectTabs, fname
                                               , loadPos, readOnly, replaceTabs
                                               , searchTerms
                                               )
                                     , PathInfo (absPath, relPath)
                                     , Stability (Release)
                                     , brightTheme, pathInfo, runWSEdit
                                     , stability, upstream
                                     )
import WSEdit.Help                   ( confHelp, keymapHelp, usageHelp
                                     , versionHelp
                                     )
import WSEdit.Util                   (mayReadFile, withFst, withSnd, dump)



-- | Match type, whether to match the entire name or just prefix and suffix.
data ABMatch = ExactName String
             | PrefSuf   String String
    deriving (Eq, Read, Show)



-- | Block of arguments that share a common file selector.
data ArgBlock = ArgBlock
    { abMatch  :: ABMatch
    , abArg    :: [Argument]
    }
    deriving (Eq, Read, Show)



-- | Argument type.
data Argument = AutocompAdd     Int    String
              | AutocompAddSelf Int
              | AutocompOff

              | DebugDumpEvOn
              | DebugDumpEvOff
              | DebugStability  Stability

              | DisplayDotsOn
              | DisplayDotsOff
              | DisplayInvBGOn
              | DisplayInvBGOff

              | EditorIndSet    Int
              | EditorJumpMAdd  Int
              | EditorJumpMDel  Int
              | EditorTabModeSpc
              | EditorTabModeTab
              | EditorTabModeAuto

              | FileEncodingSet String
              | FileEncodingDef
              | FileLineEndUnix
              | FileLineEndWin
              | FileLineEndDef

              | GeneralHighlAdd String
              | GeneralHighlDel String
              | GeneralROOn
              | GeneralROOff

              | HelpGeneral
              | HelpConfig
              | HelpKeybinds
              | HelpVersion

              | LangBracketAdd  String String
              | LangBracketDel  String String
              | LangCommLineAdd String
              | LangCommLineDel String
              | LangCommBlkAdd  String String
              | LangCommBlkDel  String String
              | LangEscapeSet   Char
              | LangEscapeOff
              | LangKeywordAdd  String
              | LangKeywordDel  String
              | LangStrChrAdd   String String
              | LangStrChrDel   String String
              | LangStrMLAdd    String String
              | LangStrMLDel    String String
              | LangStrRegAdd   String String
              | LangStrRegDel   String String

              | MetaFailsafe
              | MetaInclude     String
              | MetaStateFile

              | OtherOpenCfLoc
              | OtherOpenCfGlob
              | OtherPurgeOn
              | OtherPurgeOff

              | SpecialSetFile  String
              | SpecialSetVPos  Int
              | SpecialSetHPos  Int

    deriving (Eq, Read, Show)



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
--   then returns the modified config/state pair. The function will terminate
--   the running program directly without returning in case one of the options
--   mandates it (e.g. help requested, parse error, ...).
parseArguments :: (EdConfig, EdState) -> IO (EdConfig, EdState)
parseArguments (c, s) = do
    args  <- getArgs
    files <- readConfigFiles

    let
        parsedFiles = map (\(p, x) ->  parse configFile (absPath p) x) files
        parsedArgs  = parse configCmd "command line" $ unwords args

        parsedSucc  = concat $ rights $ parsedFiles
        parseErrors = lefts parsedFiles
                   ++ lefts [parsedArgs]

    case parsedArgs of
         Left  e -> do
            runWSEdit (c, s)
                  $ quitComplain
                  $ "Command line argument parse error:\n" ++ show e

            return (c, s)

         Right a ->
            let
                targetFName = lastMay
                            $ catMaybes
                            $ map providedFile a
            in
                case targetFName of
                     Nothing -> runWSEdit (c, s) (quitComplain "No file selected, exiting now (see -h).")
                             >> return (c, s)

                     Just f  -> do
                        finf <- mapM pathInfo
                             $ f : catMaybes (map (\case
                                                        MetaInclude n -> Just n
                                                        _             -> Nothing
                                                  )
                                                  a
                                             )


                        selArgs <- selectArgs finf parsedSucc

                        let
                            allArgs = selArgs ++ a
                            selStab = maximumDef Release
                                    $ catMaybes
                                    $ map (\case
                                                DebugStability x -> Just x
                                                _                -> Nothing
                                          )
                                    $ allArgs

                        when ((not $ null parseErrors) && MetaFailsafe `notElem` allArgs)
                            $ runWSEdit (c, s)
                            $ quitComplain
                            $ "Parse error(s) occured:\n" ++ unlines (map show parseErrors)

                        when (stability < selStab)
                             $ runWSEdit (c, s)
                             $ quitComplain
                             $ "This release is not stable enough for your preferences:\n\n"
                            ++ "    " ++ show stability ++ " < " ++ show selStab ++ "\n\n"
                            ++ "Getting the latest stable release from the \"Releases\" section\n"
                            ++ "on " ++ upstream ++ " is highly recommended,\n"
                            ++ "but you can also continue using this unstable version by passing\n"
                            ++ "-ys " ++ show stability ++ " or adding it to a config file."

                        foldM applyArg (c, s) $ dump "allArgs" allArgs

    where
        confDir :: String -> String
        confDir = (++ "/.config/wsedit/")

        globC :: String -> String
        globC = (++ "/.config/wsedit.wsconf")

        locC :: String
        locC = ".local.wsconf"


        readConfigFiles :: IO [(PathInfo, String)]
        readConfigFiles = do
            h      <- getHomeDirectory
            b      <- doesDirectoryExist $ confDir h

            fnames <- if not b
                         then return []
                         else fmap ( map (confDir h ++)
                                   . filter (isSuffixOf ".wsconf")
                                   )
                            $ listDirectory
                            $ confDir h

            confFiles <- mapM (\n -> do
                                    i <- pathInfo    n
                                    x <- mayReadFile n
                                    return (i, fromMaybe "" x)
                              )
                              fnames

            glob <- fmap (fromMaybe "") $ mayReadFile $ globC h
            loc  <- fmap (fromMaybe "") $ mayReadFile $ locC

            piGlob <- pathInfo $ globC h
            piLoc  <- pathInfo    locC

            return $ confFiles ++ [(piGlob, glob), (piLoc, loc)]



-- | Given some files to match against as well as a bunch of argument blocks,
--   return a list of arguments that should be active.
selectArgs :: [PathInfo] -> [ArgBlock] -> IO [Argument]
selectArgs files args = do
    files' <- mapM pathInfo
            $ catMaybes
            $ map (\case { MetaInclude s -> Just s; _ -> Nothing })
            $ concatMap abArg
            $ filter (appliesTo files) args

    if null $ files' \\ files
       then return $ concatMap abArg
                   $ filter (appliesTo files) args

       else selectArgs (nub $ files ++ files') args



-- | Returns whether the argument block's selector is satisfied by any of the
--   given files.
appliesTo :: [PathInfo] -> ArgBlock -> Bool
appliesTo files (ArgBlock { abMatch = m }) =
    case m of
         ExactName s   -> s `elem` map (if isRelative s
                                           then relPath
                                           else absPath
                                       ) files

         PrefSuf s1 s2 -> any (\s -> s1 `isPrefixOf` s
                                  && s2 `isSuffixOf` s
                              )
                        $ map (if isRelative s1
                                  then relPath
                                  else absPath
                              ) files



-- | Applies an argument to a config/state pair.
applyArg :: (EdConfig, EdState) -> Argument -> IO (EdConfig, EdState)
applyArg (c, s) (AutocompAdd     n f) = return (c, s { buildDict   = (Just f , Just n) : buildDict s })
applyArg (c, s) (AutocompAddSelf n  ) = return (c, s { buildDict   = (Nothing, Just n) : buildDict s })
applyArg (c, s)  AutocompOff          = return (c, s { buildDict   = []                              })

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
applyArg (c, s)  DisplayDotsOn        = return (c { drawBg       = False                                   }, s)
applyArg (c, s)  DisplayDotsOff       = return (c { drawBg       = True                                    }, s)
applyArg (c, s)  DisplayInvBGOn       = return (c { edDesign     = brightTheme                             }, s)
applyArg (c, s)  DisplayInvBGOff      = return (c { edDesign     = def                                     }, s)
applyArg (c, s) (EditorIndSet    n  ) = return (c { tabWidth     = n                                       }, s)
applyArg (c, s) (EditorJumpMAdd  n  ) = return (c { initJMarks   = n      : delete n      (initJMarks   c) }, s)
applyArg (c, s) (EditorJumpMDel  n  ) = return (c { initJMarks   =          delete n      (initJMarks   c) }, s)
applyArg (c, s) (FileEncodingSet e  ) = return (c { encoding     = Just e                                  }, s)
applyArg (c, s)  FileEncodingDef      = return (c { encoding     = Nothing                                 }, s)
applyArg (c, s)  FileLineEndUnix      = return (c { newlineMode  = NewlineMode CRLF   LF                   }, s)
applyArg (c, s)  FileLineEndWin       = return (c { newlineMode  = NewlineMode CRLF CRLF                   }, s)
applyArg (c, s)  FileLineEndDef       = return (c { newlineMode  = universalNewlineMode                    }, s)
applyArg (c, s) (LangBracketAdd  a b) = return (c { brackets     = (a, b) : delete (a, b) (brackets     c) }, s)
applyArg (c, s) (LangBracketDel  a b) = return (c { brackets     =          delete (a, b) (brackets     c) }, s)
applyArg (c, s) (LangCommLineAdd a  ) = return (c { lineComment  = a      : delete a      (lineComment  c) }, s)
applyArg (c, s) (LangCommLineDel a  ) = return (c { lineComment  =          delete a      (lineComment  c) }, s)
applyArg (c, s) (LangCommBlkAdd  a b) = return (c { blockComment = (a, b) : delete (a, b) (blockComment c) }, s)
applyArg (c, s) (LangCommBlkDel  a b) = return (c { blockComment =          delete (a, b) (blockComment c) }, s)
applyArg (c, s) (LangEscapeSet   a  ) = return (c { escape       = Just a                                  }, s)
applyArg (c, s)  LangEscapeOff        = return (c { escape       = Nothing                                 }, s)
applyArg (c, s) (LangKeywordAdd  a  ) = return (c { keywords     = a      : delete a      (keywords     c) }, s)
applyArg (c, s) (LangKeywordDel  a  ) = return (c { keywords     =          delete a      (keywords     c) }, s)
applyArg (c, s) (LangStrChrAdd   a b) = return (c { chrDelim     = (a, b) : delete (a, b) (chrDelim     c) }, s)
applyArg (c, s) (LangStrChrDel   a b) = return (c { chrDelim     =          delete (a, b) (chrDelim     c) }, s)
applyArg (c, s) (LangStrMLAdd    a b) = return (c { mStrDelim    = (a, b) : delete (a, b) (mStrDelim    c) }, s)
applyArg (c, s) (LangStrMLDel    a b) = return (c { mStrDelim    =          delete (a, b) (mStrDelim    c) }, s)
applyArg (c, s) (LangStrRegAdd   a b) = return (c { strDelim     = (a, b) : delete (a, b) (strDelim     c) }, s)
applyArg (c, s) (LangStrRegDel   a b) = return (c { strDelim     =          delete (a, b) (strDelim     c) }, s)
applyArg (c, s)  OtherPurgeOn         = return (c { purgeOnClose = True                                    }, s)
applyArg (c, s)  OtherPurgeOff        = return (c { purgeOnClose = False                                   }, s)


applyArg (c, s)  HelpGeneral          = runWSEdit (c, s) (quitComplain     usageHelp           ) >> return (c, s)
applyArg (c, s)  HelpConfig           = runWSEdit (c, s) (quitComplain      confHelp           ) >> return (c, s)
applyArg (c, s)  HelpKeybinds         = runWSEdit (c, s) (quitComplain $  keymapHelp $ keymap c) >> return (c, s)
applyArg (c, s)  HelpVersion          = runWSEdit (c, s) (quitComplain   versionHelp           ) >> return (c, s)

applyArg (c, s) (MetaInclude     _  ) = return (c, s)
applyArg (c, s)  MetaFailsafe         = return (c, s)
applyArg (c, s) (DebugStability  _  ) = return (c, s)

applyArg (c, s)  OtherOpenCfLoc       =                            return (c, s { fname =               ".local.wsconf" } )
applyArg (c, s)  OtherOpenCfGlob      = getHomeDirectory >>= \p -> return (c, s { fname = p ++ "/.config/wsedit.wsconf" } )

applyArg (c, s) (SpecialSetFile  f  ) = return (c, s { fname = f })
applyArg (c, s) (SpecialSetVPos  n  ) = return (c, s { loadPos = withFst (const n) $ loadPos s })
applyArg (c, s) (SpecialSetHPos  n  ) = return (c, s { loadPos = withSnd (const n) $ loadPos s })


-- placeholder
applyArg (c, s)  MetaStateFile        = return (c, s)





-- Boring parsec gibberish incoming... --

configCmd :: Parser [Argument]
configCmd = fmap concat
          $ sequence
          $ intersperse' (optional spaces' >> return [])
          $ intersperse' (configOption `sepBy` spaces')
          $ map (\x -> option [] (x >>= return . return))
          [ specialSetFile
          , specialSetVPos
          , specialSetHPos
          ]
    where
        intersperse' :: a -> [a] -> [a]
        intersperse' el [] = [el]
        intersperse' el l  = el : intersperse el l ++ [el]



configFile :: Parser [ArgBlock]
configFile =  (fmap catMaybes (many1 configElem <* eof))
          <|> (spaces >> eof >> return [])
          <?> "config file"


configElem :: Parser (Maybe ArgBlock)
configElem =  (fmap Just configStAlone      )
          <|> (fmap Just configBlock        )
          <|> (commentLine >> return Nothing)

configStAlone :: Parser ArgBlock
configStAlone = (do
    q <- try $ qualifier <* char ':'
    spaces'
    arg <- configOption
    many1 newline

    return ArgBlock
        { abMatch = q
        , abArg   = [arg]
        }
    ) <?> "standalone config instruction"

configBlock :: Parser ArgBlock
configBlock = (do
    q <- try qualifier

    many1 newline
    args <- (spaces' >> configOption) `sepEndBy` newline
    many newline

    return ArgBlock
        { abMatch = q
        , abArg   = args
        }
    ) <?> "config instruction block"


commentLine :: Parser String
commentLine = (do
    try $ do
        optional spaces'
        char '#'

    s <- many $ noneOf "\n"
    many1 newline
    return s
    ) <?> "comment"


qualifier :: Parser ABMatch
qualifier =  prefSufQualifier
         <|> exactQualifier
         <?> "file qualifier"

prefSufQualifier :: Parser ABMatch
prefSufQualifier = (do
    prf <- try $ many (noneOf "\n*:") <* char '*'
    PrefSuf prf <$> many (noneOf "\n*:")
    ) <?> "prefix/suffix match file qualifier"

exactQualifier :: Parser ABMatch
exactQualifier =  (ExactName <$> try (many1 $ noneOf "\n*:"))
              <?> "exact match file qualifier"



configOption :: Parser Argument
configOption = (choice
    [ autocompAdd
    , autocompAddSelf
    , autocompOff
    , displayDotsOn
    , displayDotsOff
    , displayInvBGOn
    , displayInvBGOff
    , editorIndSet
    , editorJumpMAdd
    , editorJumpMDel
    , editorTabModeSpc
    , editorTabModeTab
    , editorTabModeAuto
    , fileEncodingSet
    , fileEncodingDef
    , fileLineEndUnix
    , fileLineEndWin
    , fileLineEndDef
    , generalHighlAdd
    , generalHighlDel
    , generalROOn
    , generalROOff
    , helpGeneral
    , helpConfig
    , helpKeybinds
    , helpVersion
    , langBracketAdd
    , langBracketDel
    , langCommBlkAdd
    , langCommBlkDel
    , langCommLineAdd
    , langCommLineDel
    , langEscapeSet
    , langEscapeOff
    , langKeywordAdd
    , langKeywordDel
    , langStrChrAdd
    , langStrChrDel
    , langStrMLAdd
    , langStrMLDel
    , langStrRegAdd
    , langStrRegDel
    , metaFailsafe
    , metaInclude
    , metaStateFile
    , otherOpenCfGlob
    , otherOpenCfLoc
    , otherPurgeOn
    , otherPurgeOff
    , debugDumpEvOn
    , debugDumpEvOff
    , debugStability
    ]) <?> "config option"


autocompOff       = try (string "-A"  ) >> return AutocompOff
displayDotsOn     = try (string "-db" ) >> return DisplayDotsOn
displayDotsOff    = try (string "-dB" ) >> return DisplayDotsOff
displayInvBGOn    = try (string "-dx" ) >> return DisplayInvBGOn
displayInvBGOff   = try (string "-dX" ) >> return DisplayInvBGOff
editorTabModeSpc  = try (string "-ets") >> return EditorTabModeSpc
editorTabModeTab  = try (string "-ett") >> return EditorTabModeTab
editorTabModeAuto = try (string "-eT" ) >> return EditorTabModeAuto
fileEncodingDef   = try (string "-fE" ) >> return FileEncodingDef
fileLineEndUnix   = try (string "-flu") >> return FileLineEndUnix
fileLineEndWin    = try (string "-flw") >> return FileLineEndWin
fileLineEndDef    = try (string "-fL" ) >> return FileLineEndDef
generalROOn       = try (string "-gr" ) >> return GeneralROOn
generalROOff      = try (string "-gR" ) >> return GeneralROOff
helpGeneral       = try (string "-h"  ) >> return HelpGeneral
helpConfig        = try (string "-hc" ) >> return HelpConfig
helpKeybinds      = try (string "-hk" ) >> return HelpKeybinds
helpVersion       = try (string "-hv" ) >> return HelpVersion
langEscapeOff     = try (string "-lE" ) >> return LangEscapeOff
metaFailsafe      = try (string "-mf" ) >> return MetaFailsafe
metaStateFile     = try (string "-ms" ) >> return MetaStateFile
otherOpenCfGlob   = try (string "-ocg") >> return OtherOpenCfGlob
otherOpenCfLoc    = try (string "-ocl") >> return OtherOpenCfLoc
otherPurgeOn      = try (string "-op" ) >> return OtherPurgeOn
otherPurgeOff     = try (string "-oP" ) >> return OtherPurgeOff
debugDumpEvOn     = try (string "-ye" ) >> return DebugDumpEvOn
debugDumpEvOff    = try (string "-yE" ) >> return DebugDumpEvOff

autocompAddSelf   = do { try $ string "-as" ; spaces'; AutocompAddSelf <$> integer                          }
editorIndSet      = do { try $ string "-ei" ; spaces'; EditorIndSet    <$> integer                          }
editorJumpMAdd    = do { try $ string "-ej" ; spaces'; EditorJumpMAdd  <$> integer                          }
editorJumpMDel    = do { try $ string "-eJ" ; spaces'; EditorJumpMDel  <$> integer                          }
fileEncodingSet   = do { try $ string "-fe" ; spaces'; FileEncodingSet <$> word                             }
generalHighlAdd   = do { try $ string "-gh" ; spaces'; GeneralHighlAdd <$> word                             }
generalHighlDel   = do { try $ string "-gH" ; spaces'; GeneralHighlDel <$> word                             }
langCommLineAdd   = do { try $ string "-lcl"; spaces'; LangCommLineAdd <$> word                             }
langCommLineDel   = do { try $ string "-lcL"; spaces'; LangCommLineDel <$> word                             }
langEscapeSet     = do { try $ string "-le" ; spaces'; LangEscapeSet   <$> singleChar                       }
langKeywordAdd    = do { try $ string "-lk" ; spaces'; LangKeywordAdd  <$> word                             }
langKeywordDel    = do { try $ string "-lK" ; spaces'; LangKeywordDel  <$> word                             }
metaInclude       = do { try $ string "-mi" ; spaces'; MetaInclude     <$> filePath                         }
debugStability    = do { try $ string "-ys" ; spaces'; DebugStability  <$> stab                             }

autocompAdd       = do { try $ string "-ad" ; spaces'; n <- integer; spaces'; AutocompAdd    n <$> filePath }
langBracketAdd    = do { try $ string "-lb" ; spaces'; s <- word   ; spaces'; LangBracketAdd s <$> word     }
langBracketDel    = do { try $ string "-lB" ; spaces'; s <- word   ; spaces'; LangBracketDel s <$> word     }
langCommBlkAdd    = do { try $ string "-lcb"; spaces'; s <- word   ; spaces'; LangCommBlkAdd s <$> word     }
langCommBlkDel    = do { try $ string "-lcB"; spaces'; s <- word   ; spaces'; LangCommBlkDel s <$> word     }
langStrChrAdd     = do { try $ string "-lsc"; spaces'; s <- word   ; spaces'; LangStrChrAdd  s <$> word     }
langStrChrDel     = do { try $ string "-lsC"; spaces'; s <- word   ; spaces'; LangStrChrDel  s <$> word     }
langStrMLAdd      = do { try $ string "-lsm"; spaces'; s <- word   ; spaces'; LangStrMLAdd   s <$> word     }
langStrMLDel      = do { try $ string "-lsM"; spaces'; s <- word   ; spaces'; LangStrMLDel   s <$> word     }
langStrRegAdd     = do { try $ string "-lsr"; spaces'; s <- word   ; spaces'; LangStrRegAdd  s <$> word     }
langStrRegDel     = do { try $ string "-lsR"; spaces'; s <- word   ; spaces'; LangStrRegDel  s <$> word     }

specialSetFile    = SpecialSetFile <$> filePath
specialSetVPos    = SpecialSetVPos <$> integer
specialSetHPos    = SpecialSetHPos <$> integer


stab :: Parser Stability
stab  = read <$> (  try (string "Prototype")
                <|> try (string "WIP"      )
                <|> try (string "RC"       )
                <|> try (string "Release"  )
                 )


singleChar =          try (      noneOf " \t\n") <?> "single character"
word       =          try (many1 singleChar    ) <?> "word"
spaces'    =          try (many1 $ oneOf " \t" ) <?> "whitespace"
integer    = read <$> try (many1 digit         ) <?> "integer"

filePath   = try (concat <$> sequence
    [ option "" $ string "/"
    , intercalate "/" <$> many1 (noneOf "\n/ ") `sepBy` char '/'
    , option "" $ string "/"
    ]) <?> "file path"
