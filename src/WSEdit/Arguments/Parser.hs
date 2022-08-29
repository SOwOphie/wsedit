{-# OPTIONS_GHC -fno-warn-missing-import-lists
                -fno-warn-missing-signatures
                -fno-warn-unused-do-bind
                #-}

module WSEdit.Arguments.Parser
    ( configCmd
    , configFile
    , WSParser
    , qualifier
    , escWord
    ) where


import Control.Monad
    ( liftM2
    )
import Data.Maybe
    ( catMaybes
    )
import Text.Parsec
    ( Parsec
    , anyChar
    , char
    , choice
    , digit
    , endBy
    , eof
    , getState
    , many
    , many1
    , newline
    , noneOf
    , oneOf
    , option
    , optional
    , sepBy
    , sepEndBy
    , spaces
    , string
    , try
    , (<|>)
    , (<?>)
    )
import System.FilePath
    ( (</>)
    )

import WSEdit.Arguments.Data
    ( ArgBlockProto
        ( ArgBlockProto
        , abpMatch
        , abpArg
        )
    , Argument
        (..) -- not listing those off one by one
    , FileMatchProto
        ( FileQualifier
        , PathQualifier
        )
    )
import WSEdit.Data
    ( CanonicalPath
        ( getCanonicalPath
        )
    , Stability
        ()
    )



type WSParser = Parsec String CanonicalPath



-- | Parses all command-line arguments.
configCmd :: WSParser [Argument]
configCmd = ( fmap concat
              $ try
              $ sequence
              [ option [] $                  configOption `endBy` spaces'
              ,                              fmap return (specialSetFile)
              , option [] $ try $ spaces' >> fmap return (specialSetVPos)
              , option [] $ try $ spaces' >> fmap return (specialSetHPos)
              , option [] $ try $ spaces' >> configOption `sepBy` spaces'
              ,                              eof >> return []
              ]
            ) <|> (
              configOption `sepBy` spaces' <* eof
            )



-- | Parses an entire config file.
configFile :: WSParser [ArgBlockProto]
configFile =  (fmap catMaybes (many1 configElem <* eof))
          <|> (spaces >> eof >> return [])
          <?> "config file"


configElem :: WSParser (Maybe ArgBlockProto)
configElem =  (commentLine >> return Nothing)
          <|> (fmap Just configStAlone      )
          <|> (fmap Just configBlock        )

configStAlone :: WSParser ArgBlockProto
configStAlone = (do
    q <- try $ qualifier <* char ':'
    spaces'
    arg <- configOption
    many1 newline

    return ArgBlockProto
        { abpMatch = q
        , abpArg   = [arg]
        }
    ) <?> "standalone config instruction"

configBlock :: WSParser ArgBlockProto
configBlock = (do
    q <- try $ qualifier

    many1 newline
    args <- (spaces' >> configOption) `sepEndBy` newline
    many newline

    return ArgBlockProto
        { abpMatch = q
        , abpArg   = args
        }
    ) <?> "config instruction block"


commentLine :: WSParser String
commentLine = (do
    try $ do
        optional spaces'
        char '#'

    s <- many $ noneOf "\n"
    many1 newline
    return s
    ) <?> "comment"


qualifier :: WSParser FileMatchProto
qualifier = do
    s <- try $ many1 $ noneOf "\n:"
    p <- getState
    return $ if '/' `elem` s
                then PathQualifier p s
                else FileQualifier   s


pathName :: WSParser FilePath
pathName = liftM2 (</>) (getCanonicalPath <$> getState) word



configOption :: WSParser Argument
configOption = (choice
    [ autocompAdd
    , autocompAddSelf
    , autocompOff
    , displayBadgeSet
    , displayBadgeOff
    , displayColsOn
    , displayColsOff
    , displayDotsOn
    , displayDotsOff
    , displayInvBGOn
    , displayInvBGOff
    , displayThemeOn
    , displayThemeOff
    , editorIndSet
    , editorJumpMAdd
    , editorJumpMDel
    , editorElTabsOn
    , editorElTabsOff
    , editorPreserveOn
    , editorPreserveOff
    , editorTabModeSpc
    , editorTabModeTab
    , editorTabModeAuto
    , fileAtomicOff
    , fileAtomicOn
    , fileEncodingSet
    , fileEncodingDef
    , fileLineEndUnix
    , fileLineEndWin
    , fileLineEndDef
    , fileReadEncAuto
    , fileReadEncSet
    , fileReadEncDef
    , generalHighlAdd
    , generalHighlDel
    , generalROOn
    , generalROOff
    , helpConfig
    , helpKeybinds
    , helpVersion
    , helpGeneral
    , langBracketAdd
    , langBracketDel
    , langCommBlkAdd
    , langCommBlkDel
    , langCommLineAdd
    , langCommLineDel
    , langEscOAdd
    , langEscODel
    , langEscSAdd
    , langEscSDel
    , langIdChrAdd
    , langIdChrDel
    , langKeyPrfxAdd
    , langKeyPrfxDel
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
    , debugDumpArgs
    , debugDumpEvOn
    , debugDumpEvOff
    , debugWriOff
    , debugWriOn
    , debugStability
    ]) <?> "config option"


autocompOff       = try (string "-A"  ) >> return AutocompOff
displayBadgeOff   = try (string "-dS" ) >> return DisplayBadgeOff
displayDotsOn     = try (string "-db" ) >> return DisplayDotsOn
displayDotsOff    = try (string "-dB" ) >> return DisplayDotsOff
displayColsOn     = try (string "-dc" ) >> return DisplayColsOn
displayColsOff    = try (string "-dC" ) >> return DisplayColsOff
displayThemeOff   = try (string "-dT" ) >> return DisplayThemeOff
displayInvBGOn    = try (string "-dx" ) >> return DisplayInvBGOn
displayInvBGOff   = try (string "-dX" ) >> return DisplayInvBGOff
editorElTabsOn    = try (string "-el" ) >> return EditorElTabsOn
editorElTabsOff   = try (string "-eL" ) >> return EditorElTabsOff
editorPreserveOn  = try (string "-ep" ) >> return EditorPreserveOn
editorPreserveOff = try (string "-eP" ) >> return EditorPreserveOff
editorTabModeSpc  = try (string "-ets") >> return EditorTabModeSpc
editorTabModeTab  = try (string "-ett") >> return EditorTabModeTab
editorTabModeAuto = try (string "-eT" ) >> return EditorTabModeAuto
fileAtomicOff     = try (string "-fa" ) >> return FileAtomicOff
fileAtomicOn      = try (string "-fA" ) >> return FileAtomicOn
fileEncodingDef   = try (string "-fE" ) >> return FileEncodingDef
fileLineEndUnix   = try (string "-flu") >> return FileLineEndUnix
fileLineEndWin    = try (string "-flw") >> return FileLineEndWin
fileLineEndDef    = try (string "-fL" ) >> return FileLineEndDef
fileReadEncAuto   = try (string "-fra") >> return FileReadEncAuto
fileReadEncDef    = try (string "-fR" ) >> return FileReadEncDef
generalROOn       = try (string "-gr" ) >> return GeneralROOn
generalROOff      = try (string "-gR" ) >> return GeneralROOff
helpGeneral       = try (string "-h"  ) >> return HelpGeneral
helpConfig        = try (string "-hc" ) >> return HelpConfig
helpKeybinds      = try (string "-hk" ) >> return HelpKeybinds
helpVersion       = try (string "-hv" ) >> return HelpVersion
metaFailsafe      = try (string "-mf" ) >> return MetaFailsafe
metaStateFile     = try (string "-ms" ) >> return MetaStateFile
otherOpenCfGlob   = try (string "-ocg") >> return OtherOpenCfGlob
otherOpenCfLoc    = try (string "-ocl") >> return OtherOpenCfLoc
otherPurgeOn      = try (string "-op" ) >> return OtherPurgeOn
otherPurgeOff     = try (string "-oP" ) >> return OtherPurgeOff
debugDumpArgs     = try (string "-yc" ) >> return DebugDumpArgs
debugDumpEvOn     = try (string "-ye" ) >> return DebugDumpEvOn
debugDumpEvOff    = try (string "-yE" ) >> return DebugDumpEvOff
debugWriOff       = try (string "-yi" ) >> return DebugWRIOff
debugWriOn        = try (string "-yI" ) >> return DebugWRIOn

autocompAddSelf   = do { try $ string "-as" ; spaces'; AutocompAddSelf  <$> wildInt    }
displayBadgeSet   = do { try $ string "-ds" ; spaces'; DisplayBadgeSet  <$> word       }
displayThemeOn    = do { try $ string "-dt" ; spaces'; DisplayThemeOn <$> word       }
editorIndSet      = do { try $ string "-ei" ; spaces'; EditorIndSet     <$> integer    }
editorJumpMAdd    = do { try $ string "-ej" ; spaces'; EditorJumpMAdd   <$> integer    }
editorJumpMDel    = do { try $ string "-eJ" ; spaces'; EditorJumpMDel   <$> integer    }
fileEncodingSet   = do { try $ string "-fe" ; spaces'; FileEncodingSet  <$> word       }
fileReadEncSet    = do { try $ string "-fr" ; spaces'; FileReadEncSet   <$> word       }
generalHighlAdd   = do { try $ string "-gh" ; spaces'; GeneralHighlAdd  <$> word       }
generalHighlDel   = do { try $ string "-gH" ; spaces'; GeneralHighlDel  <$> word       }
langCommLineAdd   = do { try $ string "-lcl"; spaces'; LangCommLineAdd  <$> word       }
langCommLineDel   = do { try $ string "-lcL"; spaces'; LangCommLineDel  <$> word       }
langEscOAdd       = do { try $ string "-leo"; spaces'; LangEscOAdd      <$> word       }
langEscODel       = do { try $ string "-leO"; spaces'; LangEscODel      <$> word       }
langEscSAdd       = do { try $ string "-les"; spaces'; LangEscSAdd      <$> word       }
langEscSDel       = do { try $ string "-leS"; spaces'; LangEscSDel      <$> word       }
langIdChrAdd      = do { try $ string "-li" ; spaces'; LangIdChrAdd     <$> simpleChar }
langIdChrDel      = do { try $ string "-lI" ; spaces'; LangIdChrDel     <$> simpleChar }
langKeywordAdd    = do { try $ string "-lk" ; spaces'; LangKeywordAdd   <$> word       }
langKeywordDel    = do { try $ string "-lK" ; spaces'; LangKeywordDel   <$> word       }
langKeyPrfxAdd    = do { try $ string "-lkp"; spaces'; LangKeyPrfxAdd   <$> word       }
langKeyPrfxDel    = do { try $ string "-lkP"; spaces'; LangKeyPrfxDel   <$> word       }
metaInclude       = do { try $ string "-mi" ; spaces'; MetaInclude      <$> pathName   }
debugStability    = do { try $ string "-ys" ; spaces'; DebugStability   <$> stab       }

autocompAdd       = do { try $ string "-ad" ; spaces'; n <- wildInt; spaces'; AutocompAdd    n <$> qualifier}
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

specialSetFile    = SpecialSetFile <$> word
specialSetVPos    = SpecialSetVPos <$> integer
specialSetHPos    = SpecialSetHPos <$> integer


stab :: WSParser Stability
stab  = read <$> (  try (string "Prototype")
                <|> try (string "WIP"      )
                <|> try (string "RC"       )
                <|> try (string "Release"  )
                 )


singleChar = escChar <|> simpleChar <?> "single character"
escChar    = try (char '\\') >> anyChar
simpleChar = noneOf " \\\t\n\""


spaces'    =          try (many1 $ oneOf " \t"  ) <?> "whitespace"
integer    = read <$> try (many1 digit          ) <?> "integer"

wildInt    =  try (char '*' >> return Nothing)
          <|> fmap Just integer


word       = escWord <|> simpleWord <?> "word"
simpleWord = many1 singleChar
escWord    = try (char '"') >> many1 (escChar <|> noneOf "\"\n") <* char '"'
