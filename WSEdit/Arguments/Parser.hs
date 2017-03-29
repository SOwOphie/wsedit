{-# OPTIONS_GHC -fno-warn-missing-import-lists
                -fno-warn-missing-signatures
                -fno-warn-unused-do-bind
                #-}

module WSEdit.Arguments.Parser
    ( configCmd
    , configFile
    ) where


import Data.Maybe                    (catMaybes)
import Text.ParserCombinators.Parsec ( Parser
                                     , anyChar, char, choice, digit, endBy, eof
                                     , many, many1, newline, noneOf, oneOf
                                     , option, optional, sepBy, sepEndBy, spaces
                                     , string, try
                                     , (<|>), (<?>)
                                     )

import WSEdit.Arguments.Data         ( ArgBlock (ArgBlock, abMatch, abArg)
                                     , Argument (..)
                                        -- not listing those off one by one
                                     )
import WSEdit.Data                   ( FileMatch (ExactName, PrefSuf)
                                     , Stability ()
                                     )



-- | Parses all command-line arguments.
configCmd :: Parser [Argument]
configCmd = ( fmap concat
              $ try
              $ sequence
              [ option [] $                 configOption `endBy` spaces'
              ,                             fmap return (specialSetFile)
              , option [] $ try $ spaces' >> fmap return (specialSetVPos)
              , option [] $ try $ spaces' >> fmap return (specialSetHPos)
              , option [] $ try $ spaces' >> configOption `sepBy` spaces'
              ,                             eof >> return []
              ]
            ) <|> (
              configOption `sepBy` spaces' <* eof
            )



-- | Parses an entire config file.
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


qualifier :: Parser FileMatch
qualifier =  prefSufQualifier
         <|> exactQualifier
         <?> "file qualifier"

prefSufQualifier :: Parser FileMatch
prefSufQualifier = (do
    prf <- try $ many (noneOf "\n*:") <* char '*'
    PrefSuf prf <$> many (noneOf "\n*:")
    ) <?> "prefix/suffix match file qualifier"

exactQualifier :: Parser FileMatch
exactQualifier =  (ExactName <$> try (many1 $ noneOf "\n*:"))
              <?> "exact match file qualifier"



configOption :: Parser Argument
configOption = (choice
    [ autocompAdd
    , autocompAddSelf
    , autocompOff
    , displayBadgeSet
    , displayBadgeOff
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
    , fileAtomicOff
    , fileAtomicOn
    , fileEncodingSet
    , fileEncodingDef
    , fileLineEndUnix
    , fileLineEndWin
    , fileLineEndDef
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
displayInvBGOn    = try (string "-dx" ) >> return DisplayInvBGOn
displayInvBGOff   = try (string "-dX" ) >> return DisplayInvBGOff
editorTabModeSpc  = try (string "-ets") >> return EditorTabModeSpc
editorTabModeTab  = try (string "-ett") >> return EditorTabModeTab
editorTabModeAuto = try (string "-eT" ) >> return EditorTabModeAuto
fileAtomicOff     = try (string "-fa" ) >> return FileAtomicOff
fileAtomicOn      = try (string "-fA" ) >> return FileAtomicOn
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

autocompAddSelf   = do { try $ string "-as" ; spaces'; AutocompAddSelf <$> wildInt                           }
displayBadgeSet   = do { try $ string "-ds" ; spaces'; DisplayBadgeSet <$> word                              }
editorIndSet      = do { try $ string "-ei" ; spaces'; EditorIndSet    <$> integer                           }
editorJumpMAdd    = do { try $ string "-ej" ; spaces'; EditorJumpMAdd  <$> integer                           }
editorJumpMDel    = do { try $ string "-eJ" ; spaces'; EditorJumpMDel  <$> integer                           }
fileEncodingSet   = do { try $ string "-fe" ; spaces'; FileEncodingSet <$> word                              }
generalHighlAdd   = do { try $ string "-gh" ; spaces'; GeneralHighlAdd <$> word                              }
generalHighlDel   = do { try $ string "-gH" ; spaces'; GeneralHighlDel <$> word                              }
langCommLineAdd   = do { try $ string "-lcl"; spaces'; LangCommLineAdd <$> word                              }
langCommLineDel   = do { try $ string "-lcL"; spaces'; LangCommLineDel <$> word                              }
langEscOAdd       = do { try $ string "-leo"; spaces'; LangEscOAdd     <$> word                              }
langEscODel       = do { try $ string "-leO"; spaces'; LangEscODel     <$> word                              }
langEscSAdd       = do { try $ string "-les"; spaces'; LangEscSAdd     <$> word                              }
langEscSDel       = do { try $ string "-leS"; spaces'; LangEscSDel     <$> word                              }
langKeywordAdd    = do { try $ string "-lk" ; spaces'; LangKeywordAdd  <$> word                              }
langKeywordDel    = do { try $ string "-lK" ; spaces'; LangKeywordDel  <$> word                              }
metaInclude       = do { try $ string "-mi" ; spaces'; MetaInclude     <$> word                              }
debugStability    = do { try $ string "-ys" ; spaces'; DebugStability  <$> stab                              }

autocompAdd       = do { try $ string "-ad" ; spaces'; n <- wildInt; spaces'; AutocompAdd    n <$> qualifier }
langBracketAdd    = do { try $ string "-lb" ; spaces'; s <- word   ; spaces'; LangBracketAdd s <$> word      }
langBracketDel    = do { try $ string "-lB" ; spaces'; s <- word   ; spaces'; LangBracketDel s <$> word      }
langCommBlkAdd    = do { try $ string "-lcb"; spaces'; s <- word   ; spaces'; LangCommBlkAdd s <$> word      }
langCommBlkDel    = do { try $ string "-lcB"; spaces'; s <- word   ; spaces'; LangCommBlkDel s <$> word      }
langStrChrAdd     = do { try $ string "-lsc"; spaces'; s <- word   ; spaces'; LangStrChrAdd  s <$> word      }
langStrChrDel     = do { try $ string "-lsC"; spaces'; s <- word   ; spaces'; LangStrChrDel  s <$> word      }
langStrMLAdd      = do { try $ string "-lsm"; spaces'; s <- word   ; spaces'; LangStrMLAdd   s <$> word      }
langStrMLDel      = do { try $ string "-lsM"; spaces'; s <- word   ; spaces'; LangStrMLDel   s <$> word      }
langStrRegAdd     = do { try $ string "-lsr"; spaces'; s <- word   ; spaces'; LangStrRegAdd  s <$> word      }
langStrRegDel     = do { try $ string "-lsR"; spaces'; s <- word   ; spaces'; LangStrRegDel  s <$> word      }

specialSetFile    = SpecialSetFile <$> word
specialSetVPos    = SpecialSetVPos <$> integer
specialSetHPos    = SpecialSetHPos <$> integer


stab :: Parser Stability
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
escWord    = try (char '"') >> many1 (noneOf "\"\n") <* char '"'
