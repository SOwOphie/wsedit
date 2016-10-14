module WSEdit.Arguments
    (
    ) where


import Data.List                     (isPrefixOf, isSuffixOf)
import System.IO                     (FilePath)
import Text.ParserCombinators.Parsec ()



data ABMatch = ExactName String
             | PrefSuf   String String

data ArgBlock = ArgBlock
    { abMatch  :: ABMatch
    , abArg    :: [Argument]
    }



data Argument = AutocompAdd     Int    String
              | AutocompAddSelf Int
              | AutocompOff

              | DebugDumpEvOn
              | DebugDumpEvOff

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

    deriving (Eq, Read, Show)





configFile = do
    optional comment
    many $ configBlock <* optional comment

comment = do
    spaces'
    char '#'
    many $ noneOf newline
    newline
    optional comment

configBlock = do
    qualifier
    newline
    configOption `sepBy` newline


qualifier = try $ exactQualifier <|> prefSufQualifier

exactQualifier   = ExactName <$> many1 (noneOf "\n*:") <* char ':'

prefSufQualifier = do
    prf <- many1 (noneOf "\n*:")
    char '*'
    suf <- many1 (noneOf "\n*:")
    char ':'
    return $ PrefSuf prf suf


configOption = foldl (\el ls -> ls <|> try el) fail
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
    ]


autocompOff       = string "-A"   >> return AutocompOff
displayDotsOn     = string "-db"  >> return DisplayDotsOn
displayDotsOff    = string "-dB"  >> return DisplayDotsOff
displayInvBGOn    = string "-dx"  >> return DisplayInvBGOn
displayInvBGOff   = string "-dX"  >> return DisplayInvBGOff
editorTabModeSpc  = string "-ets" >> return EditorTabModeSpc
editorTabModeTab  = string "-ett" >> return EditorTabModeTab
editorTabModeAuto = string "-eT"  >> return EditorTabModeAuto
fileEncodingDef   = string "-fE"  >> return FileEncodingDef
fileLineEndUnix   = string "-flu" >> return FileLineEndUnix
fileLineEndWin    = string "-flw" >> return FileLineEndWin
fileLineEndDef    = string "-fL"  >> return FileLineEndDef
generalROOn       = string "-gr"  >> return GeneralROOn
generalROOff      = string "-gR"  >> return GeneralROOff
helpGeneral       = string "-h"   >> return HelpGeneral
helpConfig        = string "-hc"  >> return HelpConfig
helpKeybinds      = string "-hk"  >> return HelpKeybinds
helpVersion       = string "-hv"  >> return HelpVersion
langEscapeOff     = string "-lE"  >> return LangEscapeOff
metaFailsafe      = string "-mf"  >> return MetaFailsafe
metaStateFile     = string "-ms"  >> return MetaStateFile
otherOpenCfGlob   = string "-ocg" >> return OtherOpenCfGlob
otherOpenCfLoc    = string "-ocl" >> return OtherOpenCfLoc
otherPurgeOn      = string "-op"  >> return OtherPurgeOn
otherPurgeOff     = string "-oP"  >> return OtherPurgeOff
debugDumpEvOn     = string "-ye"  >> return DebugDumpEvOn
debugDumpEvOff    = string "-yE"  >> return DebugDumpEvOff

autocompAddSelf   = do { string "-as" ; spaces'; AutocompAddSelf <$> integer                          }
editorIndSet      = do { string "-ei" ; spaces'; EditorIndSet    <$> integer                          }
editorJumpMAdd    = do { string "-ej" ; spaces'; EditorJumpMAdd  <$> integer                          }
editorJumpMDel    = do { string "-eJ" ; spaces'; EditorJumpMDel  <$> integer                          }
fileEncodingSet   = do { string "-fe" ; spaces'; FileEncodingSet <$> word                             }
generalHighlAdd   = do { string "-gh" ; spaces'; GeneralHighlAdd <$> word                             }
generalHighlDel   = do { string "-gH" ; spaces'; GeneralHighlDel <$> word                             }
langCommLineAdd   = do { string "-lcl"; spaces'; LangCommLineAdd <$> word                             }
langCommLineDel   = do { string "-lcL"; spaces'; LangCommLineDel <$> word                             }
langEscapeSet     = do { string "-le" ; spaces'; LangEscapeSet   <$> singleChar                       }
langKeywordAdd    = do { string "-lk" ; spaces'; LangKeywordAdd  <$> word                             }
langKeywordDel    = do { string "-lK" ; spaces'; LangKeywordDel  <$> word                             }
metaInclude       = do { string "-mi" ; spaces'; MetaInclude     <$> filePath                         }

autocompAdd       = do { string "-ad" ; spaces'; n <- integer; spaces'; AutocompAdd    n <$> filePath }
langBracketAdd    = do { string "-lb" ; spaces'; s <- word   ; spaces'; LangBracketAdd s <$> word     }
langBracketDel    = do { string "-lB" ; spaces'; s <- word   ; spaces'; LangBracketDel s <$> word     }
langCommBlkAdd    = do { string "-lcb"; spaces'; s <- word   ; spaces'; LangCommBlkAdd s <$> word     }
langCommBlkDel    = do { string "-lcB"; spaces'; s <- word   ; spaces'; LangCommBlkDel s <$> word     }
langStrChrAdd     = do { string "-lsc"; spaces'; s <- word   ; spaces'; LangStrChrAdd  s <$> word     }
langStrChrDel     = do { string "-lsC"; spaces'; s <- word   ; spaces'; LangStrChrDel  s <$> word     }
langStrMLAdd      = do { string "-lsm"; spaces'; s <- word   ; spaces'; LangStrMLAdd   s <$> word     }
langStrMLDel      = do { string "-lsM"; spaces'; s <- word   ; spaces'; LangStrMLDel   s <$> word     }
langStrRegAdd     = do { string "-lsr"; spaces'; s <- word   ; spaces'; LangStrRegAdd  s <$> word     }
langStrRegDel     = do { string "-lsR"; spaces'; s <- word   ; spaces'; LangStrRegDel  s <$> word     }


singleChar = noneOf " \t\n"
word       = many1 singleChar
spaces'    = many1 $ oneOf " \t"
integer    = read <$> many1 digit
filePath   = undefined
