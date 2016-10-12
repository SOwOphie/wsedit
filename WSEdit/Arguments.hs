module WSEdit.Arguments
    (
    ) where


import Data.List   (isPrefixOf, isSuffixOf)
import System.IO   (FilePath)

import WSEdit.Util (Tri(..))



data ArgumentLine = ArgumentLine
    { alFilePrefix :: String
    , alFileSuffix :: String
    , alArg        :: Argument
    }

appliesTo :: FilePath -> FilePath -> ArgumentLine -> Bool
appliesTo absP relP (ArgumentLine { alFilePrefix = []    , alFileSuffix = s })
    = s `isSuffixOf` relP

appliesTo absP relP (ArgumentLine { alFilePrefix = (p:ps), alFileSuffix = s })
    | p == '/' && (p:ps) `isPrefixOf` absP = s `isSuffixOf` absP
    | p /= '/' && (p:ps) `isPrefixOf` relP = s `isSuffixOf` relP
    | otherwise                            = False



data ArgumentSource = SParameter Int
                    | SFile String Int



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





parseArguments :: (EdConfig, EdState) -> [String] -> [String] -> (EdConfig, EdState)
parseArguments (conf st) files args =





parseFileLine :: String -> Maybe ArgumentLine
parseFileLine src ln =
    case ("*" `findInStr` ln, ":" `findInStr` ln) of
         ((s:_), (c:_)) -> parseInstruction (drop (c + 1) ln)
                       >>= \a -> ArgumentLine
                                    { alPrefix =                take s ln
                                    , alSuffix = drop (s + 1) $ take c ln
                                    , alArg    = a
                                    }

         ([]   , []   ) -> parseInstruction ln
                       >>= \a -> ArgumentLine
                                    { alPrefix = ""
                                    , alSuffix = ""
                                    , alArg    = a
                                    }

         (_    , _    ) -> Nothing



parseInstructions :: [String] -> [Either String Argument]
parseInstructions []             = []
parseInstructions ("-ad" :n:f:r) = case readMay n of
                                    Nothing -> Left   "-ad"               : parseInstructions r
                                    Just n' -> Right (AutocompAdd n' f  ) : parseInstructions r

parseInstructions ("-as" :n  :r) = case readMay n of
                                    Nothing -> Left   "-as"               : parseInstructions r
                                    Just n' -> Right (AutocompAddSelf n') : parseInstructions r

parseInstructions ("-A"      :r) =             Right  AutocompOff         : parseInstructions r
parseInstructions ("-db"     :r) =             Right  DisplayDotsOn       : parseInstructions r
parseInstructions ("-dB"     :r) =             Right  DisplayDotsOff      : parseInstructions r
parseInstructions ("-dx"     :r) =             Right  DisplayInvBGOn      : parseInstructions r
parseInstructions ("-dX"     :r) =             Right  DisplayInvBGOff     : parseInstructions r
parseInstructions ("-ei" :n  :r) = case readMay n of
                                    Nothing -> Left   "-ei"               : parseInstructions r
                                    Just n' -> Right (EditorIndSet n'   ) : parseInstructions r

parseInstructions ("-ej" :n  :r) = case readMay n of
                                    Nothing -> Left   "-ej"               : parseInstructions r
                                    Just n' -> Right (EditorJumpMAdd n' ) : parseInstructions r

parseInstructions ("-eJ" :n  :r) = case readMay n of
                                    Nothing -> Left   "-eJ"               : parseInstructions r
                                    Just n' -> Right (EditorJumpMDel n' ) : parseInstructions r

parseInstructions ("-ets"    :r) =             Right  EditorTabModeSpc    : parseInstructions r
parseInstructions ("-ett"    :r) =             Right  EditorTabModeTab    : parseInstructions r
parseInstructions ("-eT"     :r) =             Right  EditorTabModeAuto
parseInstructions ("-fe" :s  :r) =             Right (FileEncodingSet s ) : parseInstructions r
parseInstructions ("-fE"     :r) =             Right  FileEncodingDef     : parseInstructions r
parseInstructions ("-flu"    :r) =             Right  FileLineEndUnix     : parseInstructions r
parseInstructions ("-flw"    :r) =             Right  FileLineEndWin      : parseInstructions r
parseInstructions ("-fL"     :r) =             Right  FileLineEndDef      : parseInstructions r
parseInstructions ("-gh" :s  :r) =             Right (GeneralHighlAdd s ) : parseInstructions r
parseInstructions ("-gH" :s  :r) =             Right (GeneralHighlDel s ) : parseInstructions r
parseInstructions ("-gr"     :r) =             Right  GeneralROOn         : parseInstructions r
parseInstructions ("-gR"     :r) =             Right  GeneralROOff        : parseInstructions r
parseInstructions ("-h"      :r) =             Right  HelpGeneral         : parseInstructions r
parseInstructions ("-hc"     :r) =             Right  HelpConfig          : parseInstructions r
parseInstructions ("-hk"     :r) =             Right  HelpKeybinds        : parseInstructions r
parseInstructions ("-V"      :r) =             Right  HelpVersion         : parseInstructions r
parseInstructions ("-lb" :s:e:r) =             Right (LangBracketAdd s e) : parseInstructions r
parseInstructions ("-lB" :s:e:r) =             Right (LangBracketDel s e) : parseInstructions r
parseInstructions ("-lcb":s:e:r) =             Right (LangCommBlkAdd s e) : parseInstructions r
parseInstructions ("-lcB":s:e:r) =             Right (LangCommBlkDel s e) : parseInstructions r
parseInstructions ("-lcl":s  :r) =             Right (LangCommLineAdd s ) : parseInstructions r
parseInstructions ("-lcL":s  :r) =             Right (LangCommLineDel s ) : parseInstructions r
parseInstructions ("-le" :s  :r) =             Right (LangEscapeSet   s ) : parseInstructions r
parseInstructions ("-lE"     :r) =             Right  LangEscapeOff       : parseInstructions r
parseInstructions ("-lk" :s  :r) =             Right (LangKeywordAdd s  ) : parseInstructions r
parseInstructions ("-lK" :s  :r) =             Right (LangKeywordDel s  ) : parseInstructions r
parseInstructions ("-lsc":s:e:r) =             Right (LangStrChrAdd  s e) : parseInstructions r
parseInstructions ("-lsC":s:e:r) =             Right (LangStrChrDel  s e) : parseInstructions r
parseInstructions ("-lsm":s:e:r) =             Right (LangStrMLAdd   s e) : parseInstructions r
parseInstructions ("-lsM":s:e:r) =             Right (LangStrMLDel   s e) : parseInstructions r
parseInstructions ("-lsr":s:e:r) =             Right (LangStrRegAdd  s e) : parseInstructions r
parseInstructions ("-lsR":s:e:r) =             Right (LangStrRegDel  s e) : parseInstructions r
parseInstructions ("-mf"     :r) =             Right  MetaFailsafe        : parseInstructions r
parseInstructions ("-mi" :s  :r) =             Right (MetaInclude    s  ) : parseInstructions r
parseInstructions ("-ms"     :r) =             Right  MetaStateFile       : parseInstructions r
parseInstructions ("-ocg"    :r) =             Right  OtherOpenCfGlob     : parseInstructions r
parseInstructions ("-ocl"    :r) =             Right  OtherOpenCfLoc      : parseInstructions r
parseInstructions ("-op"     :r) =             Right  OtherPurgeOn        : parseInstructions r
parseInstructions ("-oP"     :r) =             Right  OtherPurgeOff       : parseInstructions r
parseInstructions ("-ye"     :r) =             Right  DebugDumpEvOn       : parseInstructions r
parseInstructions ("-yE"     :r) =             Right  DebugDumpEvOff      : parseInstructions r
parseInstructions (s         :r) =             Left   s                   : parseInstructions r
