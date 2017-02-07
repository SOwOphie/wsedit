module WSEdit.Arguments.Data
    ( ArgBlock (..)
    , Argument (..)
    ) where


import WSEdit.Data ( FileMatch ()
                   , Stability ()
                   )



-- | Block of arguments that share a common file selector.
data ArgBlock = ArgBlock
    { abMatch  :: FileMatch
    , abArg    :: [Argument]
    }
    deriving (Eq, Read, Show)



-- | Argument type.
data Argument = AutocompAdd     (Maybe Int) FileMatch
              | AutocompAddSelf (Maybe Int)
              | AutocompOff

              | DebugDumpArgs
              | DebugDumpEvOn
              | DebugDumpEvOff
              | DebugWRIOff
              | DebugWRIOn
              | DebugStability  Stability

              | DisplayBadgeSet String
              | DisplayBadgeOff
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

              | FileAtomicOff
              | FileAtomicOn
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
              | LangEscOSet     Char
              | LangEscOOff
              | LangEscSSet     Char
              | LangEscSOff
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
