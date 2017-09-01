module WSEdit.Arguments.Data
    ( ArgBlock
        (..)
    , ArgBlockProto
        (..)
    , Argument
        (..)
    , FileMatchProto
        (..)
    , unProtoFM
    , unProtoAB
    ) where


import WSEdit.Data
    ( CanonicalPath
        ()
    , FileMatch
        ( MatchFilename
        , MatchPath
        )
    , Stability
        ()
    )
import WSEdit.Data.Algorithms
    ( canonicalPath
    )



-- | Block of arguments that share a common file selector.
data ArgBlock = ArgBlock
    { abMatch  :: FileMatch
    , abArg    :: [Argument]
    }
    deriving (Eq, Read, Show)

-- | ArgBlock prototype. Can be transformed to an `ArgBlock` using IO.
data ArgBlockProto = ArgBlockProto
    { abpMatch :: FileMatchProto
    , abpArg   :: [Argument]
    }
    deriving (Eq, Read, Show)



-- | File match prototype. Can be transformed to a `FileMatch` using IO.
data FileMatchProto = FileQualifier FilePath
                        -- ^ Only match file name
                    | PathQualifier CanonicalPath FilePath
                        -- ^ Match full path #2 relative to base #1
    deriving (Eq, Read, Show)


unProtoFM :: FileMatchProto -> IO FileMatch
unProtoFM (FileQualifier      f) =  return $ MatchFilename f
unProtoFM (PathQualifier base f) =  MatchPath
                                <$> canonicalPath (Just base) f

unProtoAB :: ArgBlockProto -> IO ArgBlock
unProtoAB abp = do
    fm <- unProtoFM $ abpMatch abp
    return ArgBlock
        { abMatch = fm
        , abArg   = abpArg abp
        }



-- | Argument type.
data Argument = AutocompAdd     (Maybe Int) FileMatchProto
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
              | EditorElTabsOn
              | EditorElTabsOff
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
              | LangEscOAdd     String
              | LangEscODel     String
              | LangEscSAdd     String
              | LangEscSDel     String
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
