{-# LANGUAGE FlexibleInstances
           , TypeSynonymInstances
           #-}

module WSEdit.Data
    ( version
    , upstream
    , licenseVersion
    , Stability
        (..)
    , stability
    , FmtParserState
        (..)
    , BracketStack
    , RangeCacheElem
    , RangeCache
    , BracketCacheElem
    , BracketCache
    , EdState
        (..)
    , EdConfig
        (..)
    , mkDefConfig
    , EdDesign
        (..)
    , brightTheme
    , WSEdit
    , runWSEdit
    , runIn
    , Keymap
    , HighlightMode
        (..)
    , CanonicalPath
        (..)
    , FileMatch
        (..)
    ) where


import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.RWS.Strict
    ( RWST
    , runRWST
    )
import Data.Default
    ( Default
        ( def
        )
    )
import Graphics.Vty
    ( Attr
    , Event
        ()
    , Vty
        ()
    , black
    , blue
    , bold
    , brightBlack
    , brightGreen
    , brightMagenta
    , brightRed
    , brightWhite
    , brightYellow
    , cyan
    , green
    , defAttr
    , green
    , magenta
    , red
    , reverseVideo
    , underline
    , white
    , withBackColor
    , withForeColor
    , withStyle
    , yellow
    )
import System.IO
    ( NewlineMode
    , universalNewlineMode
    )

import WSEdit.Util
    ( CharClass
        ( Bracket
        , Digit
        , Lower
        , Operator
        , Special
        , Unprintable
        , Upper
        , Whitesp
        )
    )
import WSEdit.WordTree
    ( WordTree
    , empty
    )

import qualified WSEdit.Buffer as B





-- | Version number constant.
version :: String
version = "1.2.2.8"

-- | Upstream URL.
upstream :: String
upstream = "https://github.com/SirBoonami/wsedit"

-- | License version number constant.
licenseVersion :: String
licenseVersion = "2.0"

-- | Version stability
data Stability = Prototype
               | WIP
               | RC
               | Release
    deriving (Eq, Ord, Read, Show)

-- | Current release stability
stability :: Stability
stability = Prototype




-- | Format token parser state.
data FmtParserState = PNothing                  -- ^ Default state, nothing going on
                    | PLnString Int String      -- ^ Inside line string: start pos, closing string
                    | PMLString Int String      -- ^ Inside multi-line string: start pos, closing string
                    | PBComment Int String      -- ^ Inside block comment: start pos, closing string
    deriving (Eq, Read, Show)

-- | Stack of currently opened brackets. 2d position, closing string.
type BracketStack = [((Int, Int), String)]



-- | Stores ranges of highlighted areas, as well as the parser's stack at the
--   end of each line.
type RangeCacheElem = ([((Int, Int), HighlightMode)], FmtParserState)

-- | Only built from the start of the file to the end of the viewport, lines are
--   stored in reverse order.
type RangeCache     = [RangeCacheElem]



-- | Stores bracketed ranges, as well as the parser's stack at the end of each
--   line. Every backet pair ends up in the last line it touches.
type BracketCacheElem = ([((Int, Int), (Int, Int))], BracketStack)

-- | Only built from the start of the file to the end of the viewport, lines are
--   stored in reverse order.
type BracketCache     = [BracketCacheElem]





-- | Editor state container (dynamic part).
data EdState = EdState
    { edLines      :: B.Buffer (Bool, String)
        -- ^ Buffer of lines. Contains the line string and whether the line is
        --   tagged by a jump mark.

    , fname        :: FilePath
        -- ^ Path of the current file.

    , readOnly     :: Bool
        -- ^ Whether the file is opened in read only mode. Has no relation to
        --   the write permissions on the actual file.


    , tokenCache   :: B.Buffer [(Int, String)]
        -- ^ Stores relevant tokens alongside their starting position for each
        --   line.

    , rangeCache   :: RangeCache
        -- ^ See the description of 'RangeCache' for more information.

    , bracketCache :: BracketCache
        -- ^ See the description of 'BracketCache' for more information.

    , fullRebdReq  :: Bool
        -- ^ Gets set when a full cache rebuild is required.


    , cursorPos    :: Int
        -- ^ 1-based offset from the left end of the current line in characters.

    , loadPos      :: (Int, Int)
        -- ^ Where to place the cursor when loading the file.

    , wantsPos     :: Maybe Int
        -- ^ Target visual position (1-based offset in columns) of the cursor.
        --   Used to implement the cursor vertically moving over empty lines
        --   without resetting to column 1.  (It's hard to explain, see
        --   'WSEdit.Control.Base.moveCursor'.)

    , markPos      :: Maybe (Int, Int)
        -- ^ Selection mark position.

    , scrollOffset :: (Int, Int)
        -- ^ Viewport offset, 0-based.


    , continue     :: Bool
        -- ^ Whether the main loop should continue past this iteration.

    , exitMsg      :: Maybe String
        -- ^ Optional message to print after exiting using @continue = False@.

    , status       :: String
        -- ^ Status string displayed at the bottom.

    , badgeText    :: Maybe String
        -- ^ Whether to overlay a badge in the top right corner.

    , lastEvent    :: Maybe Event
        -- ^ Last recorded input event.


    , buildDict    :: [(Maybe FileMatch, Maybe Int)]
        -- ^ File match and indentation depth pairs for dictionary building.
        --   'Nothing' stands for the current file or all depths.

    , canComplete  :: Bool
        -- ^ Whether the autocomplete function can be invoked at this moment
        --   (usually 'True' while the user is typing and 'False' while he's
        --   scrolling).

    , replaceTabs  :: Bool
        -- ^ Whether to insert spaces instead of tabs. Has no effect on existing
        --   indentation.

    , detectTabs   :: Bool
        -- ^ Whether to autodetect the 'replaceTabs' setting on each load based
        --   on the file's existing indentation.

    , overwrite    :: Bool
        -- ^ Whether overwrite mode is on.

    , searchTerms  :: [String]
        -- ^ List of search terms to highlight


    , changed      :: Bool
        -- ^ Whether the file has been changed since the last load/save.

    , history      :: Maybe EdState
        -- ^ Editor state prior to the last action, used to implement undo
        --   facilities.  Horrible memory efficiency, but it seems to work.

    , dict         :: WordTree
        -- ^ Autocompletion dictionary.
    }
    deriving (Eq, Read, Show)

instance Default EdState where
    def = EdState
        { edLines      = B.singleton (False, "")
        , fname        = ""
        , readOnly     = False

        , tokenCache   = B.singleton []
        , rangeCache   = []
        , bracketCache = []
        , fullRebdReq  = False

        , cursorPos    = 1
        , loadPos      = (1, 1)
        , wantsPos     = Nothing
        , markPos      = Nothing
        , scrollOffset = (0, 0)

        , continue     = True
        , exitMsg      = Nothing
        , status       = ""
#ifndef dev
        , badgeText    = Nothing
#else
        , badgeText    = Just "!!! DEVELOPMENT BUILD !!!"
#endif
        , lastEvent    = Nothing

        , buildDict    = []
        , canComplete  = False
        , replaceTabs  = False
        , detectTabs   = True
        , overwrite    = False
        , searchTerms  = []

        , changed      = False
        , history      = Nothing
        , dict         = empty
        }





-- | Editor configuration container (static part).
data EdConfig = EdConfig
    { vtyObj       :: Vty
        -- ^ vty object container, used to issue draw calls and receive events.

    , edDesign     :: EdDesign
        -- ^ Design object, see below.

    , keymap       :: Keymap
        -- ^ What to do when a button is pressed. Inserting a character when the
        --   corresponding key is pressed (e.g. 'a') is not included here, but
        --   may be overridden with this table. (Why would you want to do that?)

    , histSize     :: Int
        -- ^ Number of undo states to keep.

    , tabWidth     :: Int
        -- ^ Width of a tab character.

    , drawBg       :: Bool
        -- ^ Whether or not to draw the background.

    , dumpEvents   :: Bool
        -- ^ Whether or not to dump every received event to the status line.

    , atomicSaves  :: Bool
        -- ^ Whether to perform the write-read identity check on save.

    , wriCheck     :: Bool
        -- ^ Whether to perform the write-read identity check on save.

    , purgeOnClose :: Bool
        -- ^ Whether the clipboard file is to be deleted on close.

    , initJMarks   :: [Int]
        -- ^ Where to put jump marks on load.


    , newlineMode  :: NewlineMode
        -- ^ Newline conversion to use.

    , encoding     :: Maybe String
        -- ^ Name of the file encoding to use.


    , lineComment  :: [String]
        -- ^ List of strings that mark the beginning of a comment.

    , blockComment :: [(String, String)]
        -- ^ List of block comment delimiters.

    , strDelim     :: [(String, String)]
        -- ^ List of string delimiters.

    , mStrDelim    :: [(String, String)]
        -- ^ List of multi-line string delimiters.

    , chrDelim     :: [(String, String)]
        -- ^ List of char delimiters

    , keywords     :: [String]
        -- ^ List of keywords to highlight.

    , escapeO      :: [String]
        -- ^ Escape character outside strings.

    , escapeS      :: [String]
        -- ^ Escape character for strings.

    , brackets     :: [(String, String)]
        -- ^ List of bracket pairs.
    }

-- | Create a default `EdConfig`.
mkDefConfig :: Vty -> Keymap -> EdConfig
mkDefConfig v k = EdConfig
                { vtyObj       = v
                , edDesign     = def
                , keymap       = k
                , histSize     = 100
                , tabWidth     = 4
                , drawBg       = False
                , dumpEvents   = False
                , atomicSaves  = True
                , wriCheck     = True
                , purgeOnClose = False
                , initJMarks   = []
                , newlineMode  = universalNewlineMode
                , encoding     = Nothing
                , lineComment  = []
                , blockComment = []
                , strDelim     = []
                , mStrDelim    = []
                , chrDelim     = []
                , keywords     = []
                , escapeO      = []
                , escapeS      = []
                , brackets     = []
              }





-- | Design portion of the editor configuration.
data EdDesign = EdDesign
    { dFrameFormat   :: Attr
        -- ^ vty attribute for the frame lines

    , dStatusFormat  :: Attr
        -- ^ vty attribute for the status line


    , dLineNoFormat  :: Attr
        -- ^ vty attribute for the line numbers to the left

    , dLineNoInterv  :: Int
        -- ^ Display interval for the line numbers


    , dColNoInterval :: Int
        -- ^ Display interval for the column numbers. Don't set this lower than
        --   the expected number's length, or strange things might happen.

    , dColNoFormat   :: Attr
        -- ^ vty attribute for the column numbers


    , dBGChar        :: Char
        -- ^ Character to fill the background with

    , dColChar       :: Maybe Char
        -- ^ Character to draw column lines with

    , dBGFormat      :: Attr
        -- ^ vty attribute for everything in the background


    , dCurrLnMod     :: Attr
        -- ^ Attribute modifications to apply to the current line

    , dBrMod         :: Attr
        -- ^ Attribute modifications for bracket matching.

    , dJumpMarkFmt   :: Attr
        -- ^ vty attribute for jump marks


    , dTabStr        :: String
        -- ^ String to display tab characters as.  Will get truncated from the
        --   left as needed.

    , dTabExt        :: Char
        -- ^ If 'dTabStr' is too short, this will be used to pad it to the
        --   required length.


    , dCharStyles    :: [(CharClass, Attr)]
        -- ^ vty attributes list for the different character classes

    , dHLStyles      :: [(HighlightMode, Attr)]
        -- ^ vty attributes list for the different highlight modes
    }


instance Default EdDesign where
    def = EdDesign
        { dFrameFormat   = defAttr
                            `withForeColor` green

        , dStatusFormat  = defAttr
                            `withForeColor` brightGreen
                            `withStyle`     bold

        , dLineNoFormat  = defAttr
                            `withForeColor` brightGreen
                            `withStyle`     bold
        , dLineNoInterv  = 10

        , dColNoInterval = 40
        , dColNoFormat   = defAttr
                            `withForeColor` brightGreen
                            `withStyle`     bold

        , dBGChar        = '·'
        , dColChar       = Just '|'
        , dBGFormat      = defAttr
                            `withForeColor` black

        , dCurrLnMod     = defAttr
                            `withBackColor` black

        , dBrMod         = defAttr
                            `withStyle`     reverseVideo

        , dJumpMarkFmt   = defAttr
                            `withForeColor` red

        , dTabStr        = "|"
        , dTabExt        = ' '

        , dCharStyles    =
            [ (Whitesp    , defAttr
                            `withForeColor` blue
              )
            , (Digit      , defAttr
                            `withForeColor` red
              )
            , (Lower      , defAttr
              )
            , (Upper      , defAttr
              )
            , (Bracket    , defAttr
                            `withForeColor` yellow
              )
            , (Operator   , defAttr
                            `withForeColor` brightYellow
                            `withStyle`     bold
              )
            , (Unprintable, defAttr
                            `withForeColor` magenta
                            `withStyle`     bold
              )
            , (Special    , defAttr
                            `withForeColor` magenta
              )
            ]

        , dHLStyles      =
            [ (HComment , defAttr
                            `withForeColor` brightMagenta
                            `withStyle`     bold
              )
            , (HError   , defAttr
                            `withBackColor` brightRed
                            `withStyle`     bold
              )
            , (HKeyword , defAttr
                            `withForeColor` green
              )
            , (HSearch  , defAttr
                            `withForeColor` brightRed
                            `withStyle`     bold
              )
            , (HSelected, defAttr
                            `withForeColor` brightBlack
                            `withBackColor` white
              )
            , (HString  , defAttr
                            `withForeColor` cyan
              )
            ]

        }





-- | Alternate theme for terminals with bright backgrounds.
brightTheme:: EdDesign
brightTheme = EdDesign
        { dFrameFormat   = defAttr
                            `withForeColor` green

        , dStatusFormat  = defAttr
                            `withForeColor` brightGreen
                            `withStyle`     bold

        , dLineNoFormat  = defAttr
                            `withForeColor` brightGreen
                            `withStyle`     bold
        , dLineNoInterv  = 10

        , dColNoInterval = 40
        , dColNoFormat   = defAttr
                            `withForeColor` brightGreen
                            `withStyle`     bold

        , dBGChar        = '·'
        , dColChar       = Just '|'
        , dBGFormat      = defAttr
                            `withForeColor` white

        , dCurrLnMod     = defAttr
                            `withBackColor` white

        , dBrMod         = defAttr
                            `withStyle`     reverseVideo

        , dJumpMarkFmt   = defAttr
                            `withForeColor` red

        , dTabStr        = "|"
        , dTabExt        = ' '

        , dCharStyles    =
            [ (Whitesp    , defAttr
                            `withForeColor` blue
              )
            , (Digit      , defAttr
                            `withForeColor` red
              )
            , (Lower      , defAttr
              )
            , (Upper      , defAttr
              )
            , (Bracket    , defAttr
                            `withForeColor` yellow
              )
            , (Operator   , defAttr
                            `withForeColor` brightYellow
                            `withStyle`     bold
              )
            , (Unprintable, defAttr
                            `withForeColor` magenta
                            `withStyle`     bold
              )
            , (Special    , defAttr
                            `withForeColor` magenta
              )
            ]

        , dHLStyles      =
            [ (HBracket , defAttr
                            `withStyle` underline
              )
            , (HComment , defAttr
                            `withForeColor` brightMagenta
                            `withStyle`     bold
              )
            , (HError   , defAttr
                            `withBackColor` brightRed
                            `withStyle`     bold
              )
            , (HKeyword , defAttr
                            `withForeColor` green
              )
            , (HSearch  , defAttr
                            `withForeColor` brightRed
                            `withStyle`     bold
              )
            , (HSelected, defAttr
                            `withForeColor` brightWhite
                            `withBackColor` black
              )
            , (HString  , defAttr
                            `withForeColor` cyan
              )
            ]

        }



-- | Editor monad. Reads an `EdConfig`, writes nothing, alters an `EdState`.
type WSEdit = RWST EdConfig () EdState IO

-- | Convenience shortcut to run `WSEdit` actions in `IO`.
runWSEdit :: (EdConfig, EdState) -> WSEdit a -> IO a
runWSEdit (c, s) a = runRWST a c s >>= \(r, _, _) -> return r

-- | Run a `WSEdit` action in a different, isolated context. All changes made to
--   `EdState` are discarded.
runIn :: (EdConfig, EdState) -> WSEdit a -> WSEdit a
runIn env = liftIO . runWSEdit env



-- | Map of events to actions (and their descriptions). 'Nothing's are used to
--   delimit sections in the auto-generated keymap help.
type Keymap = [Maybe (Event, (WSEdit (), String))]



-- | Mode for syntax highlighting.
data HighlightMode = HNone
                   | HBracket
                   | HComment
                   | HError
                   | HKeyword
                   | HSearch
                   | HSelected
                   | HString
    deriving (Eq, Read, Show)



-- | Absolute path with all symlinks resolved.
newtype CanonicalPath = CanonicalPath { getCanonicalPath :: FilePath }
    deriving (Eq, Read, Show)



-- | File pattern to match against.
data FileMatch = MatchFilename FilePath
               | MatchPath     CanonicalPath
    deriving (Eq, Read, Show)
