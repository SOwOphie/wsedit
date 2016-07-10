{-# LANGUAGE FlexibleInstances
           , LambdaCase
           , TypeSynonymInstances
           #-}

module WSEdit.Data
    ( version
    , upstream
    , licenseVersion
    , EdState (..)
    , getCursor
    , setCursor
    , getMark
    , setMark
    , clearMark
    , getFirstSelected
    , getLastSelected
    , getSelBounds
    , getOffset
    , setOffset
    , setStatus
    , chopHist
    , mapPast
    , alter
    , popHist
    , getSelection
    , delSelection
    , getDisplayBounds
    , EdConfig (..)
    , mkDefConfig
    , EdDesign (..)
    , brightTheme
    , WSEdit
    , catchEditor
    , Keymap
    , HighlightMode (..)
    ) where


import Control.Exception        (SomeException, evaluate, try)
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.RWS.Strict (RWST, ask, get, modify, put, runRWST)
import Data.Default             (Default (def))
import Data.Maybe               (fromMaybe)
import Data.Tuple               (swap)
import Graphics.Vty             ( Attr
                                , Event (..)
                                , Vty (outputIface)
                                , black, blue, bold, brightBlack, brightGreen
                                , brightMagenta, brightRed, brightWhite
                                , brightYellow, cyan, green, defAttr
                                , displayBounds, green, magenta, red, white
                                , withBackColor, withForeColor, withStyle
                                , yellow
                                )
import Safe                     ( fromJustNote, headNote, initNote, lastNote
                                , tailNote
                                )
import System.IO                (NewlineMode, universalNewlineMode)

import WSEdit.Util              (CharClass ( Bracket, Digit, Lower, Operator
                                           , Special, Unprintable, Upper
                                           , Whitesp
                                           )
                                )
import WSEdit.WordTree          (WordTree, empty)

import qualified WSEdit.Buffer as B



fqn :: String -> String
fqn = ("WSEdit.Data." ++)





-- | Version number constant.
version :: String
version = "1.0.0.7 RC2"

-- | Upstream URL.
upstream :: String
upstream = "https://github.com/SirBoonami/wsedit"

-- | License version number constant.
licenseVersion :: String
licenseVersion = "1.1"





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

    , status       :: String
        -- ^ Status string displayed at the bottom.

    , lastEvent    :: Maybe Event
        -- ^ Last recorded input event.


    , changed      :: Bool
        -- ^ Whether the file has been changed since the last load/save.

    , history      :: Maybe EdState
        -- ^ Editor state prior to the last action, used to implement undo
        --   facilities.  Horrible memory efficiency, but it seems to work.


    , buildDict    :: [(Maybe String, Maybe Int)]
        -- ^ File suffix and indentation depth pairs for dictionary building.
        --   'Nothing' stands for the current file or all depths.

    , dict         :: WordTree
        -- ^ Autocompletion dictionary.

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
    }
    deriving (Eq, Read, Show)

instance Default EdState where
    def = EdState
        { edLines      = B.singleton (False, "")
        , fname        = ""
        , readOnly     = False

        , cursorPos    = 1
        , loadPos      = (1, 1)
        , wantsPos     = Nothing
        , markPos      = Nothing
        , scrollOffset = (0, 0)

        , continue     = True
        , status       = ""
        , lastEvent    = Nothing

        , changed      = False
        , history      = Nothing

        , buildDict    = []
        , dict         = empty
        , canComplete  = False
        , replaceTabs  = False
        , detectTabs   = True
        , overwrite    = False
        , searchTerms  = []
        }


-- | Retrieve the current cursor position.
getCursor :: WSEdit (Int, Int)
getCursor = do
    s <- get
    return (B.currPos (edLines s) + 1, cursorPos s)

-- | Set the current cursor position.
setCursor :: (Int, Int) -> WSEdit ()
setCursor (r, c) = do
    s <- get
    put $ s { cursorPos = c
            , edLines   = B.moveTo (r - 1) $ edLines s
            }


-- | Retrieve the current mark position, if it exists.
getMark :: WSEdit (Maybe (Int, Int))
getMark = markPos <$> get


-- | Set the mark to a position.
setMark :: (Int, Int) -> WSEdit ()
setMark p = do
    s <- get
    put $ s { markPos = Just p }

-- | Clear a previously set mark.
clearMark :: WSEdit ()
clearMark = do
    s <- get
    put $ s { markPos = Nothing }



-- | Retrieve the position of the first selected element.
getFirstSelected :: WSEdit (Maybe (Int, Int))
getFirstSelected = fmap fst <$> getSelBounds


-- | Retrieve the position of the last selected element.
getLastSelected :: WSEdit (Maybe (Int, Int))
getLastSelected = fmap snd <$> getSelBounds


-- | Faster combination of 'getFirstSelected' and 'getLastSelected'.
getSelBounds :: WSEdit (Maybe ((Int, Int), (Int, Int)))
getSelBounds =
    getMark >>= \case
        Nothing -> return Nothing
        Just (mR, mC) -> do
            (cR, cC) <- getCursor

            case compare mR cR of
                 LT -> return $ Just ((mR, mC), (cR, cC - 1))
                 GT -> return $ Just ((cR, cC), (mR, mC - 1))
                 EQ ->
                    case compare mC cC of
                         LT -> return $ Just ((mR, mC), (cR, cC - 1))
                         GT -> return $ Just ((cR, cC), (mR, mC - 1))
                         EQ -> return Nothing




-- | Retrieve the current viewport offset (relative to the start of the file).
getOffset :: WSEdit (Int, Int)
getOffset = scrollOffset <$> get

-- | Set the viewport offset.
setOffset :: (Int, Int) -> WSEdit ()
setOffset p = do
    s <- get
    put $ s { scrollOffset = p }



-- | Set the status line's contents.
setStatus :: String -> WSEdit ()
setStatus st = do
    s <- get

    -- Precaution, since lazyness can be quirky sometimes
    st' <- liftIO $ evaluate st

    put $ s { status = st' }



-- | The 'EdState' 'history' is structured like a conventional list, and
--   this is its 'take', with some added 'Maybe'ness.
chopHist :: Int -> Maybe EdState -> Maybe EdState
chopHist n _        | n <= 0 = Nothing
chopHist _ Nothing           = Nothing
chopHist n (Just s)          =
    Just $ s { history = chopHist (n-1) (history s) }

-- | The 'EdState' 'history' is structured like a conventional list, and
--   this is its 'map'.  Function doesn't get applied to the present state
--   though.
mapPast :: (EdState -> EdState) -> EdState -> EdState
mapPast f s =
    case history s of
         Nothing -> s
         Just  h -> s { history = Just $ mapPast f $ f h }



-- | Create an undo checkpoint and set the changed flag.
alter :: WSEdit ()
alter = do
    h <- histSize <$> ask
    modify (\s -> s { history = chopHist h (Just s)
                    , changed = True
                    } )


-- | Restore the last undo checkpoint, if available.
popHist :: WSEdit ()
popHist = modify popHist'

    where
        -- | The 'EdState' 'history' is structured like a conventional list, and
        --   this is its 'tail'.
        popHist' :: EdState -> EdState
        popHist' s = fromMaybe s $ history s



-- | Retrieve the contents of the current selection.
getSelection :: WSEdit (Maybe String)
getSelection = getSelBounds >>= \case
    Nothing                   -> return Nothing
    Just ((sR, sC), (eR, eC)) -> do
        l <- edLines <$> get

        if sR == eR
           then return $ Just
                       $ drop (sC - 1)
                       $ take eC
                       $ snd
                       $ B.curr l

           else
                let
                    lns = map snd $ B.sub (sR - 1) (eR - 1) l
                in
                    return $ Just
                           $ drop (sC - 1) (headNote (fqn "getSelection") lns)
                          ++ "\n"
                          ++ unlines ( tailNote (fqn "getSelection")
                                     $ initNote (fqn "getSelection")
                                       lns
                                     )
                          ++ take eC (lastNote (fqn "getSelection") lns)



-- | Delete the contents of the current selection from the text buffer.
delSelection :: WSEdit Bool
delSelection = getSelBounds >>= \case
    Nothing                 -> return False
    Just ((_, sC), (_, eC)) -> do
        (mR, mC) <- fromJustNote (fqn "getSelection") <$> getMark
        (cR, cC) <- getCursor

        s <- get

        case compare mR cR of
             EQ -> do
                put $ s { edLines   = B.withCurr (\(b, l) -> (b, take (sC - 1) l
                                                              ++ drop  eC      l
                                                             )
                                                 )
                                    $ edLines s
                        , cursorPos = sC
                        }
                return True

             LT -> do
                put $ s { edLines   = B.withCurr (\(b, l) -> (b, take (mC - 1) l
                                                              ++ drop (cC - 1)
                                                                 ( snd
                                                                 $ B.curr
                                                                 $ edLines s
                                                                 )
                                                             )
                                                 )
                                    $ B.dropLeft (cR - mR)
                                    $ edLines s
                        , cursorPos = sC
                        }
                return True

             GT -> do
                put $ s { edLines   = B.withCurr (\(b, l) -> (b, take (cC - 1)
                                                               ( snd
                                                               $ B.curr
                                                               $ edLines s
                                                               )
                                                              ++ drop (mC - 1) l
                                                             )
                                                 )
                                    $ B.dropRight (mR - cR)
                                    $ edLines s
                        , cursorPos = sC
                        }
                return True



-- | Retrieve the number of rows, colums displayed by vty, including all borders
--   , frames and similar woo.
getDisplayBounds :: WSEdit (Int, Int)
getDisplayBounds = fmap swap (displayBounds . outputIface . vtyObj =<< ask)





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

    , purgeOnClose :: Bool
        -- ^ Whether the clipboard file is to be deleted on close.


    , newlineMode  :: NewlineMode
        -- ^ Newline conversion to use.

    , encoding     :: Maybe String
        -- ^ Name of the file encoding to use.


    , lineComment  :: [String]
        -- ^ List of strings that mark the beginning of a comment.

    , strDelim     :: [(Char, Char)]
        -- ^ List of string delimiters.

    , keywords     :: [String]
        -- ^ List of keywords to highlight.

    , escape       :: Maybe Char
        -- ^ Escape character for strings.
    }

-- | Create a default `EdConfig`.
mkDefConfig :: Vty -> Keymap -> EdConfig
mkDefConfig v k = EdConfig
                { vtyObj       = v
                , edDesign     = def
                , keymap       = k
                , histSize     = 100
                , tabWidth     = 4
                , drawBg       = True
                , dumpEvents   = False
                , purgeOnClose = False
                , newlineMode  = universalNewlineMode
                , encoding     = Nothing
                , lineComment  = []
                , strDelim     = []
                , keywords     = []
                , escape       = Nothing
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


    , dCurrLnMod     :: Attr -> Attr
        -- ^ Attribute modifications to apply to the current line

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

        , dCurrLnMod     = flip withBackColor black
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
                            `withStyle`     bold
              )
            , (Bracket    , defAttr
                            `withForeColor` yellow
              )
            , (Operator   , defAttr
                            `withForeColor` brightYellow
                            `withStyle`     bold
              )
            , (Unprintable, defAttr
                            `withForeColor` brightRed
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

        , dCurrLnMod     = flip withBackColor white
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
                            `withStyle`     bold
              )
            , (Bracket    , defAttr
                            `withForeColor` yellow
              )
            , (Operator   , defAttr
                            `withForeColor` brightYellow
                            `withStyle`     bold
              )
            , (Unprintable, defAttr
                            `withForeColor` brightRed
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



-- | Editor monad. Reads an 'EdConfig', writes nothing, alters an 'EdState'.
type WSEdit = RWST EdConfig () EdState IO



-- | Lifted version of 'catch' typed to 'SomeException'.
catchEditor :: WSEdit a -> (SomeException -> WSEdit a) -> WSEdit a
catchEditor a e = do
    c <- ask
    s <- get
    (r, s') <- liftIO $ try (runRWST a c s) >>= \case
                    Right (r, s', _) -> return (r, s')
                    Left  err        -> do
                        (r, s', _) <- runRWST (e err) c s
                        return (r, s')
    put s'
    return r



-- | Map of events to actions (and their descriptions). 'Nothing's are used to
--   mark sections.
type Keymap = [Maybe (Event, (WSEdit (), String))]



-- | Mode for syntax highlighting.
data HighlightMode = HNone
                   | HComment
                   | HKeyword
                   | HSearch
                   | HSelected
                   | HString
    deriving (Eq, Read, Show)
