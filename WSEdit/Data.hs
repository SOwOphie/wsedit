{-# LANGUAGE LambdaCase #-}

module WSEdit.Data
    ( EdState (..)
    , getCursor
    , setCursor
    , getMark
    , setMark
    , clearMark
    , getFirstSelected
    , getLastSelected
    , getOffset
    , setOffset
    , setStatus
    , alter
    , popHist
    , getSelection
    , delSelection
    , getDisplayBounds
    , EdConfig (..)
    , EdDesign (..)
    , WSEdit
    , catchEditor
    , Keymap
    ) where


import Control.Exception        (SomeException, evaluate, try)
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.RWS.Strict (RWST, ask, get, modify, put, runRWST)
import Data.Default             (Default (def))
import Data.Maybe               (fromJust, fromMaybe, isJust)
import Data.Tuple               (swap)
import Graphics.Vty             ( Attr
                                , Event
                                , Vty (outputIface)
                                , black, blue, bold, green, defAttr
                                , displayBounds, green, red, white
                                , withBackColor, withForeColor, withStyle
                                , yellow
                                )

import WSEdit.Util              (CharClass ( Bracket, Digit, Lower, Operator
                                           , Special, Upper, Whitesp
                                           )
                                )
import WSEdit.WordTree          (WordTree, empty)

import qualified WSEdit.Buffer as B



-- | Editor state container (dynamic part).
data EdState = EdState
    { edLines      :: B.Buffer String
    , fname        :: FilePath
    , readOnly     :: Bool
    , cursorPos    :: Int
    , wantsPos     :: Maybe Int
    , markPos      :: Maybe (Int, Int)
    , scrollOffset :: (Int, Int)

    , continue     :: Bool
    , status       :: String

    , changed      :: Bool
    , history      :: Maybe EdState

    , buildDict    :: Maybe Int
    , dict         :: WordTree
    , canComplete  :: Bool

    , tabWidth     :: Int
    , replaceTabs  :: Bool

    , drawBg       :: Bool
    }
    deriving (Show)

instance Default EdState where
    def = EdState
        { edLines      = B.singleton ""
        , fname        = ""
        , readOnly     = False
        , cursorPos    = 1
        , wantsPos     = Nothing
        , markPos      = Nothing
        , scrollOffset = (0, 0)

        , continue     = True
        , status       = ""

        , changed      = False
        , history      = Nothing

        , buildDict    = Nothing
        , dict         = empty
        , canComplete  = False

        , tabWidth     = 4
        , replaceTabs  = False

        , drawBg       = True
        }


-- | Retrieve the current cursor position.
getCursor :: WSEdit (Int, Int)
getCursor = do
    s <- get
    return (B.currPos $ edLines s, cursorPos s)

-- | Set the current cursor position.
setCursor :: (Int, Int) -> WSEdit ()
setCursor (r, c) = do
    s <- get
    put $ s { cursorPos = c
            , edLines   = B.moveTo r $ edLines s
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
getFirstSelected =
    getMark >>= \case
        Nothing       -> return Nothing
        Just (mR, mC) -> do
            (cR, cC) <- getCursor

            case compare mR cR of
                 LT -> return $ Just (mR, mC)
                 GT -> return $ Just (cR, cC)
                 EQ ->
                    case compare mC cC of
                         LT -> return $ Just (mR, mC)
                         GT -> return $ Just (cR, cC)
                         EQ -> return Nothing


-- | Retrieve the position of the last selected element.
getLastSelected :: WSEdit (Maybe (Int, Int))
getLastSelected =
    getMark >>= \case
        Nothing -> return Nothing
        Just (mR, mC) -> do
            (cR, cC) <- getCursor

            case compare mR cR of
                 LT -> return $ Just (cR, cC - 1)
                 GT -> return $ Just (mR, mC - 1)
                 EQ ->
                    case compare mC cC of
                         LT -> return $ Just (cR, cC - 1)
                         GT -> return $ Just (mR, mC - 1)
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
    st' <- liftIO $ evaluate st
    put $ s { status = st' }



-- | Create an undo checkpoint and set the changed flag.
alter :: WSEdit ()
alter = do
    h <- histSize <$> ask
    modify (\s -> s { history = chopHist h (Just s)
                    , changed = True
                    } )
    where
        chopHist :: Int -> Maybe EdState -> Maybe EdState
        chopHist n _        | n <= 0 = Nothing
        chopHist _ Nothing           = Nothing
        chopHist n (Just s)          =
            Just $ s { history = chopHist (n-1) (history s) }


-- | Restore the last undo checkpoint.
popHist :: WSEdit ()
popHist = modify popHist'

    where
        popHist' :: EdState -> EdState
        popHist' s = fromMaybe s $ history s



-- | Retrieve the contents of the current selection.
getSelection :: WSEdit (Maybe String)
getSelection = do
    b <- isJust . markPos <$> get
    if not b
       then return Nothing
       else do
            (sR, sC) <- fromJust <$> getFirstSelected
            (eR, eC) <- fromJust <$> getLastSelected
            l <- edLines <$> get

            if sR == eR
               then return $ Just
                           $ drop (sC - 1)
                           $ take eC
                           $ fromJust
                           $ B.left l

               else
                    let
                        lns = B.sub (sR - 1) (eR - 1) l
                    in
                        return $ Just
                               $ drop (sC - 1) (head lns) ++ "\n"
                              ++ unlines (tail $ init lns)
                              ++ take eC (last lns)



-- | Delete the contents of the current selection from the text buffer.
delSelection :: WSEdit Bool
delSelection = do
    b <- isJust . markPos <$> get
    if not b
       then return False
       else do
            (_ , sC) <- fromJust <$> getFirstSelected
            (_ , eC) <- fromJust <$> getLastSelected

            (mR, mC) <- fromJust <$> getMark
            (cR, cC) <- getCursor

            s <- get

            case compare mR cR of
                 EQ -> do
                    put $ s { edLines   = fromJust
                                        $ B.withLeft (\l -> take (sC - 1) l
                                                         ++ drop  eC      l
                                                     )
                                        $ edLines s
                            , cursorPos = sC
                            }
                    return True

                 LT -> do
                    put $ s { edLines   = fromJust
                                        $ B.withLeft (\l -> take (mC - 1) l
                                                         ++ drop (cC - 1)
                                                              ( fromJust
                                                              $ B.left
                                                              $ edLines s
                                                              )
                                                     )
                                        $ B.dropLeft (cR - mR)
                                        $ edLines s
                            , cursorPos = sC
                            }
                    return True

                 GT ->
                    let
                        b' = B.dropRight (mR - cR - 1)
                           $ edLines s
                    in do
                        put $ s { edLines   = fromJust
                                            $ B.withLeft (\l -> take (cC - 1) l
                                                             ++ drop (mC - 1)
                                                                  (fromMaybe ""
                                                                  $ B.right b'
                                                                  )
                                                         )
                                            $ B.deleteRight b'
                                , cursorPos = sC
                                }
                        return True



-- | Retrieve the number of rows, colums displayed.
getDisplayBounds :: WSEdit (Int, Int)
getDisplayBounds = ask
               >>= displayBounds . outputIface . vtyObj
               >>= return . swap





-- | Editor configuration container (static part).
data EdConfig = EdConfig
    { vtyObj   :: Vty
    , edDesign :: EdDesign
    , keymap   :: Keymap
    , histSize :: Int
    }





-- | Design portion of the editor configuration.
data EdDesign = EdDesign
    { dFrameFormat   :: Attr
    , dStatusFormat  :: Attr

    , dLineNoFormat  :: Attr
    , dLineNoInterv  :: Int

    , dColNoInterval :: Int
    , dColNoFormat   :: Attr

    , dTextDefFormat :: Attr

    , dBGChar        :: Char
    , dColChar       :: Maybe Char
    , dBGFormat      :: Attr

    , dCurrLnMod     :: Attr -> Attr

    , dTabStr        :: String

    , dSelFormat     :: Attr

    , dCharStyles    :: [(CharClass, Attr)]
    }


instance Default EdDesign where
    def = EdDesign
        { dFrameFormat   = defAttr
                            `withForeColor` green

        , dStatusFormat  = defAttr
                            `withForeColor` green
                            `withStyle`     bold

        , dLineNoFormat  = defAttr
                            `withForeColor` green
                            `withStyle`     bold
        , dLineNoInterv  = 10

        , dColNoInterval = 40
        , dColNoFormat   = defAttr
                            `withForeColor` green
                            `withStyle`     bold

        , dTextDefFormat = defAttr

        , dBGChar        = '·'
        , dColChar       = Just '|'
        , dBGFormat      = defAttr
                            `withForeColor` black

        , dCurrLnMod     = flip withBackColor black

        , dTabStr        = "        |"

        , dSelFormat     = defAttr
                            `withForeColor` black
                            `withBackColor` white

        , dCharStyles    =
            [ (Whitesp , defAttr
                            `withForeColor` blue
              )
            , (Digit   , defAttr
                            `withForeColor` red
              )
            , (Lower   , defAttr
              )
            , (Upper   , defAttr
                            `withStyle`     bold
              )
            , (Bracket , defAttr
                            `withForeColor` yellow
              )
            , (Operator, defAttr
                            `withForeColor` yellow
                            `withStyle`     bold
              )
            , (Special , defAttr
                            `withForeColor` red
                            `withStyle`     bold
              )
            ]

        }



-- | Editor monad.
type WSEdit = RWST EdConfig () EdState IO



-- | Lifted version of 'catch'.
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



-- | Map of events to actions.
type Keymap = [(Event, WSEdit ())]
