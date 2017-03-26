{-# LANGUAGE LambdaCase #-}

module WSEdit.Control.Base
    ( refuseOnReadOnly
    , alterBuffer
    , alterState
    , moveViewport
    , moveCursor
    , fetchCursor
    , standby
    , askConfirm
    , showText
    ) where


import Control.Exception          (SomeException)
import Control.Monad              (unless, when)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.RWS.Strict   (ask, get, modify, put)
import Control.Monad.Trans.Either (EitherT (EitherT, runEitherT))
import Data.Char                  (isSpace, toLower)
import Data.Default               (def)
import Data.Maybe                 (catMaybes, fromMaybe)
import Graphics.Vty               ( Event (EvKey, EvResize)
                                  , Key (KChar)
                                  , nextEvent
                                  )

import WSEdit.Data                ( EdConfig (keymap, vtyObj)
                                  , EdState  ( badgeText, canComplete, continue
                                             , cursorPos, edLines, rangeCache
                                             , readOnly, scrollOffset, status
                                             , wantsPos
                                             )
                                  , FmtParserState (PNothing)
                                  , HighlightMode (HSearch)
                                  , Keymap
                                  , WSEdit
                                  , mkDefConfig
                                  , runIn
                                  )
import WSEdit.Data.Algorithms     ( alter, getCursor, getOffset, setCursor
                                  , setStatus, setOffset, tryEditor
                                  )
import WSEdit.Output              ( cursorOffScreen, draw, getViewportDimensions
                                  , txtToVisPos, visToTxtPos
                                  )
import WSEdit.Renderer            (rebuildAll)
import WSEdit.Util                (linesPlus, withPair)

import qualified WSEdit.Buffer as B





-- | Guard the given action against use in read-only mode. Will use 'setStatus'
--   to issue a warning.
refuseOnReadOnly :: WSEdit () -> WSEdit ()
refuseOnReadOnly a = do
    s <- get
    if readOnly s
       then setStatus "Warning: read only (press Ctrl-Meta-R to enable editing)"
       else a


-- | Declares that an action will alter the text buffer. Calls
--   'refuseOnReadOnly', creates an undo point, wiggles the cursor to ensure it
--   is in a valid position, ...
alterBuffer :: WSEdit () -> WSEdit ()
alterBuffer a = refuseOnReadOnly
              $ alterState
              $ modify (\s -> s { wantsPos = Nothing })
             >> moveCursor 0 0
             >> alter
             >> a


-- | Declares that an action will alter the text buffer or the cursor position.
--   Included in 'alterBuffer'.
alterState :: WSEdit a -> WSEdit a
alterState a = modify (\s -> s { canComplete = False })
            >> a



-- | Moves the viewport by the given amount of rows, columns.
moveViewport :: Int -> Int -> WSEdit ()
moveViewport r c =
    getOffset
        >>= setOffset
            . withPair
                (max 0 . (+r))
                (max 0 . (+c))





-- | Moves the cursor by the given amount of rows, columns, dragging the
--   viewport along when neccessary. The cursor's movements will be limited
--   by the shape of the underlying text, but pure vertical movement will try to
--   maintain the original horizontal cursor position until horizontal movement
--   occurs or 'alterBuffer' is called.
moveCursor :: Int -> Int -> WSEdit ()
moveCursor r c = alterState $ do
    b <- readOnly <$> get
    if b
       then moveViewport r c
       else do
            moveV r
            unless (c == 0) $ moveH c

            -- Adjust the viewport if necessary
            ((ru, rd), (cl, cr)) <- cursorOffScreen
            moveViewport (rd - ru) (cr - cl)

    where
        -- | Vertical portion of the movement
        moveV :: Int -> WSEdit ()
        moveV n = do
            (currR, currC) <- getCursor
            s <- get

            let lns      = edLines s
                currLn   = snd $ B.pos lns
                tLnNo    = min (B.length lns) $ max 1 $ currR + n
                targetLn = snd $ B.atDef (undefined, "") lns $ tLnNo - 1

            -- Current visual cursor offset (amount of columns)
            vPos <- txtToVisPos currLn currC

            -- Targeted visual cursor offset
            tPos <- case wantsPos s of
                 Just p  -> return p
                 Nothing -> do
                    unless (n == 0)
                        $ modify (\s' -> s' { wantsPos = Just vPos })
                    return vPos

            -- Resulting textual cursor offset (amount of characters)
            newC <- visToTxtPos targetLn tPos
            setCursor (tLnNo, newC)

        -- | Horizontal portion of the movement
        moveH :: Int -> WSEdit ()
        moveH n = do
            (currR, currC) <- getCursor
            lns <- edLines <$> get

            let currLn = snd $ B.pos lns

            setCursor (currR
                      , min (length currLn + 1)
                      $ max 1
                      $ currC + n
                      )

            -- Since this function will not be called for purely vertical
            -- motions, we can safely discard the target cursor position here.
            modify (\s -> s { wantsPos = Nothing })



-- | Moves the cursor to the upper left corner of the viewport.
fetchCursor :: WSEdit ()
fetchCursor = refuseOnReadOnly $ do
    s <- get
    (r, _) <- getViewportDimensions

    put $ s { cursorPos = 1
            , edLines = B.toFirst $ edLines s
            }

    moveCursor (fst (scrollOffset s) + (r `div` 2)) 0



-- | Display a message on the status bar until the next draw call. Restores the
--   original contents of the status bar before returning.
standby :: String -> WSEdit ()
standby str = do
    s      <- get
    (_, w) <- getViewportDimensions

    let
        strLn = forceBreakAt w
              $ linesPlus str

    put $ s { edLines      = fromMaybe (B.singleton (False, ""))
                           $ B.fromList
                           $ zip (repeat False) strLn

            , rangeCache   = replicate (max 1 $ length strLn)
                                       ([((1, maxBound), HSearch)], PNothing)
            , scrollOffset = (0, 0)
            , readOnly     = True
            , badgeText    = Nothing
            }
    draw
    put s

    where
        forceBreakAt :: Int -> [String] -> [String]
        forceBreakAt n = concatMap (breakLine n)

        breakLine :: Int -> String -> [String]
        breakLine _ []      = []
        breakLine l s
            | length s <= l = [s]
            | otherwise     =
                case break isSpace $ reverse $ take l s of
                     (_, []) -> take l s
                              : breakLine l (drop l s)

                     (c, ln) -> reverse (dropWhile isSpace ln)
                              : breakLine l (reverse c ++ drop l s)



-- | Ask the user to confirm something.
askConfirm :: String -> WSEdit Bool
askConfirm str = do
    standby $ str ++ " [y/n] "

    c  <- ask
    ev <- liftIO $ nextEvent $ vtyObj c

    case ev of
         EvKey (KChar k) _
            | toLower k == 'y' -> return True
            | toLower k == 'n' -> return False
         _                     -> askConfirm str



-- | Switch to a text in read-only mode. This function re-implements a reduced
--   version of the main loop to avoid having to deal with circular imports.
showText :: String -> Keymap -> WSEdit ()
showText s k = do
    c <- ask
    runIn ( mkDefConfig (vtyObj c) k
          , def { edLines  = B.toFirst
                           $ fromMaybe (B.singleton (False, ""))
                           $ B.fromList
                           $ zip (repeat False)
                           $ linesPlus s

                , readOnly = True
                }
          ) (rebuildAll Nothing >> runEitherT loop)
        >>= \case
            Right _ -> return ()
            Left  e -> setStatus (show e)

    where
        loop :: EitherT SomeException WSEdit ()
        loop = do
            EitherT $ tryEditor $ do
                c <- ask
                st <- get

                draw
                setStatus ""

                ev <- liftIO $ nextEvent $ vtyObj c


                maybe (case ev of
                            EvResize _ _       -> setStatus $ status st
                            _                  -> setStatus $ "Event not bound: "
                                                           ++ show ev
                      )
                      fst
                      $ lookup ev
                      $ catMaybes
                      $ keymap c


                rebuildAll $ Just st

            continue <$> get >>= flip when loop
