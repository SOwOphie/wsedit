module WSEdit.Control.Base
    ( refuseOnReadOnly
    , alterBuffer
    , alterState
    , moveViewport
    , moveCursor
    ) where


import Control.Monad            (unless, when)
import Control.Monad.RWS.Strict (get, modify)
import Data.Maybe               (fromMaybe)

import WSEdit.Data              ( EdState  ( canComplete, edLines, readOnly
                                           , wantsPos
                                           )
                                , WSEdit
                                , alter, getCursor, getDisplayBounds, getOffset
                                , setCursor, setStatus, setOffset
                                )
import WSEdit.Output            ( lineNoWidth, toCursorDispPos, txtToVisPos
                                , visToTxtPos
                                )
import WSEdit.Util              (withPair)

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
alterState :: WSEdit () -> WSEdit ()
alterState a = modify (\s -> s { canComplete = False })
            >> a



-- | Moves the viewport by the given amount of rows, columns. Will also ensure
--   that the cursor stays within the viewport by dragging it along, except in
--   read-only mode.
moveViewport :: Int -> Int -> WSEdit ()
moveViewport r c = do
    getOffset
        >>= setOffset
            . withPair
                (max 0 . (+r))
                (max 0 . (+c))


    b <- readOnly <$> get
    unless b $ do
        (curR, curC) <- getCursor >>= toCursorDispPos
        (maxR, maxC) <- getDisplayBounds
        lnW <- lineNoWidth

        when (curR > maxR - 3) $ moveCursor ((maxR - 3) - curR) 0
        when (curR <        2) $ moveCursor (        2  - curR) 0
        when (curC > maxC - 1) $ moveCursor 0                   ((maxC - 1) - curC)
        when (curC < lnW  + 3) $ moveCursor 0                   ((lnW  + 3) - curC)





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

            (curR, curC) <- getCursor >>= toCursorDispPos
            (maxR, maxC) <- getDisplayBounds
            lnW <- lineNoWidth

            when (curR > maxR - 3) $ moveViewport (curR - (maxR - 3)) 0
            when (curR <        2) $ moveViewport (curR -         2 ) 0
            when (curC > maxC - 1) $ moveViewport 0                   (curC - (maxC - 1))
            when (curC < lnW  + 4) $ moveViewport 0                   (curC - (lnW  + 4))

    where
        moveV :: Int -> WSEdit ()
        moveV n = do
            (currR, currC) <- getCursor
            s <- get

            let lns      = edLines s
                currLn   = fromMaybe "" $ B.left lns
                tLnNo    = min (B.length lns) $ max 1 $ currR + n
                targetLn = B.atDef "" lns $ tLnNo - 1

            vPos <- txtToVisPos currLn currC

            tPos <- case wantsPos s of
                 Nothing -> do
                    unless (n == 0)
                        $ modify (\s' -> s' { wantsPos = Just vPos })
                    return vPos

                 Just p  -> do
                    return p

            newC <- visToTxtPos targetLn tPos
            setCursor (tLnNo, newC)

        moveH :: Int -> WSEdit ()
        moveH n = do
            (currR, currC) <- getCursor
            lns <- edLines <$> get

            let currLn = fromMaybe "" $ B.left lns

            setCursor (currR
                      , min (length currLn + 1)
                      $ max 1
                      $ currC + n
                      )

            modify (\s -> s { wantsPos = Nothing })
