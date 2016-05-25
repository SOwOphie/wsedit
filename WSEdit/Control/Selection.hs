{-# LANGUAGE LambdaCase #-}

module WSEdit.Control.Selection
    ( initMark
    , ifMarked
    , deleteSelection
    , copy
    , paste
    , indentSelection
    , unindentSelection
    ) where


import Control.Monad.IO.Class   (liftIO)
import Control.Monad.RWS.Strict (get, put)
import Data.List                (stripPrefix)
import Data.Maybe               (fromJust, isJust, fromMaybe)
import System.Hclip             (getClipboard, setClipboard)

import WSEdit.Control.Base      ( alterBuffer, alterState, moveCursor
                                , refuseOnReadOnly
                                )
import WSEdit.Data              ( EdState ( cursorPos, edLines, markPos
                                          , replaceTabs, tabWidth
                                          )
                                , WSEdit
                                , clearMark, delSelection, getMark, getCursor
                                , getSelection, setMark, setStatus
                                )

import qualified WSEdit.Buffer as B



-- | Throw down the mark at the current cursor position, if it is not placed
--   somewhere else already.
initMark :: WSEdit ()
initMark = alterState
         $ getMark >>= \case
                Nothing -> getCursor >>= setMark
                _       -> return ()



-- | Executes the first action if the user has selected text (Shift+Movement),
--   or the second one if not.
ifMarked :: WSEdit a -> WSEdit a -> WSEdit a
ifMarked x y = do
    b <- isJust . markPos <$> get
    if b
       then x
       else y



-- | Delete the selected text.
deleteSelection :: WSEdit ()
deleteSelection = alterBuffer $ do
    _ <- delSelection
    clearMark



-- | Copy the text in the selection to the clipboard.
copy :: WSEdit ()
copy = refuseOnReadOnly
     $ getSelection >>= \case
            Nothing -> setStatus "Warning: nothing selected."
            Just s  -> do
                setStatus $ "Copied "
                         ++ show (length $ lines s)
                         ++ " lines ("
                         ++ show (length s)
                         ++ " chars) to clipboard."

                liftIO $ setClipboard s



-- | Paste the clipboard contents to the cursor position.
paste :: WSEdit ()
paste = alterBuffer $ do
    c1 <- liftIO getClipboard
    let c = lines c1
    s <- get

    put $ s
        { edLines = if length c == 1
                       then fromJust
                          $ B.withLeft (\l -> take (cursorPos s - 1) l
                                           ++ head c
                                           ++ drop (cursorPos s - 1) l
                                       )
                          $ edLines s

                       else B.insertLeft (last c ++ drop (cursorPos s - 1)
                                         (fromJust $ B.left $ edLines s))
                          $ flip (foldl (flip B.insertLeft))
                                 (drop 1 $ init c)
                          $ fromJust
                          $ B.withLeft (\l -> take (cursorPos s - 1) l ++ head c)
                          $ edLines s
        }

    if length c > 1
       then moveCursor 0 $ length $ last c
       else moveCursor 0 $ length c1

    setStatus $ "Pasted "
             ++ show (length c)
             ++ " lines ("
             ++ show (length c1)
             ++ " chars) from clipboard."



-- | Indent the currently selected area using the current tab width and
--   replacement settings.
indentSelection :: WSEdit ()
indentSelection = alterBuffer $ do
    getMark >>= \case
       Nothing      -> return ()
       Just (sR, _) -> do
            s <- get
            (cR, _) <- getCursor

            let
                ind = if replaceTabs s
                         then replicate (tabWidth s) ' '
                         else "\t"

            put $ s { edLines =
                        case compare sR cR of
                             LT -> B.withNLeft (cR - sR + 1) (ind ++)
                                 $ edLines s

                             EQ -> fromJust
                                 $ B.withLeft (ind ++)
                                 $ edLines s

                             GT -> fromJust
                                 $ B.withLeft             (ind ++)
                                 $ B.withNRight (sR - cR) (ind ++)
                                 $ edLines s
                     }



-- | Unindent the currently selected area using the current tab width and
--   replacement settings.
unindentSelection :: WSEdit ()
unindentSelection = alterBuffer $ do
    getMark >>= \case
       Nothing      -> return ()
       Just (sR, _) -> do
            s <- get
            (cR, _) <- getCursor

            let
                ind = if replaceTabs s
                         then replicate (tabWidth s) ' '
                         else "\t"

            put $ s { edLines =
                        case compare sR cR of
                             LT -> B.withNLeft (cR - sR + 1) (unindent ind)
                                 $ edLines s

                             EQ -> fromJust
                                 $ B.withLeft (unindent ind)
                                 $ edLines s

                             GT -> fromJust
                                 $ B.withLeft             (unindent ind)
                                 $ B.withNRight (sR - cR) (unindent ind)
                                 $ edLines s
                     }
    where
        unindent :: String -> String -> String
        unindent prf ln = fromMaybe ln
                        $ stripPrefix prf
                          ln
