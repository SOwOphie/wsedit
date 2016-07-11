{-# LANGUAGE LambdaCase #-}

module WSEdit.Control.Selection
    ( initMark
    , ifMarked
    , deleteSelection
    , copy
    , paste
    , indentSelection
    , unindentSelection
    , searchFor
    ) where


import Control.Monad.IO.Class   (liftIO)
import Control.Monad.RWS.Strict (ask, get, modify, put)
import Data.List                (stripPrefix)
import Data.Maybe               (fromMaybe, isJust)
import Safe                     (headMay, headNote, initNote, lastNote)
import System.Directory         (getHomeDirectory)
import System.Hclip             (getClipboard, setClipboard)

import WSEdit.Control.Base      ( alterBuffer, alterState, moveCursor
                                , refuseOnReadOnly
                                )
import WSEdit.Data              ( EdConfig (tabWidth)
                                , EdState (cursorPos, edLines, markPos
                                          , replaceTabs, searchTerms
                                          )
                                , WSEdit
                                , clearMark, delSelection, getMark, getCursor
                                , getSelection, setMark, setStatus
                                )
import WSEdit.Util              ( checkClipboardSupport, linesPlus, mayReadFile
                                , withSnd
                                )

import qualified WSEdit.Buffer as B



fqn :: String -> String
fqn = ("WSEdit.Control.Selection." ++)





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
deleteSelection = flip ifMarked (return ()) $ alterBuffer $ do
    _ <- delSelection
    clearMark



-- | Copy the text in the selection to the clipboard.
copy :: WSEdit ()
copy = refuseOnReadOnly
     $ getSelection >>= \case
            Nothing -> setStatus "Warning: nothing selected."
            Just s  -> do
                b <- liftIO checkClipboardSupport

                if b
                   then do
                        liftIO $ setClipboard s

                        setStatus $ "Copied "
                                 ++ show (length $ linesPlus s)
                                 ++ " lines ("
                                 ++ show (length s)
                                 ++ " chars) to system clipboard."

                   else do
                        liftIO $ do
                            h <- getHomeDirectory
                            writeFile (h ++ "/.wsedit-clipboard") s

                        setStatus $ "Copied "
                                 ++ show (length $ linesPlus s)
                                 ++ " lines ("
                                 ++ show (length s)
                                 ++ " chars) to editor clipboard."




-- | Paste the clipboard contents to the cursor position.
paste :: WSEdit ()
paste = alterBuffer $ do
    b <- liftIO checkClipboardSupport

    c1 <- liftIO
        $ if b
             then getClipboard
             else do
                    h <- getHomeDirectory
                    fromMaybe "" <$> mayReadFile (h ++ "/.wsedit-clipboard")

    if c1 == ""
       then setStatus $ if b
                           then "Warning: System clipboard is empty."
                           else "Warning: Editor clipboard is empty."

       else do
            let c = linesPlus c1
            s <- get

            put $ s     -- Arcane buffer magic incoming...
                { edLines =
                    if length c == 1
                       then B.withCurr (withSnd (\l -> take (cursorPos s - 1) l
                                                    ++ headNote (fqn "paste") c
                                                    ++ drop (cursorPos s - 1) l
                                                )
                                       )
                          $ edLines s

                       else B.insertLeft (False, lastNote (fqn "paste") c
                                              ++ drop (cursorPos s - 1)
                                                      (snd $ B.curr $ edLines s)
                                         )
                          $ flip (foldl (flip B.insertLeft))
                                 ( zip (repeat False)
                                 $ drop 1
                                 $ initNote (fqn "paste") c
                                 )
                          $ B.withCurr (withSnd (\l -> take (cursorPos s - 1) l
                                                    ++ headNote (fqn "paste") c
                                                )
                                       )
                          $ edLines s
                }

            if length c > 1
               then moveCursor 0 $ length (last c) - cursorPos s + 1
               else moveCursor 0 $ length c1

            setStatus $ "Pasted "
                     ++ show (length c)
                     ++ " lines ("
                     ++ show (length c1)
                     ++ if b
                           then " chars) from system clipboard."
                           else " chars) from editor clipboard."



-- | Indent the currently selected area using the current tab width and
--   replacement settings.
indentSelection :: WSEdit ()
indentSelection = alterBuffer
    $ getMark >>= \case
       Nothing      -> return ()
       Just (sR, _) -> do
            s <- get
            c <- ask
            (cR, _) <- getCursor

            let
                ind = if replaceTabs s
                         then replicate (tabWidth c) ' '
                         else "\t"

            put $ s { edLines =
                        case compare sR cR of
                             LT -> B.withCurr            (withSnd (ind ++))
                                 $ B.withNLeft (cR - sR) (withSnd (ind ++))
                                 $ edLines s

                             EQ -> B.withCurr (withSnd (ind ++))
                                 $ edLines s

                             GT -> B.withCurr             (withSnd (ind ++))
                                 $ B.withNRight (sR - cR) (withSnd (ind ++))
                                 $ edLines s
                     }



-- | Unindent the currently selected area using the current tab width and
--   replacement settings.
unindentSelection :: WSEdit ()
unindentSelection = alterBuffer
    $ getMark >>= \case
       Nothing      -> return ()
       Just (sR, _) -> do
            s <- get
            c <- ask
            (cR, _) <- getCursor

            let
                ind = if replaceTabs s
                         then replicate (tabWidth c) ' '
                         else "\t"

            put $ s { edLines =
                        case compare sR cR of
                             LT -> B.withCurr            (withSnd (unindent ind))
                                 $ B.withNLeft (cR - sR) (withSnd (unindent ind))
                                 $ edLines s

                             EQ -> B.withCurr (withSnd (unindent ind))
                                 $ edLines s

                             GT -> B.withCurr             (withSnd (unindent ind))
                                 $ B.withNRight (sR - cR) (withSnd (unindent ind))
                                 $ edLines s
                     }
    where
        unindent :: String -> String -> String
        unindent prf ln = fromMaybe ln
                        $ stripPrefix prf ln



-- | Add the currently selected area to the list of search terms, or pop the
--   last search term from the list if the selection is empty.
searchFor :: WSEdit ()
searchFor = getSelection >>= \case
    Nothing -> headMay . searchTerms <$> get >>= \case
        Nothing -> setStatus "Warning: no search terms."
        Just  s -> do
            modify (\c -> c { searchTerms =     drop 1       $ searchTerms c  })
            setStatus $ "Removed \"" ++ s ++ "\" from the list of search terms."

    Just  s -> do
        modify (\c -> c { searchTerms = s : filter (/= s) (searchTerms c) })
        setStatus $ "Added \"" ++ s ++ "\" to the list of search terms."
