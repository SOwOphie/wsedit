{-# LANGUAGE LambdaCase #-}

module WSEdit.Control.Selection
    ( initMark
    , selectAll
    , ifMarked
    , deleteSelection
    , copy
    , paste
    , indentSelection
    , unindentSelection
    , searchFor
    , unsearch
    ) where


import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.RWS.Strict
    ( ask
    , get
    , modify
    , put
    )
import Data.List
    ( stripPrefix
    )
import Data.Maybe
    ( fromMaybe
    , isJust
    )
import Safe
    ( headMay
    )
import System.Directory
    ( getHomeDirectory
    )
import System.Hclip
    ( getClipboard
    , setClipboard
    )
import System.Posix.Files
    ( fileMode
    , getFileStatus
    , intersectFileModes
    , ownerModes
    , setFileMode
    )

import WSEdit.Control.Base
    ( alterBuffer
    , alterState
    , getInput
    , refuseOnReadOnly
    , validateCursor
    )
import WSEdit.Control.Text
    ( insertText
    )
import WSEdit.Data
    ( EdConfig
        ( tabWidth
        )
    , EdState
        ( cursorPos
        , edLines
        , fullRebdReq
        , markPos
        , replaceTabs
        , searchTerms
        )
    , WSEdit
    )
import WSEdit.Data.Algorithms
    ( clearMark
    , delSelection
    , getMark
    , getCursor
    , getSelection
    , setMark
    , setStatus
    )
import WSEdit.Util
    ( checkClipboardSupport
    , linesPlus
    , mayReadFile
    , withSnd
    )

import qualified WSEdit.Buffer as B





-- | Throw down the mark at the current cursor position, if it is not placed
--   somewhere else already.
initMark :: WSEdit ()
initMark = alterState
         $ getMark >>= \case
                Nothing -> getCursor >>= setMark
                _       -> return ()



-- | Select all text (mark at beginning, cursor at end).
selectAll :: WSEdit ()
selectAll = alterState $ do
    s <- get
    let l = B.toLast $ edLines s
    modify $ \s' -> s' { markPos   = Just (1, 1)
                       , edLines   = l
                       , cursorPos = length (snd $ B.pos l) + 1
                       }
    validateCursor



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
                            fname <- (++ "/.wsedit-clipboard")
                                 <$> getHomeDirectory

                            writeFile   fname ""
                            setFileMode fname
                                . intersectFileModes ownerModes
                              =<< fmap fileMode (getFileStatus fname)

                            writeFile fname s

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
            insertText c1
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



-- | Add the currently selected area to the list of search terms, or ask for a
--   search term if the selection is empty.
searchFor :: WSEdit ()
searchFor = do
    getSelection >>= \case
        Nothing -> getInput "> " >>= \case
            Nothing -> return ()
            Just "" -> return ()
            Just  s -> do
                modify (\c -> c { searchTerms = s : filter (/= s) (searchTerms c) })
                setStatus $ "Added \"" ++ s ++ "\" to the list of search terms."
                modify $ \st -> st { fullRebdReq = True }

        Just  s -> do
            modify (\c -> c { searchTerms = s : filter (/= s) (searchTerms c) })
            setStatus $ "Added \"" ++ s ++ "\" to the list of search terms."
            modify $ \st -> st { fullRebdReq = True }



-- | Remove the most recently added search term from the list.
unsearch :: WSEdit ()
unsearch = headMay . searchTerms <$> get >>= \case
    Nothing -> setStatus "Warning: no search terms."
    Just  s -> do
        modify (\c -> c { searchTerms =     drop 1       $ searchTerms c  })
        setStatus $ "Removed \"" ++ s ++ "\" from the list of search terms."
        modify $ \st -> st { fullRebdReq = True }
