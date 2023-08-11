{-# LANGUAGE LambdaCase
           , TupleSections
           #-}

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


import Control.Exception
    ( SomeException
    , try
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.RWS.Strict
    ( ask
    , get
    , modify
    , put
    , void
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
    , initNote
    )
import System.Directory
    ( getHomeDirectory
    )
import System.Exit
    ( ExitCode
        ( ExitSuccess
        )
    )
import System.Info
    ( os
    )
import System.Posix.Files
    ( fileMode
    , getFileStatus
    , intersectFileModes
    , ownerModes
    , setFileMode
    )
import System.Process
    ( readCreateProcess
    , readCreateProcessWithExitCode
    , shell
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
    ( linesPlus
    , mayReadFile
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



-- | Check if the specified clipboard command works.
haveClipboardCmd :: String -> IO Bool
haveClipboardCmd s = (try (readCreateProcessWithExitCode (shell s) "") :: IO (Either SomeException (ExitCode, String, String))) >>= \case
    Right (ExitSuccess, _, _) -> return True
    _                         -> return False



-- | Copy the text in the selection to the clipboard.
copy :: WSEdit ()
copy = refuseOnReadOnly $ getSelection >>= \case
            Nothing -> setStatus "Warning: nothing selected."
            Just s  -> firstOrBuiltin s $ case os of
                                               "darwin" -> [ (return True                             , into "pbcopy"            , "pbcopy" ) ]
                                               "linux"  -> [ (haveClipboardCmd "wl-paste"             , into "wl-copy"           , "wl-copy")
                                                           , (haveClipboardCmd "xsel -b -o"           , into "xsel -b -i"        , "xsel"   )
                                                           , (haveClipboardCmd "xclip -selection c -o", into "xclip -selection c", "xclip"  )
                                                           ]
                                               _        -> []
    where
        into :: String -> String -> IO ()
        into cmd text = void $ readCreateProcess (shell cmd) text

        firstOrBuiltin :: String -> [(IO Bool, String -> IO (), String)] -> WSEdit ()
        firstOrBuiltin s ((t, a, n):xs) = do
            liftIO t >>= \case
                False -> firstOrBuiltin s xs
                True  -> liftIO (a s) >> setStatus ( n
                                                  ++ ": copied "
                                                  ++ show (length $ linesPlus s)
                                                  ++ " lines ("
                                                  ++ show (length s)
                                                  ++ " chars) to system clipboard."
                                                   )
        firstOrBuiltin s []          = do
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
    (back, txt) <- firstOrBuiltin $ case os of
                                         "darwin" -> [ (return True                             ,              from "pbpaste"              , "pbpaste" ) ]
                                         "linux"  -> [ (haveClipboardCmd "wl-paste"             , fmap strip $ from "wl-paste"             , "wl-paste")
                                                     , (haveClipboardCmd "xsel -b -o"           ,              from "xsel -b -o"           , "xsel"    )
                                                     , (haveClipboardCmd "xclip -selection c -o",              from "xclip -selection c -o", "xclip"   )
                                                     ]
                                         _        -> []

    if txt == ""
       then setStatus $ if isJust back
                           then "Warning: System clipboard is empty."
                           else "Warning: Editor clipboard is empty."

       else do
            insertText txt
            setStatus $ maybe "Pasted " (++ ": pasted ") back
                     ++ show (length $ linesPlus txt)
                     ++ " lines ("
                     ++ show (length txt)
                     ++ if isJust back
                           then " chars) from system clipboard."
                           else " chars) from editor clipboard."

    where
        from :: String -> IO String
        from cmd = readCreateProcess (shell cmd) ""

        firstOrBuiltin :: [(IO Bool, IO String, String)] -> WSEdit (Maybe String, String)
        firstOrBuiltin ((t, a, n):xs) = do
            liftIO t >>= \case
                False -> firstOrBuiltin xs
                True  -> fmap (Just n,) $ liftIO a

        firstOrBuiltin []          = liftIO $ do
            h <- getHomeDirectory
            fmap ((Nothing,) . fromMaybe "") $ mayReadFile (h ++ "/.wsedit-clipboard")

        strip :: String -> String
        strip "" = ""
        strip s  = initNote (fqn "paste") s



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
