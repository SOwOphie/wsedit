{-# LANGUAGE LambdaCase #-}

module WSEdit.Control.Global
    ( simulateCrash
    , bail
    , quitComplain
    , quit
    , forceQuit
    , canWriteFile
    , save
    , load
    , toggleTabRepl
    , toggleInsOvr
    , toggleReadOnly
    , undo
    ) where


import Codec.Text.Detect           (detectEncoding)
import Control.Exception           (SomeException, try)
import Control.Monad               (when)
import Control.Monad.IO.Class      (liftIO)
import Control.Monad.RWS.Strict    (ask, get, modify, put)
import Data.Maybe                  (fromMaybe)
import Graphics.Vty                (Vty (shutdown))
import Safe                        (fromJustNote)
import System.Directory            ( doesFileExist, getHomeDirectory
                                   , getPermissions
                                   , makeRelativeToCurrentDirectory, removeFile
                                   , writable
                                   )
import System.Exit                 (exitFailure)
import System.IO                   ( IOMode (AppendMode, ReadMode, WriteMode)
                                   , NewlineMode
                                   , hPutStr, hSetEncoding, hSetNewlineMode
                                   , mkTextEncoding, withFile
                                   )
import System.IO.Strict            (hGetContents)
import Text.Show.Pretty            (ppShow)

import WSEdit.Control.Autocomplete (dictAddRec)
import WSEdit.Control.Base         ( alterState, fetchCursor, moveCursor
                                   , refuseOnReadOnly
                                   )
import WSEdit.Data                 ( EdConfig ( encoding, newlineMode
                                              , purgeOnClose, vtyObj
                                              )
                                   , EdState  ( changed, continue, cursorPos
                                              , detectTabs, dict, edLines, fname
                                              , loadPos, markPos, overwrite
                                              , readOnly, replaceTabs
                                              )
                                   , WSEdit
                                   , chopHist, mapPast, popHist, setStatus
                                   , version
                                   )
import WSEdit.Data.Pretty          (prettyEdConfig)
import WSEdit.Output               (drawExitFrame)
import WSEdit.Util                 (withPair)
import WSEdit.WordTree             (empty)

import qualified Data.ByteString.Lazy as S
import qualified WSEdit.Buffer        as B


fqn :: String -> String
fqn = ("WSEdit.Control.Global." ++)





-- | Crashes the editor. Used for debugging. Mapped to Ctrl-Meta-C, test it if
--   you dare.
simulateCrash :: WSEdit ()
simulateCrash = error "Simulated crash."


-- | Shuts down vty gracefully, prints out an error message, creates a
--   (potentially quite sizeable) error dump at "./CRASH-DUMP" and finally exits
--   with return code 1.
bail :: String -> WSEdit ()
bail s = do
    c <- ask
    st <- get

    drawExitFrame

    liftIO $ do
        shutdown $ vtyObj c
        putStrLn s
        putStrLn "Writing state dump to ./CRASH-DUMP ..."
        writeFile "CRASH-DUMP"
            $ "WSEDIT " ++ version ++ " CRASH LOG\n"
           ++ "Error message: " ++ s
           ++ "\n\nEditor configuration:\n"
           ++ indent (ppShow $ prettyEdConfig c)
           ++ "\nEditor state:\n"
           ++ indent ( ppShow
                     $ mapPast (\h -> h { dict = empty })
                     $ fromJustNote (fqn "bail")
                     $ chopHist 10
                     $ Just st
                     )

        exitFailure

    where
        indent :: String -> String
        indent = unlines . map ("    " ++) . lines


-- | Similar to 'bail', but does not generate a state dump.
quitComplain :: String -> WSEdit ()
quitComplain s = do
    v <- vtyObj <$> ask

    drawExitFrame

    liftIO $ do
        shutdown v
        putStrLn s
        exitFailure


-- | Checks for unsaved changes, then either complains via 'setStatus' or calls
--   'forceQuit'.
quit :: WSEdit ()
quit = do
    b <- changed <$> get
    if b
       then setStatus "Unsaved changes: Ctrl-S to save, Ctrl-Meta-Q to ignore."
       else forceQuit


-- | Tells the main loop to exit gracefully.
forceQuit :: WSEdit ()
forceQuit = do
    b1 <- purgeOnClose <$> ask
    when b1 $ liftIO $ do
        h <- getHomeDirectory
        let cpath = h ++ "/.wsedit-clipboard"

        b2 <- doesFileExist cpath
        when b2 $ removeFile cpath

    modify (\s -> s { continue = False })



-- | Returns whether or not the current file is writable.
canWriteFile :: WSEdit Bool
canWriteFile = do
    f <- fname <$> get

    liftIO $ do
        b <- doesFileExist f

        if b
           then writable <$> getPermissions f
           else try (do
                        withFile f AppendMode $ const $ return ()
                        removeFile f
                    ) >>= \case
                        Right _ -> return True
                        Left  e -> const (return False) (e :: SomeException)

    -- I am aware of the fact that this code is pretty awful, but it has yet to
    -- fail me and/or break anything.  I use it as my daily driver to edit all
    -- kinds of mission-critical system files, but do heed lines 33-38 of the
    -- license file anyways.



-- | Saves the text buffer to the file name in the editor state.
save :: WSEdit ()
save = refuseOnReadOnly $ do
    s <- get

    if not (changed s)
       then setStatus "No changes to save."

       else do
            c <- ask
            liftIO $ writeF (fname s) (encoding c) (newlineMode c)
                   $ unlines
                   $ map snd
                   $ B.toList
                   $ edLines s

            put s { changed = False }

            setStatus $ "Saved "
                     ++ show (B.length (edLines s))
                     ++ " lines of text."

    dictAddRec

    where
        writeF :: FilePath -> Maybe String -> NewlineMode -> String -> IO ()
        writeF path mayEnc nl cont = withFile path WriteMode $ \h -> do
            case mayEnc of
                 Nothing  -> return ()
                 Just enc -> do
                    e <- mkTextEncoding enc
                    hSetEncoding h e

            hSetNewlineMode h nl
            hPutStr h cont



-- | Tries to load the text buffer from the file name in the editor state.
load :: WSEdit ()
load = alterState $ do
    p <- fname <$> get
    when (p == "") $ quitComplain "Will not load an empty filename."

    b <- liftIO $ doesFileExist p
    w <- canWriteFile
    p' <- liftIO $ makeRelativeToCurrentDirectory p

    s <- get

    (if b
        then liftIO $ readF p'
        else return $ Just "") >>= \case
        Nothing  -> quitComplain "File read error: unknown file encoding."
        Just txt -> do
            let l = fromMaybe (B.singleton (False, ""))
                  $ B.fromList
                  $ zip (repeat False)
                  $ map (filter (/= '\r'))
                  $ lines txt

            put $ s
                { edLines     = B.toFirst l
                , fname       = p'
                , cursorPos   = 1
                , readOnly    = not w || readOnly s
                , replaceTabs = if detectTabs s
                                   then '\t' `notElem` txt
                                   else replaceTabs s
                }

            setStatus $ case (b    , w    ) of
                             (True , True ) -> "Loaded "
                                            ++ show (length $ lines txt)
                                            ++ " lines of text."

                             (True , False) -> "Warning: file not writable, "
                                            ++ "opening in read-only mode ..."

                             (False, True ) -> "Warning: file "
                                            ++ p'
                                            ++ " not found, creating on "
                                            ++ "save ..."

                             (False, False) -> "Warning: cannot create file "
                                            ++ p'
                                            ++ " , check permissions and disk "
                                            ++ "state."

    -- Move the cursor to where it should be placed.
    uncurry moveCursor $ withPair dec dec $ loadPos s

    dictAddRec

    where
        dec :: Int -> Int
        dec n = n - 1

        readF :: FilePath -> IO (Maybe String)
        readF f = S.readFile f >>= detectEncoding >>= \case
            Nothing -> return Nothing
            Just  e -> withFile f ReadMode $ \h -> do
                hSetEncoding h e
                Just <$> hGetContents h



-- | Toggle the replacement of tabs with spaces.
toggleTabRepl :: WSEdit ()
toggleTabRepl = do
    s <- get
    put $ s { replaceTabs = not $ replaceTabs s }



-- | Toggle insert / overwrite mode
toggleInsOvr :: WSEdit ()
toggleInsOvr = modify (\s -> s { overwrite = not $ overwrite s })



-- | Toggle read-only mode.
toggleReadOnly :: WSEdit ()
toggleReadOnly = alterState $ do
    s <- get
    if readOnly s
       then do
            b <- canWriteFile
            if not b
               then setStatus "Error: file is read-only."
               else do
                    put $ s { readOnly = False }
                    fetchCursor

       else put $ s { readOnly  = True
                    , cursorPos = 1
                    , markPos   = Nothing
                    , edLines   = B.toFirst $ edLines s
                    }

            -- This puts the cursor to (1, 1), where it gets hidden by the
            -- output functions.



-- | Undo the last action as logged by 'alterBuffer'.
undo :: WSEdit ()
undo = refuseOnReadOnly
     $ alterState
     $ popHist >> moveCursor 0 0
