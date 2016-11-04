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


import Control.Exception           (SomeException, try)
import Control.Monad               (when)
import Control.Monad.IO.Class      (liftIO)
import Control.Monad.RWS.Strict    (ask, get, modify, put)
import Data.Maybe                  (fromMaybe, isJust)
import Graphics.Vty                (Vty (shutdown))
import Safe                        (fromJustNote, headDef)
import System.Directory            ( doesFileExist, getHomeDirectory
                                   , getPermissions
                                   , makeRelativeToCurrentDirectory, removeFile
                                   , renameFile, writable
                                   )
import System.Exit                 (exitFailure)
import System.IO                   ( IOMode (AppendMode, WriteMode)
                                   , NewlineMode
                                   , hPutStr, hSetEncoding, hSetNewlineMode
                                   , mkTextEncoding, universalNewlineMode
                                   , withFile
                                   )
import Text.Show.Pretty            (ppShow)

import WSEdit.Control.Autocomplete (dictAddRec)
import WSEdit.Control.Base         ( alterState, fetchCursor, moveCursor
                                   , refuseOnReadOnly, standby
                                   )
import WSEdit.Data                 ( EdConfig ( encoding, initJMarks
                                              , newlineMode, purgeOnClose
                                              , vtyObj, wriCheck
                                              )
                                   , EdState  ( changed, continue, cursorPos
                                              , detectTabs, dict, edLines
                                              , exitMsg, fname, lastEvent
                                              , loadPos, markPos, overwrite
                                              , readOnly, replaceTabs
                                              )
                                   , WSEdit
                                   , runWSEdit, version
                                   )
import WSEdit.Data.Algorithms      ( chopHist, mapPast, popHist, setStatus
                                   )
import WSEdit.Data.Pretty          (prettyEdConfig)
import WSEdit.Output               (drawExitFrame)
import WSEdit.Util                 ( linesPlus, readEncFile, unlinesPlus
                                   , withFst, withPair
                                   )
import WSEdit.WordTree             (empty)

import qualified WSEdit.Buffer        as B


fqn :: String -> String
fqn = ("WSEdit.Control.Global." ++)





-- | Crashes the editor. Used for debugging. Mapped to Ctrl-Meta-C, test it if
--   you dare.
simulateCrash :: WSEdit ()
simulateCrash = error "Simulated crash."


-- | Shuts down vty gracefully, prints out an error message, creates a
--   (potentially quite sizeable) error dump at "./CRASH-DUMP" and finally exits
--   with return code 1. The optional first parameter can be used to indicate
--   the subsystem where the error occured.
bail :: Maybe String -> String -> WSEdit ()
bail mayComp s = do
    c <- ask
    st <- get

    standby $ unlinesPlus
        [ s
        , ""
        , "Writing state dump to ./CRASH-DUMP ..."
        ]

    liftIO $ writeFile "CRASH-DUMP"
           $ "WSEDIT " ++ version ++ " CRASH LOG\n"
          ++ "Error message: " ++ (headDef "" $ lines s)
                ++ maybe "" (\str -> " (" ++ str ++ ")") mayComp
          ++ "\nLast recorded event: "
                ++ fromMaybe "-" (fmap show $ lastEvent st)
          ++ "\n\nEditor configuration:\n"
          ++ indent (ppShow $ prettyEdConfig c)
          ++ "\n\nEditor state:\n"
          ++ indent ( ppShow
                     $ mapPast (\h -> h { dict = empty })
                     $ fromJustNote (fqn "bail")
                     $ chopHist 10
                     $ Just st
                     )

    drawExitFrame

    liftIO $ do
        shutdown $ vtyObj c
        putStrLn s
        putStrLn "A state dump is located at ./CRASH-DUMP ."

        exitFailure

    where
        indent :: String -> String
        indent = unlinesPlus . map ("    " ++) . linesPlus


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
    standby "Saving..."

    s <- get

    if not (changed s)
       then setStatus "No changes to save."

       else do
            c <- ask
            liftIO $ writeF (fname s ++ ".atomic") (encoding c) (newlineMode c)
                   $ unlinesPlus
                   $ map snd
                   $ B.toList
                   $ edLines s

            b <- if wriCheck c
                    then doWriCheck
                    else return True

            when b $ do
                liftIO $ renameFile (fname s ++ ".atomic") (fname s)

                put s { changed = False }

                setStatus $ "Saved "
                         ++ show (B.length (edLines s))
                         ++ " lines of "
                         ++ fromMaybe "native" (encoding c)
                         ++ " text."

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

        doWriCheck :: WSEdit Bool
        doWriCheck = fmap wriCheck ask >>= \case
            False -> return True
            True  -> do
                s <- get
                put $ s { fname = fname s ++ ".atomic" }
                load False
                s' <- get
                put s
                if (B.toList (edLines s) == B.toList (edLines s'))
                   then return True
                   else do
                        c <- ask
                        liftIO $ runWSEdit ( c { encoding    = Nothing
                                               , newlineMode = universalNewlineMode
                                               }
                                           , s { fname = "CRASH-RESCUE" }
                                           )
                               $ save

                        let m = "The Write-Read identity check failed.\n\n"
                             ++ "Your saved file would have been corrupted and "
                             ++ "got reset to the last save, your changes have "
                             ++ "been dumped to ./CRASH-RESCUE using your "
                             ++ "system's native encoding.\n\n"
                             ++ "This isssue is probably due to the use of a "
                             ++ "non-standard text encoding. Encoding "
                             ++ "irregularities are a known issue, please try "
                             ++ "using a different setting for the time being."

                        modify (\s'' -> s'' { continue = False
                                            , exitMsg  = Just m
                                            }
                               )

                        liftIO $ removeFile (fname s ++ ".atomic")
                        return False



-- | Tries to load the text buffer from the file name in the editor state.
--   Pass 'True' to make it display a loading screen (pun not entirely
--   unintended) and rebuild the dictionary or 'False' to make it a raw load.
load :: Bool -> WSEdit ()
load lS = alterState $ do
    when lS $ standby "Loading..."

    p <- fname <$> get
    when (p == "") $ quitComplain "Will not load an empty filename."

    b <- liftIO $ doesFileExist p
    w <- canWriteFile
    p' <- liftIO $ makeRelativeToCurrentDirectory p

    s <- get
    c <- ask

    (mEnc, txt) <- if b
                      then liftIO $ readEncFile p'
                      else return (Just undefined, "")

    let l = fromMaybe (B.singleton (False, ""))
          $ B.fromList
          $ map (withFst (`elem` initJMarks c))
          $ zip [1..]
          $ linesPlus txt

    put $ s
        { edLines     = B.toFirst l
        , fname       = p'
        , cursorPos   = 1
        , readOnly    = not (isJust mEnc && w && not (readOnly s))
        , replaceTabs = if detectTabs s
                           then '\t' `notElem` txt
                           else replaceTabs s
        }

    when lS $ setStatus
            $ case (b    , w    , mEnc   ) of
                   (True , True , Just e ) -> "Loaded "
                                           ++ show (length $ linesPlus txt)
                                           ++ " lines of "
                                           ++ e
                                           ++ " text."

                   (True , False, Just e ) -> "Warning: "
                                           ++ e
                                           ++ " file not writable, "
                                           ++ "opening in read-only mode ..."

                   (True , _    , Nothing) -> "Warning: unknown character "
                                           ++ "encoding, opening raw..."

                   (False, True , _      ) -> "Warning: file "
                                           ++ p'
                                           ++ " not found, creating on "
                                           ++ "save ..."

                   (False, False, _      ) -> "Warning: cannot create file "
                                           ++ p'
                                           ++ " , check permissions and disk "
                                           ++ "state."

    -- Move the cursor to where it should be placed.
    uncurry moveCursor $ withPair dec dec $ loadPos s

    when lS $ dictAddRec

    where
        dec :: Int -> Int
        dec n = n - 1



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
