{-# LANGUAGE LambdaCase #-}

module WSEdit where


import Control.Exception        (SomeException, try)
import Control.Monad            (when)
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.RWS.Strict (ask, get, modify, runRWST)
import Data.Default             (def)
import Data.Maybe               (catMaybes)
import Graphics.Vty             ( Event (EvKey, EvResize)
                                , Key (KChar)
                                , mkVty, nextEvent, shutdown
                                )
import System.IO                (mkTextEncoding)

import WSEdit.Arguments         (parseArguments)
import WSEdit.Control           ( bail, deleteSelection, insert
                                , listAutocomplete, load, quitComplain, save
                                , standby
                                )
import WSEdit.Data              ( EdConfig ( dumpEvents, encoding, keymap
                                           , vtyObj
                                           )
                                , EdState ( changed, continue, exitMsg, fname
                                          , lastEvent, status
                                          )
                                , WSEdit
                                , mkDefConfig, runWSEdit
                                )
import WSEdit.Data.Algorithms   (catchEditor, setStatus)
import WSEdit.Keymaps           (defaultKM)
import WSEdit.Renderer          (rebuildAll)
import WSEdit.Output            (draw, drawExitFrame)



fqn :: String -> String
fqn = ("WSEdit." ++)





-- | Main function. Reads in the parameters, then passes control to the main
--   loop.
start :: IO ()
start = do
    -- initialize vty
    v <- mkVty def

    -- create the configuration object
    let conf = mkDefConfig v defaultKM

    (conf', st) <- parseArguments (conf, def)

    case encoding conf' of
         Nothing -> return ()
         Just  e -> try (mkTextEncoding e) >>= \case
                Right _ -> return ()
                Left ex -> do
                    const (return ()) (ex :: SomeException)
                    runWSEdit (conf', st) $ quitComplain
                                          $ "-e: Encoding not available: " ++ e

    (_, st', _) <- runRWST (exec True) conf' st

    -- Shutdown vty
    shutdown v

    case exitMsg st' of
         Nothing -> return ()
         Just  m -> putStrLn m

    where
        -- | Possibly loads the file specified in `fname`, then runs the editor.
        exec :: Bool -> WSEdit ()
        exec b = do
            when b $ catchEditor (load True) $ \e ->
                quitComplain $ "An I/O error occured while loading:\n\n"
                            ++ show e

            standby "Building initial rendering cache..."
            rebuildAll Nothing

            mainLoop
            drawExitFrame





-- | Main editor loop. Runs once per input event processed.
mainLoop :: WSEdit ()
mainLoop = do
    c <- ask
    s <- get

    ev <- flip catchEditor (errHdlDraw >> return undefined) $ do
        draw
        setStatus ""

        ev <- liftIO $ nextEvent $ vtyObj c
        modify (\st -> st { lastEvent = Just ev })
        return ev

    flip catchEditor errHdlControl $ do
        -- look up the event in the keymap
        -- if not found: insert the pressed key
        -- if it's not alphanumeric: show an "event not bound" warning
        maybe (case ev of
                    EvKey (KChar k) [] -> deleteSelection
                                       >> insert k
                                       >> listAutocomplete

                    EvResize _ _       -> return ()
                    _                  -> setStatus $ "Event not bound: "
                                                   ++ show ev
              )
              fst
              $ lookup ev
              $ catMaybes
              $ keymap c

    flip catchEditor errHdlRenderer $ do
        rebuildAll $ Just s

        when (dumpEvents c) $ do
            st <- get
            setStatus $ show ev ++ status st

    b <- continue <$> get
    when b mainLoop

    where
        errHdlDraw     = errHdl "Draw call"
        errHdlControl  = errHdl "Control"
        errHdlRenderer = errHdl "Renderer"

        errHdl :: String -> SomeException -> WSEdit ()
        errHdl comp e = do
            b <- changed <$> get
            if b
               then do
                    modify (\s -> s { fname = "CRASH-RESCUE" })
                    standby $ "An error occured: " ++ show e
                           ++ "\n\n"
                           ++ "Dumping unsaved changes..."
                    save
                    bail (Just comp) $ "An error occured: " ++ show e
                                    ++ "\n\n"
                                    ++ "All unsaved changes have been dumped to"
                                    ++ " ./CRASH-RESCUE ."

               else bail (Just comp) $ "An error occured: " ++ show e
