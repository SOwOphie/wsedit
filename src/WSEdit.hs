{-# LANGUAGE LambdaCase #-}

module WSEdit where


import Control.Exception
    ( SomeException
    , try
    )
import Control.Monad
    ( when
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.RWS.Strict
    ( ask
    , get
    , modify
    , runRWST
    )
import Data.Default
    ( def
    )
import Data.Maybe
    ( catMaybes
    )
import Graphics.Vty
    ( Event
        ( EvKey
        , EvResize
        )
    , Key
        ( KChar
        )
    , defaultConfig
    , mkVty
    , nextEvent
    , shutdown
    )
import System.IO
    ( mkTextEncoding
    )

import WSEdit.Arguments
    ( parseArguments
    )
import WSEdit.Control.Autocomplete
    ( calcAutocomplete
    , clearAutocomplete
    )
import WSEdit.Control.Base
    ( standby
    )
import WSEdit.Control.Global
    ( bail
    , emergencySave
    , load
    , quitComplain
    )
import WSEdit.Control.Selection
    ( deleteSelection
    )
import WSEdit.Control.Text
    ( insert
    )
import WSEdit.Data
    ( EdConfig
        ( dumpEvents
        , encoding
        , keymap
        , vtyObj
        )
    , EdState
        ( changed
        , continue
        , exitMsg
        , lastEvent
        , status
        )
    , WSEdit
    , mkDefConfig
    , runWSEdit
    )
import WSEdit.Data.Algorithms
    ( catchEditor
    , setStatus
    )
import WSEdit.ElasticTabstops
    ( rebuildTabCache
    )
import WSEdit.Keymaps
    ( defaultKM
    )
import WSEdit.Renderer
    ( rebuildAll
    )
import WSEdit.Output
    ( draw
    , drawExitFrame
    )



fqn :: String -> String
fqn = ("WSEdit." ++)





-- | Main function. Reads in the parameters, then passes control to the main
--   loop.
start :: IO ()
start = do
    -- create the configuration object
    let conf = mkDefConfig undefined defaultKM

    ((conf', st), b) <- parseArguments (conf, def)

    case encoding conf' of
         Nothing -> return ()
         Just  e -> try (mkTextEncoding e) >>= \case
                Right _ -> return ()
                Left ex -> do
                    const (return ()) (ex :: SomeException)
                    runWSEdit (conf', st) $ quitComplain
                                          $ "-fe: Encoding not available: " ++ e

    -- initialize vty
    v <- mkVty defaultConfig

    -- call main loop
    (_, st', _) <- runRWST (exec b) conf' { vtyObj = v } st

    -- Shutdown vty
    shutdown v

    case exitMsg st' of
         Nothing -> return ()
         Just  m -> putStrLn m

    where
        -- | Possibly loads the file specified in `fname`, then runs the editor.
        exec :: Bool -> WSEdit ()
        exec b = do
            if b
               then catchEditor (load True)
                    (\e -> quitComplain ( "An I/O error occured while loading:\n\n"
                                       ++ show e
                                        )
                        >> return False
                    )
                  >>= flip when (do
                                    standby "Building initial rendering cache..."
                                    rebuildAll Nothing
                                    rebuildTabCache
                                    mainLoop
                                )
               else mainLoop

            drawExitFrame





-- | Main editor loop. Runs once per input event processed.
mainLoop :: WSEdit ()
mainLoop = do
    c <- ask
    s <- get

    ev <- flip catchEditor (\e -> errHdlDraw e
                               >> return (EvResize undefined undefined)
                           ) $ do
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
                                       >> calcAutocomplete

                    EvResize _ _       -> setStatus $ status s
                    _                  -> setStatus $ "Event not bound: "
                                                   ++ show ev
              )
              ((>> clearAutocomplete) . fst)
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

                    flip catchEditor (const $ return ())
                        $ standby
                        $ "An error occured: " ++ show e ++ "\n\n"
                       ++ "Dumping unsaved changes..."

                    emergencySave

                    bail (Just comp) $ "An error occured: " ++ show e
                                    ++ "\n\n"
                                    ++ "All unsaved changes have been dumped to"
                                    ++ " $HOME/CRASH-RESCUE ."

               else bail (Just comp) $ "An error occured: " ++ show e
