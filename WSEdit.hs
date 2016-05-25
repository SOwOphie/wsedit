{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Exception        (SomeException)
import Control.Monad            (when)
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.RWS.Strict (ask, get, modify, runRWST)
import Data.Default             (def)
import Data.List                (isPrefixOf, partition, stripPrefix)
import Data.Maybe               (fromMaybe)
import Graphics.Vty             ( Event (EvKey)
                                , Key (KChar)
                                , mkVty
                                , nextEvent
                                , shutdown
                                )
import Safe                     (headMay)
import System.Directory         (getHomeDirectory)
import System.Environment       (getArgs)

import WSEdit.Control           ( bail, deleteSelection, insert
                                , listAutocomplete, load, quitComplain, save
                                )
import WSEdit.Data              ( EdConfig ( EdConfig, edDesign, histSize
                                           , keymap, vtyObj
                                           )
                                , EdState ( buildDict, changed, continue, drawBg
                                          , fname, readOnly, replaceTabs
                                          , tabWidth
                                          )
                                , WSEdit
                                , catchEditor, setStatus
                                )
import WSEdit.Keymaps           (defaultKM)
import WSEdit.Output            (draw)
import WSEdit.Util              (getExt, mayReadFile)



mainLoop :: WSEdit ()
mainLoop = do
    draw

    setStatus ""

    c <- ask
    ev <- liftIO $ nextEvent $ vtyObj c


    fromMaybe (case ev of
                    EvKey (KChar k) [] -> deleteSelection
                                       >> insert k
                                       >> listAutocomplete

                    _                  -> setStatus $ "Event not bound: "
                                                   ++ show ev
              )
        $ lookup ev
        $ keymap c

    b <- continue <$> get
    when b mainLoop





main :: IO ()
main = do
    (sw, args) <- partition (isPrefixOf "-") <$> getArgs
    v <- mkVty def

    let conf = EdConfig
            { vtyObj   = v
            , edDesign = def
            , keymap   = defaultKM
            , histSize = 100
            }
        filename = headMay args

    h <- liftIO $ getHomeDirectory
    glob <- fromMaybe "" <$> mayReadFile (h ++ "/.config/wsedit.conf")
    loc  <- fromMaybe "" <$> mayReadFile "./.wsedit"

    _ <- runRWST (argLoop $ filterFileArgs (getExt <$> filename) glob
                         ++ filterFileArgs (getExt <$> filename) loc
                         ++ sw
                 ) conf $ def { fname = fromMaybe "" filename }
    shutdown v

    where
        filterFileArgs :: Maybe String -> String -> [String]
        filterFileArgs Nothing    s = concatMap words $ filter (notElem ':') $ lines s
        filterFileArgs (Just ext) s =
            let
                (loc, gl) = partition (elem ':') $ lines s
            in
                concatMap words
              $ gl
             ++ ( filter (/= "")
                $ map (fromMaybe "" . stripPrefix (ext ++ ":"))
                  loc
                )

        argLoop :: [String] -> WSEdit ()
        argLoop (('-':'b'    :x ):xs) = do
            modify (\s -> s { drawBg = False })
            argLoop (('-':x):xs)

        argLoop (('-':'B'    :x ):xs) = do
            modify (\s -> s { drawBg = True })
            argLoop (('-':x):xs)

        argLoop (('-':'c':'g':x ):xs) = do
            h <- liftIO getHomeDirectory
            modify (\s -> s { fname = h ++ "/.config/wsedit.conf" })
            argLoop (('-':x):xs)

        argLoop (('-':'c':'l':x ):xs) = do
            modify (\s -> s { fname = "./.wsedit" })
            argLoop (('-':x):xs)

        argLoop (('-':'d': c :x ):xs) = do
            modify (\s -> s { buildDict = Just $ read [c] })
            argLoop (('-':x):xs)

        argLoop (('-':'D'    :x ):xs) = do
            modify (\s -> s { buildDict = Nothing })
            argLoop (('-':x):xs)

        argLoop (('-':'r'    :x ):xs) = do
            modify (\s -> s { readOnly = True })
            argLoop (('-':x):xs)

        argLoop (('-':'i': c :x ):xs) = do
            modify (\s -> s { tabWidth = read [c] })
            argLoop (('-':x):xs)

        argLoop (('-':'R'    :x ):xs) = do
            modify (\s -> s { readOnly = False })
            argLoop (('-':x):xs)

        argLoop (('-':'t'    :x ):xs) = do
            modify (\s -> s { replaceTabs = True })
            argLoop (('-':x):xs)

        argLoop (('-':'T'    :x ):xs) = do
            modify (\s -> s { replaceTabs = False })
            argLoop (('-':x):xs)

        argLoop (['-']           :xs) =
            argLoop xs

        argLoop (('-': x     :_ ):_ ) =
            usage $ "Unknown argument: " ++ [x]

        argLoop (x               :_ ) =
            usage $ "Unexpected argument: " ++ x

        argLoop []                    = do
            f <- fname <$> get
            if f == ""
               then usage "No file specified."
               else do
                    load
                    catchEditor mainLoop errHdl


        errHdl :: SomeException -> WSEdit ()
        errHdl e = do
            b <- changed <$> get
            if b
               then do
                    modify (\s -> s { fname = "CRASH-RESCUE" })
                    save
                    bail $ "An error occured: " ++ show e
                        ++ "\n\n"
                        ++ "Your unsaved work has been rescued to ./CRASH-RESCUE ."
               else bail $ "An error occured: " ++ show e


        usage :: String -> WSEdit ()
        usage s = quitComplain
                $ s ++ "\n"
               ++ "\n"
               ++ "Usage: wsedit [<arguments>] [filename]\n"
               ++ "\n"
               ++ "Arguments (the uppercase options are on by default):\n"
               ++ "\n"
               ++ "\t-b\tDon't draw the background (dots + lines). May speed up\n"
               ++ "\t\tthe editor on older systems, as it seems to be quite the\n"
               ++ "\t\tresource hog.\n"
               ++ "\t-B\tDraw the usual background (dots + lines).\n"
               ++ "\n"
               ++ "\n"
               ++ "\n"
               ++ "\t-cg\tOpen global configuration file (~/.config/wsedit.conf).\n"
               ++ "\t-cl\tOpen local configuration file (./.wsedit).\n"
               ++ "\n"
               ++ "\t\tThose files will be concatenated with the command line\n"
               ++ "\t\targuments and then evaluated, so that flags in the global\n"
               ++ "\t\tconfig are overridden by those in the local config, which\n"
               ++ "\t\tare then overridden by command line arguments.  You can\n"
               ++ "\t\tprefix lines with \"<ext>:\" so that they are only read\n"
               ++ "\t\tfor files with extension .<ext> , e.g.\n"
               ++ "\n"
               ++ "\t\ths: -i4 -t\n"
               ++ "\n"
               ++ "\n"
               ++ "\n"
               ++ "\t-d<n>\tEnable dictionary building at indentation depth n.\n"
               ++ "\t-D\tDisable dictionary building.\n"
               ++ "\n"
               ++ "\t\tWith dictionary building enabled, wsed will scan all files\n"
               ++ "\t\tand directories under the current working directory,\n"
               ++ "\t\tskipping hidden ones (starting with a dot).  Every file\n"
               ++ "\t\twith the same file ending as the opened file will be read,\n"
               ++ "\t\tand a dictionary will be built from all words from lines\n"
               ++ "\t\tat depth n (either n tabs or n*tabWidth spaces).  This \n"
               ++ "\t\tdictionary will then be used to feed the autocomplete\n"
               ++ "\t\tfunction.  The scan will take place everytime you safe or\n"
               ++ "\t\tload.\n"
               ++ "\t\tSETTING THIS GLOBALLY WILL MAKE YOUR EDITOR TAKE AGES TO\n"
               ++ "\t\tSTART UP, E.G. WHEN RUNNING FROM THE HOME DIRECTORY!\n"
               ++ "\n"
               ++ "\n"
               ++ "\n"
               ++ "\t-i<n>\tSet indentation width to n (default = -i4).\n"
               ++ "\n"
               ++ "\n"
               ++ "\n"
               ++ "\t-t\tInsert the appropriate amount of spaces instead of tabs.\n"
               ++ "\t-T\tInsert a tab character when pressing tab.\n"
               ++ "\n"
               ++ "\t\tPressing Ctrl-Meta-Tab in the editor will also toggle this.\n"
               ++ "\n"
               ++ "\n"
               ++ "\n"
               ++ "\t-r\tOpen file in read-only mode.\n"
               ++ "\t-R\tOpen file in read-write mode.\n"
               ++ "\n"
               ++ "\t\tPressing Ctrl-Meta-R in the editor will also toggle this.\n"
