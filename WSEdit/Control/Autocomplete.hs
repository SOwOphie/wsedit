module WSEdit.Control.Autocomplete
    ( dictAdd
    , dictAddRec
    , listAutocomplete
    , applyAutocomplete
    , completeOr
    ) where


import Control.Exception        (evaluate)
import Control.Monad            (forM_, unless, when)
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.RWS.Strict (ask, get, modify, put)
import Data.Char                (isSpace)
import Data.List                (intercalate, isPrefixOf, stripPrefix)
import Safe                     (lastDef)
import System.FilePath          (splitPath, (</>))
import System.Directory         ( doesDirectoryExist, doesFileExist
                                , listDirectory
                                )

import WSEdit.Control.Base      (alterBuffer, standby)
import WSEdit.Control.Text      (insertRaw)
import WSEdit.Data              ( WSEdit
                                , EdConfig (lineComment, tabWidth)
                                , EdState ( buildDict, canComplete, cursorPos
                                          , dict, edLines, fname, readOnly
                                          )
                                )
import WSEdit.Data.Algorithms   (canonicalPath, fileMatch, setStatus)
import WSEdit.Util              ( findInStr, getKeywordAtCursor
                                , linesPlus, longestCommonPrefix, readEncFile
                                , unlinesPlus, wordsPlus
                                )
import WSEdit.WordTree          (addWord, complete)

import qualified WSEdit.Buffer as B




-- | Adds a file to the dictionary.
dictAdd :: FilePath -> WSEdit ()
dictAdd f = do
    s <- get
    c <- ask

    fI <- liftIO $ canonicalPath Nothing   f
    tI <- liftIO $ canonicalPath Nothing $ fname s

    let
        depths = map snd
               $ filter ( maybe (fI == tI)
                                (flip fileMatch fI)
                        . fst
                        )
               $ buildDict s

    unless (null depths) $ do
        b   <- liftIO $ doesFileExist f
        txt <- if b
                  then fmap snd $ liftIO $ readEncFile f
                  else return ""

        d <- liftIO
           $ evaluate
           $ foldl (flip addWord) (dict s)
           $ wordsPlus
           $ unlinesPlus
           $ map ( (\l -> take ( minimum
                               $ maxBound
                               : concat [ findInStr x l | x <- lineComment c ]
                               ) l
                   )
                 . dropWhile isSpace
                 )
           $ (case sequence depths of
                  Nothing -> id
                  Just r  -> filter (\l -> or [ atLevel (tabWidth c) x l | x <- r])
             )
           $ linesPlus txt

        {- This is the magic behind the whole function. Ready?
            1. Split the read file into lines
            2. Filter out all the lines not at the specified indentation level.
            3. Filter the lines by their depth.
            4. Glue the list back to a string.
            5. Use a fancy word extraction function from the Util module.
            6. Add everything to the dictionary.
            7. Evaluate (to force strictness and surface any residual errors)
            8. Lift to IO.
        -}

        put $! s { dict = d }

    where
        -- | Checks whether a line is at a specified indentation level. The
        --   first parameter is the number of spaces equal to a tab.
        atLevel :: Int -> Int -> String -> Bool
        atLevel _ _ []        = False
        atLevel 0 _ _         = error "Tab width 0 encountered"
        atLevel _ 0 ( x  :_ ) = not $ isSpace x
        atLevel t n ('\t':xs) = atLevel t (n-1) xs
        atLevel t n s | all (== ' ') $ take t s = atLevel t (n-1) $ drop t s
        atLevel _ _ _         = False



-- | Rebuilds the dictionary.
dictAddRec :: WSEdit ()
dictAddRec = do
    standby $ "Rebuilding dictionary...\n\n\
              \Revisit your -a* settings if this takes too long."

    s <- get

    -- Skip everything if dictionary building is disabled
    unless (null $ buildDict s) $ dictAddRec' "."

    where
        -- | Processes the files inside the given directory.
        dictAddRec' :: FilePath -> WSEdit ()
        dictAddRec' p =
            let
                el = lastDef "" (splitPath p)
            in
                unless ("." `isPrefixOf` el && length el > 1) $ do
                    -- Retrieve the directory contents.
                    l <- liftIO $ listDirectory p

                    forM_ l $ \p' -> do
                        -- Full file name
                        let f = p </> p'

                        isFile <- liftIO $ doesFileExist f
                        if isFile
                           then dictAdd f
                           else do
                                isDir <- liftIO $ doesDirectoryExist f
                                when isDir $ dictAddRec' f



-- | Lists all autocomplete possibilities into the status bar.
listAutocomplete :: WSEdit ()
listAutocomplete = do
    s <- get

    unless (null (buildDict s) || readOnly s)
        $ case getKeywordAtCursor (cursorPos s)
                $ snd
                $ B.pos
                $ edLines s of

               Nothing -> setStatus "..."
               Just  k ->
                let
                    l = complete k
                        $ dict s
                    p = longestCommonPrefix l
                in do
                    setStatus $ k
                            ++ " => "
                            ++ p
                            ++ " => { "
                            ++ intercalate " | " l
                            ++ " }"

                    modify (\s' -> s' { canComplete = p /= "" })



-- | Inserts the longest common prefix of all autocomplete suggestions.
applyAutocomplete :: WSEdit ()
applyAutocomplete = do
    s <- get

    when (not (null $ buildDict s) && canComplete s)
        $ case getKeywordAtCursor (cursorPos s)
                $ snd
                $ B.pos
                $ edLines s of

               Nothing -> return ()
               Just  k ->
                let
                    w = longestCommonPrefix
                        $ complete k
                        $ dict s
                in
                    case stripPrefix k w of
                         Nothing -> setStatus "X"
                         Just  i -> alterBuffer
                                  $ insertRaw i



-- | Tries to autocomplete the current word, or executes another action
--   if impossible.
completeOr :: WSEdit () -> WSEdit ()
completeOr a = do
    b <- canComplete <$> get
    if b
       then applyAutocomplete
       else a
