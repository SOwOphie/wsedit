module WSEdit.Control.Autocomplete
    ( dictAdd
    , dictAddRec
    , listAutocomplete
    , applyAutocomplete
    , completeOr
    ) where


import Control.Exception        (evaluate)
import Control.Monad            (forM_, when)
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.RWS.Strict (ask, get, modify, put)
import Data.Char                (isSpace)
import Data.Maybe               (isJust)
import Data.List                (intercalate, isSuffixOf, stripPrefix)
import Safe                     (headDef)
import System.Directory         ( doesDirectoryExist, doesFileExist
                                , getCurrentDirectory, getDirectoryContents
                                )

import WSEdit.Control.Base      (alterBuffer)
import WSEdit.Control.Text      (insertRaw)
import WSEdit.Data              ( WSEdit
                                , EdConfig (tabWidth)
                                , EdState ( buildDict, canComplete, cursorPos
                                          , dict, edLines, fname, readOnly
                                          )
                                , setStatus
                                )
import WSEdit.Util              (getExt, getKeywordAtCursor, isIdentifierChar
                                , longestCommonPrefix, wordsPlus
                                )
import WSEdit.WordTree          (addWord, complete)

import qualified WSEdit.Buffer as B




-- | Adds a file to the dictionary, parsing all lines at the given indentation
--   depth.
dictAdd :: Int -> FilePath -> WSEdit ()
dictAdd l f = do
    s <- get

    when (isJust $ buildDict s) $ do
        t <- tabWidth <$> ask
        txt <- liftIO $ readFile f

        {- This is the magic behind the whole function. Ready?
            1. Split the read file into lines
            2. Filter out all the lines not at the specified indentation level.
            3. Cut all leading whitespace.
            4. Filter out all the lines not beginning with an alphanumeric char.
            5. Glue the list back to a string.
            6. Use a fancy word extraction function from the Util module.
            7. Add everything to the dictionary.
            8. Evaluate (to force strictness and surface any residual errors)
            9. Lift to IO.
        -}

        d <- liftIO
           $ evaluate
           $ foldl (flip addWord) (dict s)
           $ wordsPlus
           $ unlines
           $ filter (isIdentifierChar . headDef '.')
           $ map (dropWhile isSpace)
           $ filter (atLevel t l)
           $ lines txt

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



-- | Adds all files with the right extension in the current working directory
--   to the dictionary.
dictAddRec :: WSEdit ()
dictAddRec = do
    s <- get

    -- Skip everything if dictionary building is disabled
    case buildDict s of
         Nothing -> return ()
         Just l  -> do
            p <- liftIO getCurrentDirectory
            dictAddRec' l p

    where
        -- | Processes the files inside the given directory. First parameter is
        --   the indentation depth to search at.
        dictAddRec' :: Int -> FilePath -> WSEdit ()
        dictAddRec' n p = do
            -- Get the extension of the current file.
            ext <- getExt . fname <$> get

            -- Retrieve the directory contents, filtering out hidden files.
            l <- liftIO
               $ filter (\s -> headDef '.' s /= '.')
              <$> getDirectoryContents p

            forM_ l $ \p' -> do
                -- Full file name
                let f = p ++ "/" ++ p'

                isFile <- liftIO $ doesFileExist f
                if isFile && ext `isSuffixOf` f
                   then dictAdd n f
                   else do
                        isDir <- liftIO $ doesDirectoryExist f
                        when isDir $ dictAddRec' n f



-- | Lists all autocomplete possibilities into the status bar.
listAutocomplete :: WSEdit ()
listAutocomplete = do
    s <- get

    when (isJust (buildDict s) && not (readOnly s))
        $ case getKeywordAtCursor (cursorPos s)
                $ snd
                $ B.curr
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

    when (isJust (buildDict s) && canComplete s)
        $ case getKeywordAtCursor (cursorPos s)
                $ snd
                $ B.curr
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
