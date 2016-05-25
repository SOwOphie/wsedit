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
import Control.Monad.RWS.Strict (get, modify, put)
import Data.Char                (isSpace)
import Data.Maybe               (fromJust, isJust)
import Data.List                (intercalate, isSuffixOf, stripPrefix)
import Safe                     (headDef)
import System.Directory         ( doesDirectoryExist, doesFileExist
                                , getCurrentDirectory, getDirectoryContents
                                )

import WSEdit.Control.Base      (alterBuffer)
import WSEdit.Control.Text      (insertRaw)
import WSEdit.Data              ( WSEdit
                                , EdState (buildDict, canComplete, cursorPos
                                          , dict, edLines, fname, readOnly
                                          , tabWidth
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
        t <- tabWidth <$> get
        txt <- liftIO $ readFile f

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

    case buildDict s of
         Nothing -> return ()
         Just l  -> do
            p <- liftIO getCurrentDirectory
            dictAddRec' l p

    where
        dictAddRec' :: Int -> FilePath -> WSEdit ()
        dictAddRec' n p = do
            ext <- getExt . fname <$> get

            l <- liftIO
               $ filter (\s -> headDef '.' s /= '.')
              <$> getDirectoryContents p

            forM_ l $ \p' -> do
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
                $ fromJust
                $ B.left
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
                $ fromJust
                $ B.left
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
